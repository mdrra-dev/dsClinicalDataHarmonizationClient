#' @title Generic exploratory + inferential analysis of one clinical label
#' @description Fully generic: takes a \code{label} column name and works
#'   identically for any label (\code{"IP_Score"}, \code{"EP_Score"},
#'   \code{"D2T"}, or anything else) -- no label-specific logic anywhere in
#'   this function. Classifies the label's type from the data itself
#'   (continuous / binary / ordinal, via \code{label_type_infoDS}), then runs
#'   whichever of the following are appropriate, saving figures/CSVs under
#'   \code{<output_dir>/EDA_<label>/} (existing files are never deleted):
#'   distribution (+frequency table if discrete), Spearman correlations vs.
#'   clinical features (BH-FDR corrected), Kruskal-Wallis across label levels
#'   (if the label has a manageable number of levels) or Wilcoxon (if
#'   exactly binary), a multivariable model (lm/glm/polr chosen by label
#'   type), an exploratory random forest importance ranking, and a focused
#'   Spearman correlation matrix (label + features).
#'
#'   EVERY statistic returned is aggregate/disclosure-safe (see the
#'   individual DS functions this calls); complete-case N is reported for
#'   every analysis. Where multiple sites are involved, correlations and
#'   model coefficients are pooled across sites via standard fixed-effect
#'   meta-analysis (Fisher-z for correlations, inverse-variance for model
#'   coefficients -- the same family of technique \code{ds.glmSLMA} uses
#'   elsewhere in DataSHIELD); rank-based tests (Kruskal-Wallis, Wilcoxon)
#'   are reported PER SITE rather than pooled, since a statistically sound
#'   pooling method for those would need more machinery than is justified
#'   here -- flagged, not silently approximated.
#'
#' @param label Character, the column name to analyze (e.g. "IP_Score").
#' @param data_object Character, the server-side (already cleaned) data
#'   frame. Default \code{"Draw_cleaned"}.
#' @param clinical_cols Character vector of candidate feature columns.
#'   \code{NULL} (default): the package's canonical variable dictionary,
#'   minus the label and \code{pat_id_col}, intersected with what's actually
#'   present and numeric.
#' @param pat_id_col Patient identifier column (excluded from features).
#' @param output_dir Base directory; results go under
#'   \code{file.path(output_dir, paste0("EDA_", label))}.
#' @param num_bins Histogram bins for the label's distribution. Default 20.
#' @param nfilter Disclosure/stability floor used throughout.
#' @param max_categories_shown,max_levels_shown Distinct-value thresholds --
#'   see \code{cdh_categorical_summary}/\code{label_type_infoDS}.
#' @param kruskal_max_levels Above this many label levels, skip the
#'   per-level boxplot/Kruskal-Wallis step (per the spec, to avoid hundreds
#'   of group plots) and rely on the continuous Spearman analysis instead.
#' @param standardize Logical; standardize numeric predictors in the
#'   multivariable model. Default \code{TRUE}.
#' @param run_random_forest Logical; attempt the exploratory random forest
#'   step. Default \code{TRUE} (silently skipped, with a message, if the
#'   \code{randomForest} package isn't installed server-side).
#' @param rf_ntree Number of trees. Default 500.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return \code{NULL} (with an explanatory message) if \code{label} isn't
#'   available anywhere, or too sparse to analyze. Otherwise a list with
#'   \code{label}, \code{type_info}, \code{clinical_cols}, \code{distribution},
#'   \code{frequency_table}, \code{spearman}, \code{kruskal}, \code{wilcoxon},
#'   \code{model}, \code{random_forest}, \code{correlation},
#'   \code{summary_table} (the section-22 long-format table), \code{fig_dir}.
#' @export
run_clinical_label_eda <- function(label,
                                    data_object = "Draw_cleaned",
                                    clinical_cols = NULL,
                                    pat_id_col = "pat_ID",
                                    output_dir = ".",
                                    num_bins = 20,
                                    nfilter = 5,
                                    max_categories_shown = 20,
                                    max_levels_shown = 20,
                                    kruskal_max_levels = 10,
                                    standardize = TRUE,
                                    run_random_forest = TRUE,
                                    rf_ntree = 500,
                                    datasources = NULL) {

  if (is.null(datasources)) datasources <- datashield.connections_find()

  # ---- 21: validate label existence -------------------------------------
  all_cols <- DSI::datashield.aggregate(conns = datasources,
    expr = call("list_columnsDS", as.symbol(data_object)))
  present_sites <- names(all_cols)[vapply(all_cols, function(cn) label %in% cn, logical(1))]
  if (length(present_sites) == 0) {
    message("Requested label '", label, "' is not available in ", data_object, ".")
    message("Available analysis labels: ", paste(sort(unique(unlist(all_cols))), collapse = ", "))
    return(NULL)
  }
  ds_active <- datasources[present_sites]
  if (length(present_sites) < length(datasources)) {
    message("Note: '", label, "' is only available at: ", paste(present_sites, collapse = ", "),
            " -- proceeding using those sites only.")
  }

  # ---- 4: classify label type from the data ------------------------------
  type_per_site <- DSI::datashield.aggregate(conns = ds_active,
    expr = call("label_type_infoDS", as.symbol(data_object), label, nfilter, max_levels_shown))
  type_info <- .cdh_combine_type_info(type_per_site, max_levels_shown)
  if (is.null(type_info)) {
    message("Label '", label, "' has fewer than nfilter (", nfilter,
            ") non-missing observations across available sites -- cannot analyze.")
    return(NULL)
  }
  n_unique_display <- if (is.na(type_info$n_unique)) paste0("many (>", max_levels_shown, ")") else type_info$n_unique
  message("Label '", label, "': n=", type_info$n_nonmissing,
          ", n_unique=", n_unique_display,
          ", range=[", round(type_info$min, 2), ", ", round(type_info$max, 2), "]",
          ", suggested_type=", type_info$suggested_type)

  fig_dir <- .cdh_ensure_dir(file.path(output_dir, paste0("EDA_", label)))

  # ---- 7: resolve clinical_cols -------------------------------------------
  if (is.null(clinical_cols)) {
    dict <- DSI::datashield.aggregate(conns = ds_active[1], expr = call("clinical_variable_dictionaryDS"))[[1]]
    clinical_cols <- setdiff(c(dict$required, dict$optional), c(label, pat_id_col))
  }
  avail_cols <- Reduce(intersect, all_cols[present_sites])
  clinical_cols <- intersect(clinical_cols, avail_cols)
  if (length(clinical_cols) > 0) {
    numeric_per_site <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("numeric_columnsDS", as.symbol(data_object), paste(clinical_cols, collapse = "$")))
    clinical_cols <- Reduce(intersect, numeric_per_site)
  }
  message(length(clinical_cols), " clinical feature(s) available: ", paste(clinical_cols, collapse = ", "))

  summary_rows <- list()

  # ==========================================================================
  # 6. LABEL DISTRIBUTION
  # ==========================================================================
  message("Label distribution...")
  dist_per_site <- DSI::datashield.aggregate(conns = ds_active,
    expr = call("label_distributionDS", as.symbol(data_object), label, num_bins, nfilter, max_categories_shown))

  dist_table <- do.call(rbind, lapply(names(dist_per_site), function(s) {
    ss <- dist_per_site[[s]]$summary
    data.frame(server = s, n = ss$n, mean = ss$mean, sd = ss$sd, min = ss$min, q25 = ss$q25,
               median = ss$median, q75 = ss$q75, max = ss$max, stringsAsFactors = FALSE)
  }))
  .cdh_save_csv(dist_table, fig_dir, "01_label_distribution.csv")

  freq_table <- NULL
  if (type_info$suggested_type %in% c("binary", "ordinal")) {
    freq_rows <- do.call(rbind, lapply(names(dist_per_site), function(s) {
      f <- dist_per_site[[s]]$frequency_table
      if (is.null(f) || !identical(f$type, "full")) return(NULL)
      data.frame(server = s, category = names(f$counts), count = as.numeric(unlist(f$counts)),
                 stringsAsFactors = FALSE)
    }))
    if (!is.null(freq_rows) && nrow(freq_rows) > 0) {
      freq_rows$pct <- with(freq_rows, ave(count, server, FUN = function(x) 100 * x / sum(x, na.rm = TRUE)))
      .cdh_save_csv(freq_rows, fig_dir, "02_label_frequency.csv")
      freq_table <- freq_rows
    }
  }

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    for (s in names(dist_per_site)) {
      h <- dist_per_site[[s]]$histogram
      if (!is.null(h)) {
        .cdh_save_plot(.cdh_plot_histogram(h$breaks, h$counts, paste0(label, " distribution -- ", s), label),
                       fig_dir, paste0("01_label_distribution_", s, ".png"))
      }
    }
    if (!is.null(freq_table)) {
      p <- ggplot2::ggplot(freq_table, ggplot2::aes(x = category, y = count)) +
        ggplot2::geom_col() + ggplot2::facet_wrap(~server) +
        ggplot2::labs(title = paste0(label, " frequency"), y = "count (% within site)") +
        ggplot2::theme_minimal()
      .cdh_save_plot(p, fig_dir, "02_label_frequency.png")
    }
  }

  # ==========================================================================
  # 8. LABEL vs. CLINICAL FEATURES: SPEARMAN (BH-FDR, meta-pooled across sites)
  # ==========================================================================
  spearman_result <- NULL
  if (length(clinical_cols) > 0) {
    message("Label-feature Spearman correlations...")
    sp_per_site <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("spearman_multiDS", as.symbol(data_object), label, paste(clinical_cols, collapse = "$"), nfilter))

    pooled <- setNames(lapply(clinical_cols, function(f) {
      n_vec <- vapply(sp_per_site, function(s) s[[f]]$n %||% NA_integer_, numeric(1))
      rho_vec <- vapply(sp_per_site, function(s) s[[f]]$rho %||% NA_real_, numeric(1))
      m <- .cdh_meta_spearman(n_vec, rho_vec)
      list(n = m$n_total, rho = m$rho, p_value = m$p_value)
    }), clinical_cols)

    spearman_result <- .cdh_fdr_table(pooled, estimate_field = "rho", estimate_name = "rho")
    spearman_result <- spearman_result[order(spearman_result$p_value), ]
    .cdh_save_csv(spearman_result, fig_dir, "03_label_feature_spearman.csv")

    if (requireNamespace("ggplot2", quietly = TRUE)) {
      hm <- ggplot2::ggplot(spearman_result, ggplot2::aes(x = feature, y = 1, fill = rho)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(!is.na(fdr) & fdr < 0.05, "*", ""))) +
        ggplot2::scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                                      midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
        ggplot2::labs(title = paste0(label, " vs. features: Spearman rho (* = FDR<0.05)"), x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), axis.text.y = ggplot2::element_blank())
      .cdh_save_plot(hm, fig_dir, "03_label_feature_spearman.png")
    }

    for (i in seq_len(nrow(spearman_result))) summary_rows[[length(summary_rows) + 1]] <- data.frame(
      label = label, analysis = "spearman", feature = spearman_result$feature[i], n = spearman_result$n[i],
      estimate = spearman_result$rho[i], statistic = NA_real_,
      p_value = spearman_result$p_value[i], fdr = spearman_result$fdr[i], stringsAsFactors = FALSE)
  }

  # ==========================================================================
  # 9. FEATURE DISTRIBUTIONS ACROSS LABEL LEVELS (Kruskal-Wallis; per-site --
  #    NOT meta-pooled, see function docstring)
  # ==========================================================================
  kruskal_result <- NULL
  n_levels <- type_info$n_unique
  if (!is.na(n_levels) && n_levels >= 3 && length(clinical_cols) > 0) {
    if (n_levels > kruskal_max_levels) {
      message("Label has ", n_levels, " levels (> kruskal_max_levels=", kruskal_max_levels,
              ") -- skipping per-level boxplots/Kruskal-Wallis (would mean too many group plots); ",
              "relying on the continuous Spearman analysis above instead.")
    } else {
      message("Feature distributions across label levels (Kruskal-Wallis, per site)...")
      kw_per_site <- DSI::datashield.aggregate(conns = ds_active,
        expr = call("kruskal_by_groupDS", as.symbol(data_object), paste(clinical_cols, collapse = "$"), label, nfilter))

      kruskal_result <- do.call(rbind, lapply(names(kw_per_site), function(s) {
        do.call(rbind, lapply(names(kw_per_site[[s]]), function(f) {
          r <- kw_per_site[[s]][[f]]
          data.frame(server = s, feature = f, statistic = r$statistic, df = r$df,
                     p_value = r$p_value, n = r$n_total, stringsAsFactors = FALSE)
        }))
      }))
      kruskal_result$fdr <- stats::p.adjust(kruskal_result$p_value, method = "BH")
      .cdh_save_csv(kruskal_result, fig_dir, "04_feature_by_label_kruskal.csv")

      if (requireNamespace("ggplot2", quietly = TRUE)) {
        for (f in clinical_cols) {
          for (s in names(ds_active)) {
            gss <- DSI::datashield.aggregate(conns = ds_active[s],
              expr = call("group_summary_statsDS", as.symbol(data_object), f, label, nfilter))[[1]]
            if (length(gss) >= 2) {
              .cdh_save_plot(.cdh_plot_summary_boxplot(gss, paste0(f, " by ", label, " -- ", s), f),
                             fig_dir, paste0("04_boxplot_", f, "_by_", label, "_", s, ".png"))
            }
          }
        }
      }

      for (i in seq_len(nrow(kruskal_result))) summary_rows[[length(summary_rows) + 1]] <- data.frame(
        label = label, analysis = "kruskal_wallis", feature = kruskal_result$feature[i], n = kruskal_result$n[i],
        estimate = NA_real_, statistic = kruskal_result$statistic[i],
        p_value = kruskal_result$p_value[i], fdr = kruskal_result$fdr[i], stringsAsFactors = FALSE)
    }
  }

  # ==========================================================================
  # 10. BINARY ANALYSIS (Wilcoxon; per site)
  # ==========================================================================
  wilcox_result <- NULL
  if (identical(type_info$suggested_type, "binary") && length(clinical_cols) > 0) {
    message("Binary label: Wilcoxon (Mann-Whitney) tests, per site...")
    wx_per_site <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("wilcox_binaryDS", as.symbol(data_object), paste(clinical_cols, collapse = "$"), label, nfilter))

    wilcox_result <- do.call(rbind, lapply(names(wx_per_site), function(s) {
      do.call(rbind, lapply(names(wx_per_site[[s]]), function(f) {
        r <- wx_per_site[[s]][[f]]
        data.frame(server = s, feature = f, group_1_n = r$group_1_n, group_2_n = r$group_2_n,
                   p_value = r$p_value, stringsAsFactors = FALSE)
      }))
    }))
    wilcox_result$fdr <- stats::p.adjust(wilcox_result$p_value, method = "BH")
    .cdh_save_csv(wilcox_result, fig_dir, "wilcoxon_binary.csv")

    for (i in seq_len(nrow(wilcox_result))) summary_rows[[length(summary_rows) + 1]] <- data.frame(
      label = label, analysis = "wilcoxon", feature = wilcox_result$feature[i],
      n = wilcox_result$group_1_n[i] + wilcox_result$group_2_n[i],
      estimate = NA_real_, statistic = NA_real_,
      p_value = wilcox_result$p_value[i], fdr = wilcox_result$fdr[i], stringsAsFactors = FALSE)
  }

  # ==========================================================================
  # 11. MULTIVARIABLE MODEL (coefficients meta-pooled across sites)
  # ==========================================================================
  model_result <- NULL
  if (length(clinical_cols) > 0) {
    model_type <- switch(type_info$suggested_type, continuous = "lm", binary = "glm", ordinal = "polr")
    if (model_type == "polr" && !requireNamespace("MASS", quietly = TRUE)) {
      message("MASS not installed -- falling back to lm for the ordinal label '", label,
              "'. DOCUMENTED ASSUMPTION: treated as continuous for this model only.")
      model_type <- "lm"
    }
    message("Multivariable model (", model_type, ")...")
    mv_per_site <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("multivariable_modelDS", as.symbol(data_object), label, paste(clinical_cols, collapse = "$"),
                  model_type, standardize, nfilter))

    ok_sites <- Filter(function(x) is.null(x$error), mv_per_site)
    if (length(ok_sites) == 0) {
      message("Model failed to fit at every site: ",
              paste(unique(vapply(mv_per_site, function(x) x$error %||% "unknown", character(1))), collapse = "; "))
    } else {
      all_terms <- unique(unlist(lapply(ok_sites, function(x) names(x$coefficients))))
      coef_table <- do.call(rbind, lapply(all_terms, function(tm) {
        est_vec <- vapply(ok_sites, function(x) x$coefficients[[tm]]$estimate %||% NA_real_, numeric(1))
        se_vec <- vapply(ok_sites, function(x) x$coefficients[[tm]]$se %||% NA_real_, numeric(1))
        pooled <- .cdh_meta_coef(est_vec, se_vec)
        data.frame(term = tm, estimate = pooled$estimate, se = pooled$se, p_value = pooled$p_value,
                   n_sites_contributing = pooled$n_sites, stringsAsFactors = FALSE)
      }))
      coef_table$ci_low <- coef_table$estimate - 1.96 * coef_table$se
      coef_table$ci_high <- coef_table$estimate + 1.96 * coef_table$se
      if (model_type == "glm") {
        coef_table$odds_ratio <- exp(coef_table$estimate)
        coef_table$or_ci_low <- exp(coef_table$ci_low)
        coef_table$or_ci_high <- exp(coef_table$ci_high)
      }
      n_total <- sum(vapply(ok_sites, function(x) x$n, numeric(1)))
      fit_stat <- if (model_type == "lm") {
        c(adj_r_squared = mean(vapply(ok_sites, function(x) x$adj_r_squared %||% NA_real_, numeric(1)), na.rm = TRUE))
      } else {
        c(aic_mean = mean(vapply(ok_sites, function(x) x$aic %||% NA_real_, numeric(1)), na.rm = TRUE))
      }
      message("  Model N (pooled) = ", n_total, "; standardized predictors = ", standardize)
      .cdh_save_csv(coef_table, fig_dir, paste0("05_multivariable_", model_type, ".csv"))

      model_result <- list(model_type = model_type, standardized = standardize, n = n_total,
                            fit_stat = fit_stat, coefficients = coef_table, per_site = mv_per_site)

      for (i in seq_len(nrow(coef_table))) summary_rows[[length(summary_rows) + 1]] <- data.frame(
        label = label, analysis = paste0("model_", model_type), feature = coef_table$term[i], n = n_total,
        estimate = coef_table$estimate[i], statistic = NA_real_, p_value = coef_table$p_value[i], fdr = NA_real_,
        stringsAsFactors = FALSE)
    }
  }

  # ==========================================================================
  # 12. RANDOM FOREST (EXPLORATORY ONLY -- not causal; averaged across sites)
  # ==========================================================================
  rf_result <- NULL
  if (isTRUE(run_random_forest) && length(clinical_cols) > 0) {
    message("Random forest variable importance (EXPLORATORY -- not causal inference)...")
    rf_classification <- identical(type_info$suggested_type, "binary")
    rf_per_site <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("random_forest_importanceDS", as.symbol(data_object), label, paste(clinical_cols, collapse = "$"),
                  rf_classification, rf_ntree, nfilter))

    if (!isTRUE(rf_per_site[[1]]$available)) {
      message("  randomForest package not installed server-side -- skipped (flagged only, not approximated).")
    } else {
      ok <- Filter(function(x) isTRUE(x$available) && is.null(x$error), rf_per_site)
      if (length(ok) == 0) {
        message("  Random forest failed to fit at every site.")
      } else {
        all_feats <- unique(unlist(lapply(ok, function(x) names(x$importance))))
        avg_imp <- sort(sapply(all_feats, function(f) mean(
          vapply(ok, function(x) x$importance[[f]] %||% NA_real_, numeric(1)), na.rm = TRUE)), decreasing = TRUE)
        top15 <- head(avg_imp, 15)
        imp_df <- data.frame(feature = names(top15), importance = as.numeric(top15), stringsAsFactors = FALSE)
        .cdh_save_csv(imp_df, fig_dir, "05_random_forest_importance.csv")

        if (requireNamespace("ggplot2", quietly = TRUE)) {
          p <- ggplot2::ggplot(imp_df, ggplot2::aes(x = stats::reorder(feature, importance), y = importance)) +
            ggplot2::geom_col() + ggplot2::coord_flip() +
            ggplot2::labs(title = "Random forest importance (EXPLORATORY -- not causal)", x = NULL, y = "importance (avg. across sites)") +
            ggplot2::theme_minimal()
          .cdh_save_plot(p, fig_dir, "05_random_forest_importance.png")
        }
        rf_result <- list(importance = imp_df, per_site = rf_per_site)
      }
    }
  }

  # ==========================================================================
  # 13. FOCUSED CORRELATION MATRIX (label + features), per site
  # ==========================================================================
  message("Correlation matrix (label + features)...")
  corr_per_site <- DSI::datashield.aggregate(conns = ds_active,
    expr = call("correlation_matrix_labelDS", as.symbol(data_object), label, paste(clinical_cols, collapse = "$"), nfilter))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    for (s in names(corr_per_site)) {
      if (!is.null(corr_per_site[[s]])) {
        .cdh_save_plot(.cdh_plot_corr_heatmap(corr_per_site[[s]], paste0("Correlation matrix -- ", s)),
                       fig_dir, paste0("06_correlation_matrix_", s, ".png"))
      }
    }
  }

  # ==========================================================================
  # 22. FINAL SUMMARY
  # ==========================================================================
  summary_table <- if (length(summary_rows)) do.call(rbind, summary_rows) else NULL
  .cdh_save_csv(summary_table, fig_dir, "analysis_summary.csv")

  human_summary <- c(
    paste0("Label: ", label, " (", type_info$suggested_type, "), n=", type_info$n_nonmissing,
           ", range=[", round(type_info$min, 2), ",", round(type_info$max, 2), "]"),
    paste0("Clinical features analyzed: ", length(clinical_cols), " (", paste(clinical_cols, collapse = ", "), ")"),
    if (!is.null(spearman_result) && nrow(spearman_result) > 0) paste0(
      "Strongest Spearman association: ", spearman_result$feature[which.min(spearman_result$p_value)],
      " (rho=", round(spearman_result$rho[which.min(spearman_result$p_value)], 3), ")"),
    if (!is.null(spearman_result)) paste0("FDR-significant Spearman associations: ", sum(spearman_result$fdr < 0.05, na.rm = TRUE)),
    if (!is.null(model_result)) paste0("Multivariable model (", model_result$model_type, "): pooled N=", model_result$n),
    if (isTRUE(run_random_forest) && is.null(rf_result)) "Random forest: not available or failed to fit (see messages).",
    "LIMITATION: Kruskal-Wallis/Wilcoxon results are reported PER SITE, not meta-pooled across sites.",
    "LIMITATION: random forest importance is exploratory only, NOT a causal or federated-optimal estimate."
  )
  writeLines(human_summary, file.path(fig_dir, "human_readable_summary.txt"))

  message("\nResults saved under: ", fig_dir)

  list(label = label, type_info = type_info, clinical_cols = clinical_cols,
       distribution = dist_table, frequency_table = freq_table,
       spearman = spearman_result, kruskal = kruskal_result, wilcoxon = wilcox_result,
       model = model_result, random_forest = rf_result, correlation = corr_per_site,
       summary_table = summary_table, fig_dir = fig_dir)
}
