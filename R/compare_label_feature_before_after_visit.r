#' @title Compare a label-feature relationship before vs. after a visit cutoff
#' @description Fully generic across labels and features -- works
#'   identically for \code{label = "IP_Score"}, \code{"EP_Score"}, or
#'   \code{"D2T"} without any code change. For each feature: reports
#'   per-period (before/after \code{visit_cutoff}) descriptive stats and
#'   Spearman correlation (Fisher-z meta-pooled across sites), then formally
#'   tests whether the label-feature association DIFFERS between periods via
#'   an interaction model (\code{feature ~ label * period}, or
#'   \code{label ~ feature * period}), never by simply comparing the two
#'   periods' p-values. If \code{pat_ID} is available, attempts a
#'   patient-level mixed-effects version (\code{lme4::lmer} with a random
#'   intercept per patient) to account for repeated measurements; if that
#'   isn't available or doesn't converge, falls back to the simpler
#'   aggregate interaction test AND explicitly reports that repeated
#'   observations may violate its independence assumption -- never silently.
#'
#'   \code{pat_ID} VALUES are never returned or used for anything beyond
#'   internal grouping server-side; only a count of distinct patients is
#'   ever reported.
#'
#' @param label Character, the label column.
#' @param features Character vector of feature column names.
#' @param visit_cutoff Numeric; \code{visit_col < visit_cutoff} is "before",
#'   \code{>=} is "after".
#' @param data_object Character, the server-side data frame. Default \code{"Draw_cleaned"}.
#' @param visit_col Character, the visit/time column. Default \code{"Visit"}.
#' @param pat_id_col Character, the patient identifier column (grouping only).
#' @param outcome_is_label Logical; \code{TRUE} fits \code{label ~ feature *
#'   period}, \code{FALSE} (default) fits \code{feature ~ label * period} --
#'   the more common framing (does the biomarker's relationship to the score
#'   change over time), but explicit and overridable either way.
#' @param output_dir Base directory; results go under
#'   \code{file.path(output_dir, paste0("EDA_", label))} (same directory
#'   \code{run_clinical_label_eda()} uses for this label, files numbered 07-10).
#' @param nfilter Disclosure/stability floor used throughout.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return \code{NULL} (with a message) if \code{label}, any \code{feature},
#'   or \code{visit_col} is unavailable. Otherwise a named list, one entry
#'   per feature, each with \code{period_table} (section-15 table),
#'   \code{interaction} (aggregate interaction-model result),
#'   \code{mixed_interaction} (mixed-model result, or \code{NULL} with a
#'   noted limitation), and \code{fig_dir}.
#' @export
compare_label_feature_before_after_visit <- function(label, features, visit_cutoff,
                                                       data_object = "Draw_cleaned",
                                                       visit_col = "Visit",
                                                       pat_id_col = "pat_ID",
                                                       outcome_is_label = FALSE,
                                                       output_dir = ".",
                                                       nfilter = 5,
                                                       datasources = NULL) {

  if (is.null(datasources)) datasources <- datashield.connections_find()

  # ---- 21: validate columns exist ----------------------------------------
  all_cols <- DSI::datashield.aggregate(conns = datasources,
    expr = call("list_columnsDS", as.symbol(data_object)))
  needed <- c(label, features, visit_col)
  union_cols <- sort(unique(unlist(all_cols)))
  missing_any <- setdiff(needed, union_cols)
  if (length(missing_any) > 0) {
    message("Requested column(s) not available in ", data_object, ": ", paste(missing_any, collapse = ", "))
    message("Available columns: ", paste(union_cols, collapse = ", "))
    return(NULL)
  }
  present_sites <- names(all_cols)[vapply(all_cols, function(cn) all(needed %in% cn), logical(1))]
  if (length(present_sites) == 0) {
    message("No single site has all of: ", paste(needed, collapse = ", "), " -- cannot compare.")
    return(NULL)
  }
  ds_active <- datasources[present_sites]
  has_pat_id <- pat_id_col %in% Reduce(intersect, all_cols[present_sites])

  fig_dir <- .cdh_ensure_dir(file.path(output_dir, paste0("EDA_", label)))
  results <- list()

  for (feature in features) {
    message("\n=== ", label, " vs. ", feature, " (before/after Visit=", visit_cutoff, ") ===")

    # ---- 15: per-period descriptive + Spearman table -----------------------
    lab_stats <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("period_group_statsDS", as.symbol(data_object), label, visit_col, visit_cutoff, nfilter))
    feat_stats <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("period_group_statsDS", as.symbol(data_object), feature, visit_col, visit_cutoff, nfilter))
    sp_stats <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("period_spearmanDS", as.symbol(data_object), label, feature, visit_col, visit_cutoff, nfilter))

    period_table <- do.call(rbind, lapply(c("before", "after"), function(per) {
      lab_n_vec <- vapply(lab_stats, function(s) s[[per]]$n %||% NA_integer_, numeric(1))
      lab_mean_vec <- vapply(lab_stats, function(s) s[[per]]$mean %||% NA_real_, numeric(1))
      lab_median_vec <- vapply(lab_stats, function(s) s[[per]]$median %||% NA_real_, numeric(1))
      feat_n_vec <- vapply(feat_stats, function(s) s[[per]]$n %||% NA_integer_, numeric(1))
      feat_mean_vec <- vapply(feat_stats, function(s) s[[per]]$mean %||% NA_real_, numeric(1))
      feat_median_vec <- vapply(feat_stats, function(s) s[[per]]$median %||% NA_real_, numeric(1))
      rho_vec <- vapply(sp_stats, function(s) s[[per]]$rho %||% NA_real_, numeric(1))
      sp_n_vec <- vapply(sp_stats, function(s) s[[per]]$n %||% NA_integer_, numeric(1))

      lab_n <- sum(lab_n_vec, na.rm = TRUE); feat_n <- sum(feat_n_vec, na.rm = TRUE)
      lab_mean <- stats::weighted.mean(lab_mean_vec, lab_n_vec, na.rm = TRUE)
      feat_mean <- stats::weighted.mean(feat_mean_vec, feat_n_vec, na.rm = TRUE)
      meta_sp <- .cdh_meta_spearman(sp_n_vec, rho_vec)

      # NOTE (approximation, flagged not hidden): a site's own median/quantiles
      # cannot be exactly re-pooled into one federation-wide median without
      # extra machinery (e.g. the binned-ecdf approach used elsewhere in this
      # codebase); "label_median"/"feature_median" below are the median OF
      # per-site medians, a standard but approximate federated summary -- NOT
      # the exact pooled median. Means are exact (site-n-weighted).
      data.frame(period = per, n = lab_n, label_n = lab_n, feature_n = feat_n,
                 label_mean = lab_mean, label_median = stats::median(lab_median_vec, na.rm = TRUE),
                 feature_mean = feat_mean, feature_median = stats::median(feat_median_vec, na.rm = TRUE),
                 spearman_rho = meta_sp$rho, spearman_p = meta_sp$p_value, spearman_n = meta_sp$n_total,
                 stringsAsFactors = FALSE)
    }))
    .cdh_save_csv(period_table, fig_dir, paste0("before_after_", feature, "_period_table.csv"))

    # ---- interaction test: does the association DIFFER between periods? ---
    inter_model_type <- if (identical(outcome_is_label, TRUE)) "lm" else "lm"  # both label/feature numeric here
    inter_per_site <- DSI::datashield.aggregate(conns = ds_active,
      expr = call("interaction_modelDS", as.symbol(data_object), label, feature, visit_col, visit_cutoff,
                  outcome_is_label, inter_model_type, nfilter))
    ok_inter <- Filter(function(x) is.null(x$error), inter_per_site)
    interaction_result <- if (length(ok_inter) > 0) {
      est_vec <- vapply(ok_inter, function(x) x$interaction$estimate, numeric(1))
      se_vec <- vapply(ok_inter, function(x) x$interaction$se, numeric(1))
      pooled <- .cdh_meta_coef(est_vec, se_vec)
      message("  Interaction (aggregate, ", length(ok_inter), " site(s)): estimate=",
              round(pooled$estimate, 4), ", p=", signif(pooled$p_value, 3))
      list(interaction_estimate = pooled$estimate, interaction_SE = pooled$se, interaction_p = pooled$p_value,
           n_sites = pooled$n_sites)
    } else {
      message("  Interaction model failed to fit at every site.")
      NULL
    }

    # ---- mixed-effects version, accounting for repeated measures -----------
    mixed_result <- NULL
    if (has_pat_id) {
      mixed_per_site <- DSI::datashield.aggregate(conns = ds_active,
        expr = call("mixed_interaction_modelDS", as.symbol(data_object), label, feature, visit_col,
                    visit_cutoff, pat_id_col, outcome_is_label, nfilter))
      if (!isTRUE(mixed_per_site[[1]]$available)) {
        message("  lme4 not installed server-side -- using the simpler aggregate interaction test above. ",
                "LIMITATION: repeated observations per patient may violate that test's independence assumption.")
      } else {
        ok_mixed <- Filter(function(x) is.null(x$error), mixed_per_site)
        if (length(ok_mixed) > 0) {
          est_vec <- vapply(ok_mixed, function(x) x$interaction$estimate, numeric(1))
          se_vec <- vapply(ok_mixed, function(x) x$interaction$se, numeric(1))
          pooled_m <- .cdh_meta_coef(est_vec, se_vec)
          n_patients <- sum(vapply(ok_mixed, function(x) x$n_patients, numeric(1)))
          message("  Mixed model (random intercept per patient, ", n_patients, " patients): interaction p=",
                  signif(pooled_m$p_value, 3))
          mixed_result <- list(interaction_estimate = pooled_m$estimate, interaction_SE = pooled_m$se,
                                interaction_p = pooled_m$p_value, n_patients = n_patients)
        } else {
          message("  Mixed model did not converge at any site -- using the simpler aggregate interaction test above. ",
                  "LIMITATION: repeated observations per patient may violate that test's independence assumption.")
        }
      }
    } else {
      message("  '", pat_id_col, "' not available -- cannot account for repeated measures. ",
              "Using the simpler aggregate interaction test above; independence assumption unverified.")
    }

    # ==========================================================================
    # 16. VISUALIZATIONS
    # ==========================================================================
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      # A: label-vs-feature "scatter" (disclosure-safe 2D binned approximation), by period
      for (per in c("before", "after")) {
        jh <- DSI::datashield.aggregate(conns = ds_active[1],
          expr = call("joint_histogram_2dDS", as.symbol(data_object), label, feature, 10L, 10L,
                      nfilter, visit_col, visit_cutoff, per))[[1]]
        if (!is.null(jh)) {
          .cdh_save_plot(.cdh_plot_joint_histogram(jh, label, feature, paste0(label, " vs ", feature, " -- ", per)),
                         fig_dir, paste0("07_", feature, "_", per, ".png"))
        }
      }
      # B: label / feature distribution by period
      lab_gss1 <- lab_stats[[1]]
      feat_gss1 <- feat_stats[[1]]
      if (length(lab_gss1) >= 1) {
        .cdh_save_plot(.cdh_plot_summary_boxplot(lab_gss1, paste0(label, " before/after Visit=", visit_cutoff), label),
                       fig_dir, paste0("08_", label, "_before_after.png"))
      }
      if (length(feat_gss1) >= 1) {
        .cdh_save_plot(.cdh_plot_summary_boxplot(feat_gss1, paste0(feature, " before/after Visit=", visit_cutoff), feature),
                       fig_dir, paste0("09_", feature, "_before_after.png"))
      }
      # C: compact before/after association summary
      summary_df <- data.frame(
        metric = c("Before: rho", "After: rho", "Interaction p"),
        value = c(round(period_table$spearman_rho[period_table$period == "before"], 3),
                  round(period_table$spearman_rho[period_table$period == "after"], 3),
                  if (!is.null(interaction_result)) signif(interaction_result$interaction_p, 3) else NA),
        stringsAsFactors = FALSE
      )
      p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = metric, y = 1, label = value)) +
        ggplot2::geom_tile(fill = "grey95", color = "white") + ggplot2::geom_text(size = 5) +
        ggplot2::labs(title = paste0(label, " vs. ", feature, ": before/after association summary"), x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
      .cdh_save_plot(p, fig_dir, paste0("10_before_after_association_summary_", feature, ".png"))
    }

    results[[feature]] <- list(period_table = period_table, interaction = interaction_result,
                                mixed_interaction = mixed_result, fig_dir = fig_dir)
  }

  message("\nResults saved under: ", fig_dir)
  results
}
