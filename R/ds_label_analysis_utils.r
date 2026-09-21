#' @title Combine per-site label_type_infoDS results into one classification
#' @description Aggregates n_nonmissing (summed), min/max (extremes across
#'   sites), and the union of observed values (only if every site that has
#'   the label also reported \code{values}, i.e. stayed within
#'   \code{max_levels_shown} at every site) into a single suggested type.
#' @export
.cdh_combine_type_info <- function(per_site, max_levels_shown = 20) {
  present <- Filter(function(x) isTRUE(x$exists) && !isTRUE(x$below_nfilter), per_site)
  if (length(present) == 0) return(NULL)

  n_nonmissing <- sum(vapply(present, function(x) x$n_nonmissing, numeric(1)))
  is_numeric <- all(vapply(present, function(x) isTRUE(x$is_numeric), logical(1)))
  is_integer_valued <- all(vapply(present, function(x) isTRUE(x$is_integer_valued), logical(1)))

  all_have_values <- all(vapply(present, function(x) !is.null(x$values), logical(1)))
  values <- if (all_have_values) sort(unique(unlist(lapply(present, `[[`, "values")))) else NULL
  n_unique <- if (!is.null(values)) length(values) else NA_integer_

  min_val <- min(vapply(present, function(x) x$min, numeric(1)))
  max_val <- max(vapply(present, function(x) x$max, numeric(1)))

  suggested_type <- if (!is.na(n_unique) && n_unique == 2) {
    "binary"
  } else if (is_numeric && is_integer_valued && !is.na(n_unique) && n_unique <= max_levels_shown) {
    "ordinal"
  } else {
    "continuous"
  }

  list(n_nonmissing = n_nonmissing, is_numeric = is_numeric, is_integer_valued = is_integer_valued,
       n_unique = n_unique, values = values, min = min_val, max = max_val,
       suggested_type = suggested_type, sites_present = names(present))
}

#' @title Build a feature/n/estimate/p_value/fdr data.frame with BH correction
#' @description Shared by the Spearman and Kruskal-Wallis/Wilcoxon steps.
#' @param results_list Named list (feature -> per-feature stat list, as
#'   returned by e.g. \code{spearman_multiDS}).
#' @param estimate_field Name of the field in each entry holding the point
#'   estimate (e.g. \code{"rho"} or \code{"statistic"}).
#' @param estimate_name Column name to use for the estimate in the output.
#' @export
.cdh_fdr_table <- function(results_list, estimate_field = "rho", estimate_name = "rho",
                            n_field = "n", p_field = "p_value") {
  feats <- names(results_list)
  df <- data.frame(
    feature = feats,
    n = vapply(feats, function(f) results_list[[f]][[n_field]] %||% NA_integer_, numeric(1)),
    estimate = vapply(feats, function(f) results_list[[f]][[estimate_field]] %||% NA_real_, numeric(1)),
    p_value = vapply(feats, function(f) results_list[[f]][[p_field]] %||% NA_real_, numeric(1)),
    stringsAsFactors = FALSE
  )
  names(df)[names(df) == "estimate"] <- estimate_name
  df$fdr <- stats::p.adjust(df$p_value, method = "BH")
  df
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' @title Fisher-z meta-analytic pooling of per-site Spearman correlations
#' @description Standard fixed-effect meta-analysis for correlation
#'   coefficients: Fisher-z transform each site's rho, inverse-variance
#'   weight by (n-3), pool, back-transform. The same family of technique
#'   \code{ds.glmSLMA} uses for regression coefficients elsewhere in
#'   DataSHIELD (study-level meta-analysis) -- nothing here touches
#'   row-level data, it only combines already-aggregate per-site statistics.
#' @param n_vec,rho_vec Numeric vectors, one entry per site (NAs allowed and
#'   dropped).
#' @return \code{list(rho=, p_value=, n_total=, n_sites=)}, or all-\code{NA}
#'   if fewer than 1 usable site.
#' @export
.cdh_meta_spearman <- function(n_vec, rho_vec) {
  ok <- !is.na(n_vec) & !is.na(rho_vec) & n_vec > 3
  if (!any(ok)) return(list(rho = NA_real_, p_value = NA_real_, n_total = sum(n_vec, na.rm = TRUE), n_sites = 0L))
  z <- atanh(pmin(pmax(rho_vec[ok], -0.999999), 0.999999))
  w <- n_vec[ok] - 3
  z_pooled <- sum(z * w) / sum(w)
  se_pooled <- sqrt(1 / sum(w))
  p_value <- 2 * stats::pnorm(abs(z_pooled / se_pooled), lower.tail = FALSE)
  list(rho = tanh(z_pooled), p_value = p_value, n_total = sum(n_vec[ok]), n_sites = sum(ok))
}

#' @title Inverse-variance meta-analytic pooling of a per-site model coefficient
#' @description Fixed-effect meta-analysis: weight each site's estimate by
#'   1/se^2, pool. Standard technique (same family as \code{ds.glmSLMA}).
#' @param estimate_vec,se_vec Numeric vectors, one entry per site.
#' @return \code{list(estimate=, se=, p_value=, n_sites=)}.
#' @export
.cdh_meta_coef <- function(estimate_vec, se_vec) {
  ok <- !is.na(estimate_vec) & !is.na(se_vec) & se_vec > 0
  if (!any(ok)) return(list(estimate = NA_real_, se = NA_real_, p_value = NA_real_, n_sites = 0L))
  w <- 1 / (se_vec[ok]^2)
  est_pooled <- sum(estimate_vec[ok] * w) / sum(w)
  se_pooled <- sqrt(1 / sum(w))
  p_value <- 2 * stats::pnorm(abs(est_pooled / se_pooled), lower.tail = FALSE)
  list(estimate = est_pooled, se = se_pooled, p_value = p_value, n_sites = sum(ok))
}

#' @title Build a ggplot histogram from server-computed bin breaks/counts
#' @export
.cdh_plot_histogram <- function(breaks, counts, title, xlab = NULL) {
  mids <- head(breaks, -1) + diff(breaks) / 2
  d <- data.frame(mid = mids, count = as.numeric(counts))
  ggplot2::ggplot(d, ggplot2::aes(x = mid, y = count)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = title, x = xlab, y = "count (bins < nfilter suppressed)") +
    ggplot2::theme_minimal()
}

# #' @title Build a ggplot "boxplot" from a precomputed five-number summary
# #' @description Uses \code{geom_boxplot(stat = "identity")}, since no
# #'   row-level data is ever available to compute a boxplot the normal way.
# #' @param summary_list Named list, group -> list(min, q25, median, q75, max, n).
# #' @export
# .cdh_plot_summary_boxplot <- function(summary_list, title, ylab = NULL) {
#   groups <- names(summary_list)
#   d <- do.call(rbind, lapply(groups, function(g) {
#     s <- summary_list[[g]]
#     data.frame(group = g, ymin = s$min, lower = s$q25, middle = s$median,
#                upper = s$q75, ymax = s$max, n = s$n, stringsAsFactors = FALSE)
#   }))
#   ggplot2::ggplot(d, ggplot2::aes(x = group, ymin = ymin, lower = lower, middle = middle,
#                                    upper = upper, ymax = ymax)) +
#     ggplot2::geom_boxplot(stat = "identity") +
#     ggplot2::geom_text(ggplot2::aes(y = ymax, label = paste0("n=", n)), vjust = -0.5, size = 3) +
#     ggplot2::labs(title = title, x = NULL, y = ylab) +
#     ggplot2::theme_minimal()
# }

#' @export
.cdh_plot_summary_boxplot <- function(
    results,
    title = "Follow-up duration by cohort",
    ylab = "months"
) {

  if (is.null(results) || length(results) == 0L) {
    return(NULL)
  }

  rows <- list()
  k <- 0L

  # ------------------------------------------------------------
  # results is expected to be:
  #
  # site1
  #   R
  #     min, q25, median, q75, max, ...
  #   STRAPPAT
  #
  # site2
  #   R
  #   STRAPPAT
  # ------------------------------------------------------------

  for (site in names(results)) {

    site_res <- results[[site]]

    if (is.null(site_res) || length(site_res) == 0L) {
      next
    }

    # If site result is not named, nothing useful to plot
    if (is.null(names(site_res))) {
      next
    }

    for (g in names(site_res)) {

      s <- site_res[[g]]

      if (is.null(s) || length(s) == 0L) {
        next
      }

      # ----------------------------------------------------------
      # Safely extract scalar summary values
      # ----------------------------------------------------------

      get_scalar <- function(x) {
        if (is.null(x) || length(x) == 0L) {
          return(NA_real_)
        }

        x <- suppressWarnings(as.numeric(x))

        if (length(x) == 0L || is.na(x[1])) {
          return(NA_real_)
        }

        x[1]
      }

      ymin   <- get_scalar(s$min)
      lower  <- get_scalar(s$q25)
      middle <- get_scalar(s$median)
      upper  <- get_scalar(s$q75)
      ymax   <- get_scalar(s$max)

      # ----------------------------------------------------------
      # Don't create a row if there is no releasable summary
      # ----------------------------------------------------------

      if (all(is.na(c(ymin, lower, middle, upper, ymax)))) {
        next
      }

      k <- k + 1L

      rows[[k]] <- data.frame(
        server = site,
        group = g,
        ymin = ymin,
        lower = lower,
        middle = middle,
        upper = upper,
        ymax = ymax,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    warning(
      "No releasable follow-up summary statistics available for plotting."
    )
    return(NULL)
  }

  plot_df <- do.call(rbind, rows)

  rownames(plot_df) <- NULL

  # ------------------------------------------------------------
  # Plot summary boxplots
  # ------------------------------------------------------------

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = group,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    )
  ) +
    ggplot2::geom_boxplot(
      stat = "identity"
    ) +
    ggplot2::facet_wrap(
      ~server,
      scales = "free_x"
    ) +
    ggplot2::labs(
      title = title,
      x = "Cohort",
      y = ylab
    ) +
    ggplot2::theme_minimal()
}

#' @title Build a ggplot heatmap from a correlation-matrix result
#' @export
.cdh_plot_corr_heatmap <- function(corr, title) {
  if (is.null(corr)) return(NULL)
  tab <- as.data.frame(as.table(corr$matrix), stringsAsFactors = FALSE)
  names(tab) <- c("var1", "var2", "correlation")
  ggplot2::ggplot(tab, ggplot2::aes(x = var1, y = var2, fill = correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                                  midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' @title Build a ggplot "scatter" from a 2D binned joint histogram
#' @description Renders bin CENTERS sized/colored by count, as the
#'   disclosure-safe approximation to a scatterplot -- see
#'   \code{joint_histogram_2dDS}.
#' @export
.cdh_plot_joint_histogram <- function(jh, xlab, ylab, title) {
  if (is.null(jh)) return(NULL)
  x_mids <- head(jh$x_breaks, -1) + diff(jh$x_breaks) / 2
  y_mids <- head(jh$y_breaks, -1) + diff(jh$y_breaks) / 2
  counts <- jh$counts
  d <- expand.grid(x = x_mids, y = y_mids)
  d$count <- as.vector(t(counts))
  d <- d[!is.na(d$count) & d$count > 0, , drop = FALSE]
  ggplot2::ggplot(d, ggplot2::aes(x = x, y = y, size = count, color = count)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_gradient(low = "steelblue", high = "firebrick") +
    ggplot2::labs(title = title, x = xlab, y = ylab,
                  caption = "Disclosure-safe approximation: binned counts, not individual points (cells < nfilter suppressed)") +
    ggplot2::theme_minimal()
}

#' @title Ensure an output directory exists without touching existing files
#' @export
.cdh_ensure_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  dir_path
}

#' @title Save a ggplot to a directory if ggplot2/the plot are available
#' @export
.cdh_save_plot <- function(plot, dir_path, filename, width = 7, height = 5) {
  if (is.null(plot)) return(invisible(NULL))
  path <- file.path(dir_path, filename)
  tryCatch(
    ggplot2::ggsave(path, plot = plot, width = width, height = height),
    error = function(e) message("Could not save '", filename, "': ", conditionMessage(e))
  )
  invisible(path)
}

#' @title Save a data.frame as CSV to a directory
#' @export
.cdh_save_csv <- function(df, dir_path, filename) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  path <- file.path(dir_path, filename)
  utils::write.csv(df, path, row.names = FALSE)
  invisible(path)
}
