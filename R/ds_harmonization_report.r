#' @title Non-disclosive harmonization report (tables + figures)
#' @description One-call orchestrator that pulls only aggregate,
#'   disclosure-checked statistics from every site (via
#'   \code{harmonization_summaryDS}) and turns them into a small set of
#'   report tables and \code{ggplot2} figures the analyst can put directly
#'   into a harmonization report. No row-level or patient-level value is ever
#'   transmitted from any site -- every number behind these figures is a
#'   count, percentage, or summary statistic that already passed the
#'   server-side \code{nfilter} floor.
#'
#'   Figures produced (as a named list of ggplot objects, only if
#'   \code{ggplot2} is installed -- if not, only the tables are returned):
#'   \itemize{
#'     \item \code{missingness_plot}: bar chart of \% missing per variable,
#'       faceted by server.
#'     \item \code{numeric_summary_plot}: point-range plot (median, IQR) per
#'       numeric variable, faceted by server.
#'     \item \code{categorical_counts_plot}: one bar chart per categorical
#'       variable (list element), faceted by server, cells below the site's
#'       \code{nfilter} shown as blank/NA.
#'   }
#'
#' Server function called: \code{harmonization_summaryDS}
#'
#' @param df Character string naming the server-side data frame to summarize.
#' @param nfilter Minimum count required before a statistic/cell is released
#'   (passed straight through to \code{harmonization_summaryDS}).
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{missingness_table}: data.frame, variable x server \% missing.
#'     \item \code{numeric_summary_table}: data.frame, one row per
#'       (server, variable) numeric summary.
#'     \item \code{categorical_summary}: named list (per variable) of
#'       data.frames, one row per (server, category, count).
#'     \item \code{figures}: named list of ggplot objects (see above), or
#'       \code{NULL} if \code{ggplot2} is not available.
#'   }
#' @export
ds.harmonization_report <- function(df, nfilter = 5, datasources = NULL) {

  if (is.null(datasources)) datasources <- datashield.connections_find()

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("harmonization_summaryDS", as.symbol(df), nfilter)
  )

  server_names <- names(results)

  # ---- missingness table -----------------------------------------------
  missingness_table <- do.call(rbind, lapply(server_names, function(srv) {
    mp <- results[[srv]]$missing_pct
    data.frame(server = srv, variable = names(mp), missing_pct = as.numeric(mp),
               row.names = NULL, stringsAsFactors = FALSE)
  }))

  # ---- numeric summary table ---------------------------------------------
  numeric_summary_table <- do.call(rbind, lapply(server_names, function(srv) {
    ns <- results[[srv]]$numeric_summary
    if (length(ns) == 0) return(NULL)
    do.call(rbind, lapply(names(ns), function(vn) {
      s <- ns[[vn]]
      data.frame(server = srv, variable = vn, n = s$n, mean = s$mean, sd = s$sd,
                 min = s$min, q25 = s$q25, median = s$median, q75 = s$q75, max = s$max,
                 stringsAsFactors = FALSE)
    }))
  }))

  # ---- categorical summary tables (one per variable) ---------------------
  all_cat_vars <- unique(unlist(lapply(results, function(r) names(r$categorical_summary))))
  categorical_summary <- lapply(all_cat_vars, function(vn) {
    do.call(rbind, lapply(server_names, function(srv) {
      cs <- results[[srv]]$categorical_summary[[vn]]
      if (is.null(cs) || length(cs) == 0) return(NULL)
      data.frame(server = srv, category = names(cs),
                 count = as.numeric(unlist(cs)), stringsAsFactors = FALSE)
    }))
  })
  names(categorical_summary) <- all_cat_vars

  # ---- figures (only if ggplot2 is available) ----------------------------
  figures <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    figures <- list()

    figures$missingness_plot <- ggplot2::ggplot(
        missingness_table, ggplot2::aes(x = variable, y = missing_pct)) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(~server) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Missingness by variable and server",
                    x = NULL, y = "% missing") +
      ggplot2::theme_minimal()

    if (!is.null(numeric_summary_table) && nrow(numeric_summary_table) > 0) {
      figures$numeric_summary_plot <- ggplot2::ggplot(
          numeric_summary_table,
          ggplot2::aes(x = variable, y = median, ymin = q25, ymax = q75)) +
        ggplot2::geom_pointrange() +
        ggplot2::facet_wrap(~server) +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Numeric variables: median (IQR) by server",
                      x = NULL, y = "value") +
        ggplot2::theme_minimal()
    }

    if (length(categorical_summary) > 0) {
      figures$categorical_counts_plot <- lapply(names(categorical_summary), function(vn) {
        tab <- categorical_summary[[vn]]
        if (is.null(tab) || nrow(tab) == 0) return(NULL)
        ggplot2::ggplot(tab, ggplot2::aes(x = category, y = count)) +
          ggplot2::geom_col() +
          ggplot2::facet_wrap(~server) +
          ggplot2::labs(title = paste0(vn, ": counts by category and server"),
                        x = NULL, y = "count (cells < nfilter suppressed)") +
          ggplot2::theme_minimal()
      })
      names(figures$categorical_counts_plot) <- names(categorical_summary)
    }
  } else {
    message("ggplot2 not installed -- returning tables only, no figures.")
  }

  list(
    missingness_table = missingness_table,
    numeric_summary_table = numeric_summary_table,
    categorical_summary = categorical_summary,
    figures = figures
  )
}
