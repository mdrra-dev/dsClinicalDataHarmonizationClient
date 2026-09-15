#' @title Harmonization orchestrator: exploratory analysis
#' @description The third orchestrator alongside \code{ds.harmonization_diagnose()}
#'   and \code{ds.harmonization_clean()} -- rather than checking conformity or
#'   modifying data, this one describes the data: distributions, category
#'   frequencies, correlations between numeric variables, and (optionally) a
#'   simple longitudinal trend. Defaults to running on \code{"Draw_cleaned"},
#'   i.e. the object \code{ds.harmonization_clean()} /
#'   \code{ds.harmonization_orchestrator()} produce, since exploring cleaned,
#'   harmonized data is almost always what's wanted -- pass a different
#'   \code{df} to explore anything else (e.g. the raw input, for comparison).
#'
#'   Builds on \code{ds.exploratory_analysis()} (server function
#'   \code{exploratory_analysisDS}): every number behind the report/figures
#'   is an aggregate statistic, binned histogram count, or correlation cell
#'   that already passed the site's \code{nfilter} floor -- no row-level
#'   value is ever returned.
#'
#' @param df Character string naming the server-side object to explore.
#'   Default \code{"Draw_cleaned"}.
#' @param numeric_cols,categorical_cols Character vectors restricting which
#'   columns to summarize. \code{NULL} (default): every numeric / every
#'   non-numeric column respectively.
#' @param group_col Optional column for a group-wise trend figure (mean of
#'   each numeric variable at each level). Default \code{"Visit"}; set
#'   \code{NULL} to skip.
#' @param num_bins Number of histogram bins per numeric column. Default 20.
#' @param nfilter Disclosure/stability floor.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{object}: \code{df}.
#'     \item \code{stats}: raw per-server \code{exploratory_analysisDS} output.
#'     \item \code{report}: list with \code{numeric_summary} (per-site
#'       data.frame), \code{categorical_summary} (named list of per-site
#'       data.frames, one per variable), and \code{correlation} (named list
#'       of per-site correlation-matrix data.frames, one per server).
#'     \item \code{figures}: named list of ggplot objects (histograms --
#'       one per numeric column per server --, a categorical-frequency bar
#'       chart per variable, a correlation heatmap per server, and a
#'       group-trend line chart if \code{group_col} was usable), or
#'       \code{NULL} if ggplot2 isn't installed.
#'   }
#' @export
ds.harmonization_explore <- function(df = "Draw_cleaned",
                                      numeric_cols = NULL, categorical_cols = NULL,
                                      group_col = "Visit", num_bins = 20, nfilter = 5,
                                      datasources = NULL) {

  if (is.null(datasources)) datasources <- datashield.connections_find()

  message("Exploring '", df, "'...")
  stats_result <- DSI::datashield.aggregate(
    conns = datasources,
    expr = call("exploratory_analysisDS", as.symbol(df), numeric_cols, categorical_cols,
                group_col, num_bins, nfilter)
  )
  server_names <- names(stats_result)

  # ---- numeric summary table (per-site) ------------------------------------
  numeric_summary_table <- do.call(rbind, lapply(server_names, function(srv) {
    ns <- stats_result[[srv]]$numeric_summary
    if (length(ns) == 0) return(NULL)
    do.call(rbind, lapply(names(ns), function(vn) {
      s <- ns[[vn]]
      data.frame(server = srv, variable = vn, n = s$n, mean = s$mean, sd = s$sd,
                 min = s$min, q25 = s$q25, median = s$median, q75 = s$q75, max = s$max,
                 stringsAsFactors = FALSE)
    }))
  }))

  # ---- categorical summary tables (one per variable) -----------------------
  all_cat_vars <- unique(unlist(lapply(stats_result, function(r) names(r$categorical_summary))))
  categorical_summary <- lapply(all_cat_vars, function(vn) {
    do.call(rbind, lapply(server_names, function(srv) {
      cs <- stats_result[[srv]]$categorical_summary[[vn]]
      if (is.null(cs) || length(cs) == 0) return(NULL)
      data.frame(server = srv, category = names(cs), count = as.numeric(unlist(cs)),
                 stringsAsFactors = FALSE)
    }))
  })
  names(categorical_summary) <- all_cat_vars

  # ---- correlation tables (one per server) ---------------------------------
  correlation <- lapply(server_names, function(srv) {
    corr <- stats_result[[srv]]$correlation
    if (is.null(corr)) return(NULL)
    m <- corr$matrix
    tab <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
    setNames(tab, c("var1", "var2", "correlation"))
  })
  names(correlation) <- server_names
  correlation <- Filter(Negate(is.null), correlation)

  report <- list(numeric_summary = numeric_summary_table,
                  categorical_summary = categorical_summary,
                  correlation = correlation)

  # ---- figures --------------------------------------------------------------
  figures <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    figures <- list()

    # one histogram per numeric column, faceted by server (bin midpoints as x)
    hist_rows <- do.call(rbind, lapply(server_names, function(srv) {
      hgs <- stats_result[[srv]]$numeric_histograms
      do.call(rbind, lapply(names(hgs), function(vn) {
        h <- hgs[[vn]]
        mids <- head(h$breaks, -1) + diff(h$breaks) / 2
        data.frame(server = srv, variable = vn, mid = mids, count = as.numeric(h$counts),
                   stringsAsFactors = FALSE)
      }))
    }))
    if (!is.null(hist_rows) && nrow(hist_rows) > 0) {
      figures$histograms <- lapply(split(hist_rows, hist_rows$variable), function(sub) {
        ggplot2::ggplot(sub, ggplot2::aes(x = mid, y = count)) +
          ggplot2::geom_col() +
          ggplot2::facet_wrap(~server) +
          ggplot2::labs(title = paste0("Distribution: ", unique(sub$variable)),
                        x = NULL, y = "count (bins < nfilter suppressed)") +
          ggplot2::theme_minimal()
      })
    }

    # one bar chart per categorical variable, faceted by server
    if (length(categorical_summary) > 0) {
      figures$categorical_counts <- lapply(names(categorical_summary), function(vn) {
        tab <- categorical_summary[[vn]]
        if (is.null(tab) || nrow(tab) == 0) return(NULL)
        ggplot2::ggplot(tab, ggplot2::aes(x = category, y = count)) +
          ggplot2::geom_col() +
          ggplot2::facet_wrap(~server) +
          ggplot2::labs(title = paste0(vn, ": counts by category and server"),
                        x = NULL, y = "count (cells < nfilter suppressed)") +
          ggplot2::theme_minimal()
      })
      names(figures$categorical_counts) <- names(categorical_summary)
    }

    # correlation heatmap, one per server
    if (length(correlation) > 0) {
      figures$correlation_heatmap <- lapply(names(correlation), function(srv) {
        ggplot2::ggplot(correlation[[srv]], ggplot2::aes(x = var1, y = var2, fill = correlation)) +
          ggplot2::geom_tile() +
          ggplot2::scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                                        midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
          ggplot2::labs(title = paste0("Correlation matrix: ", srv), x = NULL, y = NULL) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      })
      names(figures$correlation_heatmap) <- names(correlation)
    }

    # group-wise trend, one line per server, faceted by numeric variable
    trend_rows <- do.call(rbind, lapply(server_names, function(srv) {
      gt <- stats_result[[srv]]$group_trend
      if (is.null(gt)) return(NULL)
      do.call(rbind, lapply(names(gt$means), function(vn) {
        data.frame(server = srv, variable = vn, level = gt$levels,
                   mean = as.numeric(gt$means[[vn]]), stringsAsFactors = FALSE)
      }))
    }))
    if (!is.null(trend_rows) && nrow(trend_rows) > 0) {
      figures$group_trend <- ggplot2::ggplot(
          trend_rows, ggplot2::aes(x = level, y = mean, color = server, group = server)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::facet_wrap(~variable, scales = "free_y") +
        ggplot2::labs(title = paste0("Mean by ", group_col), x = group_col, y = "mean") +
        ggplot2::theme_minimal()
    }
  } else {
    message("ggplot2 not installed -- returning report tables only, no figures.")
  }

  message("Exploratory analysis complete: ", length(unique(numeric_summary_table$variable)),
          " numeric variable(s), ", length(categorical_summary), " categorical variable(s).")

  list(object = df, stats = stats_result, report = report, figures = figures)
}
