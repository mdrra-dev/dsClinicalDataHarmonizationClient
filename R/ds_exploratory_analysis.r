#' @title Non-disclosive exploratory analysis of a server-side data frame
#' @description Per-site and aggregated descriptive statistics, distributions,
#'   category frequencies, correlations, and group-wise trends, using the
#'   server function \code{exploratory_analysisDS}. Only aggregate statistics
#'   ever return -- see \code{exploratory_analysisDS} for the exact
#'   disclosure-control convention (nfilter-suppressed cells/bins).
#'
#'   Usually called via \code{ds.harmonization_explore()} rather than
#'   directly -- that wrapper defaults to the cleaned dataset and builds a
#'   full figure set. Use this function directly for finer control over
#'   which columns/grouping to summarize.
#'
#' Server function called: \code{exploratory_analysisDS}
#'
#' @param df Character string naming the server-side data frame.
#' @param numeric_cols,categorical_cols Character vectors restricting which
#'   columns to summarize. \code{NULL} (default): every numeric / every
#'   non-numeric column respectively.
#' @param group_col Optional column for group-wise numeric means (e.g. a
#'   visit number, for a longitudinal trend).
#' @param num_bins Number of histogram bins per numeric column. Default 20.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return Invisible per-server list, the raw \code{exploratory_analysisDS} output.
#' @export
ds.exploratory_analysis <- function(df, numeric_cols = NULL, categorical_cols = NULL,
                                     group_col = NULL, num_bins = 20
                                     datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr = call("exploratory_analysisDS", as.symbol(df), numeric_cols, categorical_cols,
                group_col, num_bins)
  )

  for (server in names(results)) {
    r <- results[[server]]
    message("Server ", server, ": n = ", r$n, ", ",
            length(r$numeric_summary), " numeric column(s), ",
            length(r$categorical_summary), " categorical column(s) summarized")
  }

  invisible(results)
}
