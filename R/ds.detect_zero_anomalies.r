#' @title Detect suspicious zero-inflation (possible erroneous numeric cast)
#' @description Flags numeric columns where zeros look like a cast artefact
#'   rather than genuine clinical zeros -- see \code{detect_zero_anomaliesDS}
#'   for the two heuristics used. Run this after any type coercion step
#'   (\code{ds.force_convert_types}, \code{ds.cast_NA}, imputation) and,
#'   generally, as part of routine data-quality review.
#'
#' Server function called: \code{detect_zero_anomaliesDS}
#'
#' @param df Character string naming the server-side data frame.
#' @param cols Character vector restricting the check to specific columns.
#'   \code{NULL} (default): every numeric column.
#' @param zero_prop_threshold,spike_ratio_threshold,num_bins,nfilter See
#'   \code{detect_zero_anomaliesDS}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return Invisible per-server list of per-column zero-anomaly stats.
#' @export
ds.detect_zero_anomalies <- function(df, cols = NULL, zero_prop_threshold = 0.3,
                                      spike_ratio_threshold = 3, num_bins = 10,
                                      nfilter = 5, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("detect_zero_anomaliesDS", as.symbol(df), cols,
                 zero_prop_threshold, spike_ratio_threshold, num_bins, nfilter)
  )

  for (server in names(results)) {
    flagged <- Filter(function(x) isTRUE(x$flagged), results[[server]])
    if (length(flagged) == 0) {
      message("Server ", server, ": no suspicious zero-inflation detected.")
    } else {
      for (cn in names(flagged)) {
        f <- flagged[[cn]]
        reason <- paste(c(
          if (isTRUE(f$prop_flag)) sprintf("%.0f%% of values are exactly 0", 100 * f$prop_zero),
          if (isTRUE(f$spike_flag)) sprintf("zero-count is %.1fx the local average bin count", f$spike_ratio)
        ), collapse = "; ")
        message("Server ", server, ": '", cn, "' flagged -- ", reason,
                " -- worth checking whether this reflects a cast/import error rather than true zeros.")
      }
    }
  }

  invisible(results)
}
