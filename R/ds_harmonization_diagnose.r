#' @title Harmonization diagnosis (standalone)
#' @description Runs the non-disclosive harmonization diagnosis
#'   (\code{harmonization_diagnosisDS}: variable presence, "NA"-string
#'   contamination, numeric/categorical range conformity, missingness \%,
#'   patient-visit duplicates, zero-inflation anomalies) on a single
#'   server-side object, and returns a report + \code{ggplot2} figures.
#'   Usable standalone, and used internally by \code{ds.harmonization_clean()}
#'   (post-cleaning snapshot) and \code{ds.harmonization_orchestrator()}
#'   (before AND after snapshots).
#'
#' @param df Character string naming the server-side object to diagnose.
#' @param required_vars,optional_vars Character vectors. \code{NULL}
#'   (default) fetches the package's canonical dictionary from the first
#'   connected server.
#' @param pat_id_col,visit_col Patient id / visit columns used for the
#'   duplicate check.
#' @param zero_prop_threshold,spike_ratio_threshold See \code{detect_zero_anomaliesDS}.
#' @param nfilter Disclosure/stability floor.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{object}: \code{df} (echoed back for convenience).
#'     \item \code{diagnosis}: raw per-server \code{harmonization_diagnosisDS} output.
#'     \item \code{report}: list with \code{missingness}, \code{conformity},
#'       \code{zero_anomalies}, \code{duplicates} -- each with per-site (and,
#'       for missingness/conformity, an aggregated) data.frame.
#'     \item \code{figures}: named list of ggplot objects, or \code{NULL} if
#'       ggplot2 isn't installed.
#'   }
#' @export
ds.harmonization_diagnose <- function(df,
                                       required_vars = NULL, optional_vars = NULL,
                                       pat_id_col = "pat_ID",
                                       visit_col = "Visit",
                                       zero_prop_threshold = 0.3, spike_ratio_threshold = 3,
                                       nfilter = 5, datasources = NULL) {

  if (is.null(datasources)) datasources <- datashield.connections_find()

  message("Diagnosing '", df, "'...")
  diag <- .cdh_run_diagnosis(df, required_vars, optional_vars, pat_id_col, visit_col,
                              nfilter, zero_prop_threshold, spike_ratio_threshold, datasources)

  built <- .cdh_single_report(diag)

  cf <- built$report$conformity$aggregated
  message("Diagnosis complete: ", cf$n_missing_required_vars, " missing required variable(s), ",
          cf$n_invalid_numeric + cf$n_invalid_categorical, " non-conforming column(s) (aggregated).")

  list(object = df, diagnosis = diag, report = built$report, figures = built$figures)
}
