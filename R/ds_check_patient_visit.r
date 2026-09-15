#' @title Check patient-visit combinations on a server-side data frame
#' @description Reports whether duplicate (patient, visit) combinations
#'   exist, using the disclosure-safe \code{check_patient_visitDS} server-side
#'   function. Only aggregate counts are ever returned or printed here --
#'   an earlier version of this function printed the actual duplicated
#'   patient IDs to the console (via the server returning the full
#'   per-patient-visit table), which was itself a disclosure issue
#'   independent of how it was called; that table is no longer computed
#'   client-side at all.
#'
#' Server function called: \code{check_patient_visitDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#'   containing at least the patient identifier and visit columns.
#' @param pat_id_col Character. Name of the patient identifier column.
#'   Default \code{"pat_ID"}.
#' @param visit_col Character. Name of the visit column. Default
#'   \code{"Visit_months_from_diagnosis"}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#'   after login. If the \code{datasources} argument is not specified, the default set of
#'   connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisible per-server list of counts
#'   (\code{n_unique_patients}, \code{n_unique_pairs}, \code{n_duplicate_pairs},
#'   \code{max_occurrences}). Messages report the same, per server.
#' @export

ds.check_patient_visit <- function(df, pat_id_col = "pat_ID",
                                    visit_col = "Visit",
                                    datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("check_patient_visitDS", df = as.symbol(df),
                 pat_id_col = pat_id_col, visit_col = visit_col)
  )

  for (server in names(results)) {
    r <- results[[server]]
    if (r$n_duplicate_pairs > 0) {
      message("Server ", server, ": ", r$n_duplicate_pairs,
              " duplicate patient-visit pair(s) out of ", r$n_unique_pairs,
              " unique pairs (", r$n_unique_patients, " patients; max ",
              r$max_occurrences, " rows for a single pair)")
    } else {
      message("Server ", server, ": all patient-visit pairs are unique (",
              r$n_unique_patients, " patients, ", r$n_unique_pairs, " pairs)")
    }
  }

  invisible(results)
}
