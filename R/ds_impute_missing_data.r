#' @title Longitudinal (patient-grouped) MICE imputation -- preferred method
#' @description Client wrapper for the disclosure-safe server function
#'   \code{longitudinal_imputationDS}, the preferred imputation strategy for
#'   this harmonization module: patient-grouped, visit-ordered, predictive
#'   mean matching. Called through \code{datashield.aggregate} -- the imputed
#'   data frame is created server-side as a side effect; only row/patient
#'   counts (never data) come back. Prefer this over
#'   \code{ds.global_imputation()} whenever the data are longitudinal.
#'
#' Server function called: \code{longitudinal_imputationDS}
#'
#' @param df Character string naming the server-side data frame.
#' @param pat_id_col Name of the patient identifier column. Default \code{"pat_ID"}.
#' @param visit_col Name of the visit-ordering column. Default \code{"Visit"}.
#' @param exclude_cols Character vector of columns to exclude from the MICE
#'   predictor matrix.
#' @param m Number of imputations per patient. Default 5.
#' @param maxit Maximum MICE iterations per chain. Default 30.
#' @param nfilter Minimum site row count required to proceed at all. Default 5.
#' @param imputed_newobj Name of the new server-side object. Defaults to
#'   \code{paste0(df, "_imputed")}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return Invisibly returns \code{imputed_newobj}.
#' @export
ds.impute_missing_data <- function(df, pat_id_col = "pat_ID", visit_col = "Visit",
                                    exclude_cols = NULL, m = 5, maxit = 30, nfilter = 5,
                                    imputed_newobj = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(imputed_newobj)) {
    imputed_newobj <- paste0(df, "_imputed")
  }

  message("Running longitudinal (patient-grouped, PMM) imputation on each server...")

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("longitudinal_imputationDS", as.symbol(df), pat_id_col, visit_col,
                 exclude_cols, m, maxit, nfilter, imputed_newobj)
  )

  for (server in names(results)) {
    r <- results[[server]]
    message("Server ", server, ": imputed ", r$n_rows, " rows across ",
            r$n_patients, " patients (", r$n_patients_unimputed,
            " left unimputed -- fewer than 2 visits)")
  }

  message("Imputation completed. Result saved as: '", imputed_newobj, "'")
  message("Tip: run ds.check_residual_na('", imputed_newobj, "') to see whether ",
          "any rows still have missing values.")

  invisible(imputed_newobj)
}
