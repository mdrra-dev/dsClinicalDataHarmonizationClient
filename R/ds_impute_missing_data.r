#' @title Longitudinal (patient-grouped) MICE imputation -- preferred method
#' @export
ds.impute_missing_data <- function(df, pat_id_col = "pat_ID", visit_col = "Visit",
                                    exclude_cols = NULL, m = 5, maxit = 30, nfilter = 5,
                                    imputed_newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(imputed_newobj)) imputed_newobj <- paste0(df, "_imputed")

  message("Running longitudinal (patient-grouped, PMM) imputation on each server...")
  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("longitudinal_imputationDS", as.symbol(df), pat_id_col, visit_col,
                exclude_cols, m, maxit, nfilter, imputed_newobj))

  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": imputed ", r$n_rows, " rows across ", r$n_patients,
            " patients (", r$n_patients_unimputed, " left unimputed -- fewer than 2 visits)")
  }
  message("Imputation completed. Result saved as: '", imputed_newobj, "'")
  message("Tip: run ds.check_residual_na('", imputed_newobj, "') to see whether any rows still have missing values.")
  invisible(imputed_newobj)
}
