#' @title Longitudinal (patient-grouped) MICE imputation -- preferred method
#' @export
ds.impute_missing_data <- function(df, pat_id_col = "pat_ID", visit_col = "Visit",
                                    include_cols = NULL, exclude_cols = NULL,  m = 5, maxit = 30, min_visits = 3, 
                                    nfilter = 5, imputed_newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(imputed_newobj)) imputed_newobj <- paste0(df, "_imputed")
  if (!is.null(exclude_cols)) exclude_cols <- paste(exclude_cols, collapse = "$")
  if (!is.null(include_cols)) include_cols <- paste(include_cols, collapse = "$")
  message("Running longitudinal (patient-grouped, PMM) imputation on each server...")

  results <- DSI::datashield.aggregate(conns = datasources,
  expr = call("longitudinal_imputationDS", df = as.symbol(df),
              pat_id_col = pat_id_col, visit_col = visit_col,
              include_cols = include_cols, exclude_cols = exclude_cols,
              m = m, maxit = maxit, min_visits = min_visits,
              nfilter = nfilter, newobj = imputed_newobj))

  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": imputed ", r$n_rows, " rows across ", r$n_patients,
            " patients (", r$n_patients_unimputed, " left unimputed -- fewer than 2 visits)")
  }
  message("Imputation completed. Result saved as: '", imputed_newobj, "'")
  message("Tip: run ds.check_residual_na('", imputed_newobj, "') to see whether any rows still have missing values.")
  invisible(imputed_newobj)
}


#' @title missForest imputation (random-forest, mixed types, keeps all rows)
#' @export
ds.impute_missforest <- function(df, exclude_cols = NULL, maxiter = 10, ntree = 100,
                                 nfilter = 5, seed = NULL, imputed_newobj = NULL,
                                 datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(imputed_newobj)) imputed_newobj <- paste0(df, "_imputed_rf")
  if (!is.null(exclude_cols)) exclude_cols <- paste(exclude_cols, collapse = "$")
  message("Running missForest imputation on each server...")
  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("missforest_imputationDS", as.symbol(df), exclude_cols, maxiter,
                ntree, nfilter, seed, imputed_newobj))

  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": ", r$n_rows, " rows kept; ", r$n_cols_imputed,
            " columns imputed (", r$n_cols_skipped, " skipped); missing cells ",
            r$n_missing_before, " -> ", r$n_missing_after,
            if (!is.na(r$oob_error[1])) paste0("; OOB error ", signif(r$oob_error[1], 3)) else ""
            )
   
  }
  message("Imputation completed. Result saved as: '", imputed_newobj, "'")
  message("Tip: run ds.check_residual_na('", imputed_newobj, "') to see whether any rows still have missing values.")
  invisible(imputed_newobj)
}