#' @title Global imputation of a server-side object using MICE
#' @export
ds.global_imputation <- function(df, id_col = NULL, m = 5, maxit = 30,
                                 imputed_newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(imputed_newobj)) imputed_newobj <- paste0(df, "_imputed")

  message("Applying global imputation on each server...")
  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("global_imputationDS", as.symbol(df), id_col, m, maxit, imputed_newobj))

  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": imputed ", r$n_rows, " rows x ", r$n_cols, " columns (methods: ",
      paste(names(r$method_by_column)[nzchar(r$method_by_column)],
            r$method_by_column[nzchar(r$method_by_column)], sep = "=", collapse = ", "), ")")
  }
  message("Preprocessing completed. Result saved as: '", imputed_newobj, "'")
  invisible(imputed_newobj)
}
