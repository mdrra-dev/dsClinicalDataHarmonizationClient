#' @title Global imputation of a server-side object using MICE
#' @description Performs pooled (not patient-grouped) imputation via the
#'   disclosure-safe server function \code{global_imputationDS}, called
#'   through \code{datashield.aggregate} -- the imputed data frame is
#'   created server-side as a side effect; only row/column counts and the
#'   per-column imputation method names (never data) come back.
#'
#' Server function called: \code{global_imputationDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#' on which the global imputation is performed.
#' @param id_col A character string indicating the name of the identifier column that
#' is excluded from imputation.
#' @param m A numeric value representing the number of imputations to generate.
#' Default is 5.
#' @param maxit A numeric value representing the maximum number of MICE iterations per imputation chain.
#' Default is 30.
#' @param imputed_newobj A character string specifying the name of the new
#' imputed object to be created on each server. If NULL, a name is generated
#' automatically by appending \code{"_imputed"} to \code{df}.
#' @param datasources A list of DataSHIELD connections. If NULL, the active
#' connections returned by \code{datashield.connections_find()} are used.
#'
#' @return Invisibly returns the name of the newly created server-side object.
#' @export
#'
ds.global_imputation <- function(df, id_col = NULL, m = 5, maxit = 30,
                                 imputed_newobj = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(imputed_newobj)) {
    imputed_newobj <- paste0(df, "_imputed")
  }

  message("Applying global imputation on each server...")

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("global_imputationDS", as.symbol(df), id_col, m, maxit, imputed_newobj)
  )

  for (server in names(results)) {
    r <- results[[server]]
    message("Server ", server, ": imputed ", r$n_rows, " rows x ", r$n_cols,
            " columns (methods: ",
            paste(names(r$method_by_column)[nzchar(r$method_by_column)],
                  r$method_by_column[nzchar(r$method_by_column)],
                  sep = "=", collapse = ", "), ")")
  }
  message("Preprocessing completed. Result saved as: '", imputed_newobj, "'")

  invisible(imputed_newobj)
}
