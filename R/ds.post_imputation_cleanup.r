#' @title Check for residual missing values after imputation
#' @export
ds.check_residual_na <- function(df, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  results <- DSI::datashield.aggregate(conns = datasources, expr = call("check_residual_naDS", as.symbol(df)))
  for (s in names(results)) {
    na_counts <- results[[s]]
    if (length(na_counts) == 0) message("Server ", s, ": no residual missing values.")
    else message("Server ", s, ": residual NAs -- ", paste(names(na_counts), na_counts, sep = "=", collapse = ", "))
  }
  invisible(results)
}

#' @title Drop rows still incomplete after imputation
#' @export
ds.post_imputation_cleanup <- function(df, newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_complete")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("post_imputation_cleanupDS", as.symbol(df), newobj))

  for (s in names(results)) message("Server ", s, ": removed ", results[[s]]$n_removed, " of ",
    results[[s]]$n_before, " rows still incomplete after imputation")
  message("Result saved as: '", newobj, "'")
  invisible(newobj)
}
