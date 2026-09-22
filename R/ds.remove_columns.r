#' @title Remove specified columns from a server-side data frame
#' @export
ds.remove_columns <- function(df, col_names, newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_filtered")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("remove_columnsDS", as.symbol(df), paste(col_names, collapse = "$"), newobj))

  for (s in names(results)) message("Server ", s, ": removed columns -- ",
    paste(results[[s]]$columns_removed, collapse = ", "),
    " (", results[[s]]$n_cols_before, " -> ", results[[s]]$n_cols_after, " columns)")
  message("Object modified saved as: '", newobj, "'")
  invisible(newobj)
}
