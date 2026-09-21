#' @title Fill missing visit data on a server-side data frame
#' @export
ds.fill_missing_data <- function(df, pat_id_col, visit_col, value_col,
                                 filled_newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(filled_newobj)) filled_newobj <- paste0(df, "_filled")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("fill_missing_dataDS", as.symbol(df), pat_id_col, visit_col, value_col, filled_newobj))

  for (s in names(results)) message("Server ", s, ": filled ", results[[s]]$n_filled,
    " previously-missing '", value_col, "' values across ", results[[s]]$n_rows, " rows")
  message("Result saved as: '", filled_newobj, "'")
  invisible(filled_newobj)
}
