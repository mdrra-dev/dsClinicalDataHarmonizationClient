#' @title Compute derived clinical variables (USPDGS, DAS2C, CDAI50, SDAI)
#' @export
ds.compute_derived_variables <- function(df, newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_derived")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("compute_derived_variablesDS", as.symbol(df), newobj))

  for (s in names(results)) {
    computed <- results[[s]]$computed
    if (length(computed) == 0) message("Server ", s, ": no derived variables computed (missing source columns).")
    else message("Server ", s, ": computed ", paste(computed, collapse = ", "))
  }
  message("Result saved as: '", newobj, "'")
  invisible(newobj)
}
