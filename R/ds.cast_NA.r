#' @title Cast "NA" string into NA
#' @description Converts "NA" strings into proper NA values. Calls
#'   \code{cast_NADS} via \code{datashield.aggregate}; the object is created
#'   server-side via \code{base::assign(newobj, ..., envir=parent.frame())}
#'   as a side effect, disclosure-safe since only the small summary comes back.
#' @export
ds.cast_NA <- function(df, modified_obj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(modified_obj)) modified_obj <- paste0(df, "_cast_NA")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("cast_NADS", as.symbol(df), modified_obj))

  for (s in names(results)) {
    if (isTRUE(results[[s]]$changed)) message("Server ", s, ": some 'NA' strings converted to NA")
    else message("Server ", s, ": no 'NA' strings found (nothing changed)")
  }
  invisible(modified_obj)
}
