#' @title Cast specified columns to factor on a server-side data frame
#' @export
ds.cast_to_factor <- function(df, columns, modified_obj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(modified_obj)) modified_obj <- paste0(df, "_modified")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("cast_to_factorDS", as.symbol(df), paste(columns, collapse = "$"), modified_obj))

  for (s in names(results)) message("Server ", s, ": cast to factor -- ",
                                     paste(results[[s]]$columns_cast, collapse = ", "))
  message("Result saved as: '", modified_obj, "'")
  invisible(modified_obj)
}
