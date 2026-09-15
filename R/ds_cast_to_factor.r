#' @title Cast specified columns to factor on a server-side data frame
#' @description Convert one or more columns of a server-side data frame to
#'   factor, via the disclosure-safe server function \code{cast_to_factorDS},
#'   called through \code{datashield.aggregate} -- the modified data frame is
#'   created server-side as a side effect; only column names come back.
#'
#' Server function called: \code{cast_to_factorDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#' containing the columns to be converted.
#' @param columns A character vector specifying the names of the columns to convert
#' to factor.
#' @param modified_obj A character string specifying the name of the new server-side
#' object to store the modified data frame. If NULL, a name is generated automatically
#' by appending \code{"_modified"} to \code{df}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#' after login. If the \code{datasources} argument is not specified the default set of
#' connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisibly returns the name of the newly created server-side object.
#' @export
#'

ds.cast_to_factor <- function(df, columns, modified_obj = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(modified_obj)) {
    modified_obj <- paste0(df, "_modified")
  }

  collapsed_columns <- paste(columns, collapse = "$")

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("cast_to_factorDS", as.symbol(df), collapsed_columns, modified_obj)
  )

  for (server in names(results)) {
    message("Server ", server, ": cast to factor -- ",
            paste(results[[server]]$columns_cast, collapse = ", "))
  }
  message("Result saved as: '", modified_obj, "'")

  invisible(modified_obj)
}
