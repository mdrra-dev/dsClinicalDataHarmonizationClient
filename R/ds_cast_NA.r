#' @title Cast "NA" string into NA
#' @description Converts "NA" strings into proper NA values across a
#'   server-side data frame, and reports whether any replacements occurred.
#'   Calls the disclosure-safe server function \code{cast_NADS} via
#'   \code{datashield.aggregate} -- the transformed data frame is created
#'   server-side as a side effect (never returned to the client); only the
#'   \code{changed} flag comes back.
#'
#' Server function called: \code{cast_NADS}
#'
#' @param df A character string specifying the name of the server-side data frame.
#' @param modified_obj A character string specifying the name of the new server-side
#' object to store the modified data frame. If NULL, a name is generated automatically
#' by appending \code{"_cast_NA"} to \code{df}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#' after login. If the \code{datasources} argument is not specified the default set of
#' connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisibly returns the name of the newly created server-side object.
#' @export

ds.cast_NA <- function(df, modified_obj = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(modified_obj)) {
    modified_obj <- paste0(df, "_cast_NA")
  }

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("cast_NADS", as.symbol(df), modified_obj)
  )

  for (server in names(results)) {
    if (isTRUE(results[[server]]$changed)) {
      message("Server ", server, ": some 'NA' strings were converted to NA")
    } else {
      message("Server ", server, ": no 'NA' strings found (nothing changed)")
    }
  }

  invisible(modified_obj)
}
