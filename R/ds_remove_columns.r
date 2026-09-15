#' @title Remove specified columns from a server-side data frame
#' @description Remove one or more columns via the disclosure-safe server
#'   function \code{remove_columnsDS}, called through
#'   \code{datashield.aggregate} -- the modified data frame is created
#'   server-side as a side effect; only column names/counts come back.
#'
#' Server function called: \code{remove_columnsDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#' from which columns will be removed.
#' @param col_names A character vector specifying the names of the columns to remove.
#' @param newobj A character string specifying the name of the new server-side object
#' to store the modified data frame. If NULL, a name is generated automatically by
#' appending \code{"_filtered"} to \code{df}.
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}}
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisibly returns the name of the newly created server-side object.
#' @export
#'
ds.remove_columns <- function(df, col_names, newobj = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(newobj)) {
    newobj <- paste0(df, "_filtered")
  }

  collapsed_columns <- paste(col_names, collapse = "$")

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("remove_columnsDS", as.symbol(df), collapsed_columns, newobj)
  )

  for (server in names(results)) {
    message("Server ", server, ": removed columns -- ",
            paste(results[[server]]$columns_removed, collapse = ", "),
            " (", results[[server]]$n_cols_before, " -> ",
            results[[server]]$n_cols_after, " columns)")
  }
  message("Object modified saved as: '", newobj, "'")

  invisible(newobj)
}
