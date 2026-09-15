#' @title Check variable types on a server-side data frame
#' @description Verify that numeric and categorical variables in a server-side data frame
#'   comply with predefined valid ranges and values. Uses the server-side functions
#'   \code{check_numericDS} and \code{check_categoricalDS} to identify non-compliant columns.
#'   Messages are printed for each server indicating which columns, if any, are invalid.
#'
#' Server functions called: \code{check_numericDS}, \code{check_categoricalDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#'   containing numeric and categorical variables to be checked.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#'   after login. If the \code{datasources} argument is not specified, the default set of
#'   connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisible \code{NULL}. Messages are printed to indicate which columns,
#'   if any, are non-compliant on each server.
#' @export

ds.check_types <- function(df, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  call_categorical <- call("check_categoricalDS", df = as.symbol(df))
  call_numerical <- call("check_numericDS", df = as.symbol(df))

  results_categorical <- DSI::datashield.aggregate(conns = datasources, expr = call_categorical)
  results_numerical <- DSI::datashield.aggregate(conns = datasources, expr = call_numerical)

  for (server in names(results_numerical)) {

    invalid_cols <- c(results_numerical[[server]], results_categorical[[server]])

    message(paste0("Server: ", server))

    if (length(invalid_cols) > 0) {
      message("Warning: The following columns are not compliant:")
      message(paste(invalid_cols, collapse = ", "), "\n")

    } else {
      message("All column values are valid\n")
    }
  }

  invisible(NULL)
}
