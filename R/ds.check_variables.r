#' @title Check variables on a server-side data frame
#' @description Verify that a server-side data frame contains every required
#'   variable, report which declared optional variables are present, and flag
#'   anything unexpected. Uses the server-side function \code{check_variablesDS}.
#'   Messages are printed for each server indicating discrepancies.
#'
#'   If \code{variables} is not supplied, this defaults to the RA harmonization
#'   protocol's canonical \code{required} variable list (fetched server-side
#'   from \code{clinical_variable_dictionaryDS()} on the first connected
#'   server, so the client never has to hardcode it), and likewise for
#'   \code{optional}.
#'
#' Server function called: \code{check_variablesDS}
#'
#' @param df A character string specifying the name of the server-side data frame
#'   to check.
#' @param variables A character vector specifying the expected REQUIRED variable
#'   names. NULL (default) uses the package's canonical required list.
#' @param optional A character vector specifying OPTIONAL variable names --
#'   these are never reported as "missing", only as present/absent for
#'   informational purposes, and are excluded from the "extra" list.
#'   NULL (default) uses the package's canonical optional list.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained
#'   after login. If the \code{datasources} argument is not specified, the default set of
#'   connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#'
#' @return Invisible list of results returned by the server-side function. Each element
#'   corresponds to a server and contains \code{missing}, \code{optional_present},
#'   and \code{extra}.
#' @export

ds.check_variables <- function(df, variables = NULL, optional = NULL, datasources = NULL) {

  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }

  if (is.null(variables) || is.null(optional)) {
    dict <- DSI::datashield.aggregate(conns = datasources[1],
                                       expr = call("clinical_variable_dictionaryDS"))[[1]]
    if (is.null(variables)) variables <- dict$required
    if (is.null(optional))  optional  <- dict$optional
  }

  collapsed_variables <- paste(variables, collapse = "$")
  collapsed_optional  <- paste(optional, collapse = "$")

  call <- call("check_variablesDS", df = as.symbol(df),
               variables_string = collapsed_variables,
               optional_string = collapsed_optional)

  results <- DSI::datashield.aggregate(conns = datasources, expr = call)

  for (server in names(results)) {

    missing  <- results[[server]]$missing
    present  <- results[[server]]$optional_present
    extra    <- results[[server]]$extra

    if (length(missing) == 0 && length(extra) == 0) {
      message("All required variables present in ", server,
              " (no unexpected columns).")
    } else {
      msg <- paste0("Server ", server, ": ")
      if (length(missing) > 0) {
        msg <- paste0(msg, "MISSING required variables: ", paste(missing, collapse = ", "))
      }
      if (length(extra) > 0) {
        if (length(missing) > 0) msg <- paste0(msg, "; ")
        msg <- paste0(msg, "Unexpected/extra variables: ", paste(extra, collapse = ", "))
      }
      message(msg)
    }

    if (length(present) > 0) {
      message("Server ", server, ": optional variables available: ",
              paste(present, collapse = ", "))
    }
  }

  invisible(results)
}
