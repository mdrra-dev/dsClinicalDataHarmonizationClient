#' @title Check for residual missing values after imputation
#' @description Reports, per server, which columns (if any) still contain
#'   \code{NA} after an imputation step, and how many. Call this BEFORE
#'   deciding whether to use \code{ds.post_imputation_cleanup()}.
#'
#' Server function called: \code{check_residual_naDS}
#'
#' @param df Character string naming the (imputed) server-side data frame.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#' @return Invisible per-server list of named NA counts.
#' @export
ds.check_residual_na <- function(df, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("check_residual_naDS", as.symbol(df))
  )

  for (server in names(results)) {
    na_counts <- results[[server]]
    if (length(na_counts) == 0) {
      message("Server ", server, ": no residual missing values.")
    } else {
      message("Server ", server, ": residual NAs -- ",
              paste(names(na_counts), na_counts, sep = "=", collapse = ", "))
    }
  }

  invisible(results)
}

#' @title Drop rows still incomplete after imputation
#' @description Explicit, opt-in row-drop for whatever residual \code{NA}s
#'   remain after imputation. Run \code{ds.check_residual_na()} first to see
#'   the scale of what would be dropped. Calls the disclosure-safe server
#'   function \code{post_imputation_cleanupDS} via \code{datashield.aggregate}
#'   -- the cleaned data frame is created server-side as a side effect; only
#'   row counts come back.
#'
#' Server function called: \code{post_imputation_cleanupDS}
#'
#' @param df Character string naming the (imputed) server-side data frame.
#' @param newobj Name of the new server-side object. Defaults to
#'   \code{paste0(df, "_complete")}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#' @return Invisibly returns \code{newobj}.
#' @export
ds.post_imputation_cleanup <- function(df, newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_complete")

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("post_imputation_cleanupDS", as.symbol(df), newobj)
  )

  for (server in names(results)) {
    message("Server ", server, ": removed ", results[[server]]$n_removed, " of ",
            results[[server]]$n_before, " rows still incomplete after imputation")
  }

  message("Result saved as: '", newobj, "'")
  invisible(newobj)
}
