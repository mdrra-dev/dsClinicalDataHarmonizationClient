#' @title Check whether columns would change type if conversion were forced
#' @description Dry-run report using \code{detect_type_mismatchesDS} -- use
#'   this after \code{ds.check_types()} flags a column, to see whether it's a
#'   simple representation issue (e.g. numeric stored as character) that
#'   \code{ds.force_convert_types()} could resolve, before actually forcing it.
#'   Never creates a server-side object; already a plain, disclosure-safe
#'   aggregate call.
#'
#' Server function called: \code{detect_type_mismatchesDS}
#'
#' @param df Character string naming the server-side data frame.
#' @param numeric_cols Character vector of columns expected to be numeric.
#' @param categorical_cols Character vector of columns expected to be a factor.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#' @return Invisible list of per-server results.
#' @export
ds.check_type_conversion <- function(df, numeric_cols = NULL, categorical_cols = NULL,
                                      datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()

  call <- call("detect_type_mismatchesDS", as.symbol(df), numeric_cols, categorical_cols)
  results <- DSI::datashield.aggregate(conns = datasources, expr = call)

  for (server in names(results)) {
    num <- results[[server]]$numeric_would_change
    cat_ <- results[[server]]$categorical_would_change
    if (length(num) == 0 && length(cat_) == 0) {
      message("Server ", server, ": no type mismatches for the requested columns.")
    } else {
      if (length(num) > 0)
        message("Server ", server, ": would convert to numeric: ", paste(num, collapse = ", "))
      if (length(cat_) > 0)
        message("Server ", server, ": would convert to factor: ", paste(cat_, collapse = ", "))
    }
  }

  invisible(results)
}

#' @title Force type conversion on a server-side data frame
#' @description Actually performs the conversion identified by
#'   \code{ds.check_type_conversion()}. This is an explicit, opt-in step --
#'   the harmonization pipeline never silently coerces types on its own.
#'   Calls the disclosure-safe server function \code{force_convert_typesDS}
#'   via \code{datashield.aggregate} -- the converted data frame is created
#'   server-side as a side effect; only column names and non-disclosive
#'   zero-anomaly diagnostics for the newly-converted numeric columns come
#'   back (see \code{detect_zero_anomaliesDS}).
#'
#' Server function called: \code{force_convert_typesDS}
#'
#' @param df Character string naming the server-side data frame.
#' @param numeric_cols Character vector of columns to force to numeric.
#' @param categorical_cols Character vector of columns to force to factor.
#' @param newobj Name of the new server-side object. Defaults to
#'   \code{paste0(df, "_typed")}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#' @return Invisibly returns \code{newobj}.
#' @export
ds.force_convert_types <- function(df, numeric_cols = NULL, categorical_cols = NULL,
                                    newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_typed")

  results <- DSI::datashield.aggregate(
    conns = datasources,
    expr  = call("force_convert_typesDS", as.symbol(df), numeric_cols, categorical_cols, newobj)
  )

  for (server in names(results)) {
    r <- results[[server]]
    message("Server ", server, ": converted to numeric: ",
            if (length(r$numeric_converted)) paste(r$numeric_converted, collapse = ", ") else "none",
            "; converted to factor: ",
            if (length(r$categorical_converted)) paste(r$categorical_converted, collapse = ", ") else "none")

    flagged <- Filter(function(x) isTRUE(x$flagged), r$zero_anomalies)
    for (cn in names(flagged)) {
      f <- flagged[[cn]]
      reason <- paste(c(
        if (isTRUE(f$prop_flag)) sprintf("%.0f%% of values are exactly 0", 100 * f$prop_zero),
        if (isTRUE(f$spike_flag)) sprintf("zero-count is %.1fx the local average bin count", f$spike_ratio)
      ), collapse = "; ")
      message("Server ", server, ": '", cn, "' flagged for suspicious zero-inflation -- ",
              reason, " -- worth checking whether this reflects a cast/import error.")
    }
  }

  message("Forced type conversion applied. Result saved as: '", newobj, "'")
  invisible(newobj)
}
