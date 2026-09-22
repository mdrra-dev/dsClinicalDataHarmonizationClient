#' @title Check whether columns would change type if conversion were forced (dry run)
#' @export
ds.check_type_conversion <- function(df, numeric_cols = NULL, categorical_cols = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("detect_type_mismatchesDS", as.symbol(df), numeric_cols, categorical_cols))

  for (s in names(results)) {
    num <- results[[s]]$numeric_would_change; cat_ <- results[[s]]$categorical_would_change
    if (length(num) == 0 && length(cat_) == 0) message("Server ", s, ": no type mismatches for the requested columns.")
    else {
      if (length(num) > 0) message("Server ", s, ": would convert to numeric: ", paste(num, collapse = ", "))
      if (length(cat_) > 0) message("Server ", s, ": would convert to factor: ", paste(cat_, collapse = ", "))
    }
  }
  invisible(results)
}

#' @title Force type conversion on a server-side data frame
#' @export
ds.force_convert_types <- function(df, numeric_cols = NULL, categorical_cols = NULL,
                                    newobj = NULL, check_zero_anomalies = TRUE, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_typed")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("force_convert_typesDS", as.symbol(df), numeric_cols, categorical_cols, newobj))

  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": converted to numeric: ",
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
      message("Server ", s, ": '", cn, "' flagged for suspicious zero-inflation -- ", reason)
    }
  }
  message("Forced type conversion applied. Result saved as: '", newobj, "'")
  invisible(newobj)
}
