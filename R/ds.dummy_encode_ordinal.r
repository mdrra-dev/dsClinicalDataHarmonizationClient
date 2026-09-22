#' @title Dummy-encode ordinal DMARD-style variables on a server-side data frame
#' @export
ds.dummy_encode_ordinal <- function(df,
                                     columns = c("csDMARD1", "csDMARD2", "csDMARD3",
                                                 "bDMARD", "tsDMARD", "GC_type", "GC"),
                                     newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_dummy")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("dummy_encode_ordinalDS", as.symbol(df), paste(columns, collapse = "$"), newobj))

  for (s in names(results)) {
    added <- results[[s]]$added; skipped <- results[[s]]$skipped_excluded
    message("Server ", s, ": added dummy columns: ", if (length(added)) paste(added, collapse = ", ") else "none")
    if (length(skipped) > 0) message("Server ", s, ": skipped (excluded by design): ", paste(skipped, collapse = ", "))
  }
  message("Result saved as: '", newobj, "'")
  invisible(newobj)
}
