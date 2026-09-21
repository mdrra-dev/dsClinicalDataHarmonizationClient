#' @title Build a patient-level survival table on the server
#' @export
ds.build_survival_table <- function(df, pat_id_col = "pat_ID", event_col = "D2T",
                                     time_col = "Visit_months_from_diagnosis",
                                     entry_time_col = NULL, group_col = NULL,
                                     covariate_cols = NULL, newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_survtable")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("build_survival_tableDS", as.symbol(df), pat_id_col, event_col, time_col,
                entry_time_col, group_col, covariate_cols, newobj))
  for (s in names(results)) message("Server ", s, ": ", results[[s]]$n_patients, " patients, ",
                                     results[[s]]$n_events, " events")
  invisible(newobj)
}
