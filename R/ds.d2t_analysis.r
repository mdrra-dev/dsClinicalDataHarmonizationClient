#' @title Derive a cohort column from pat_ID prefix on a server-side object
#' @export
ds.add_cohort_column <- function(df, pat_id_col = "pat_ID", prefix_regex = "^[A-Za-z]+",
                                  newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_cohort")

  #TODO: prefix_regex should use toSerialize. 
  # results <- DSI::datashield.aggregate(conns = datasources,
  #   expr = call("add_cohort_columnDS", as.symbol(df), pat_id_col, prefix_regex, newobj))
  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("add_cohort_columnDS", as.symbol(df), pat_id_col,  newobj = newobj))
  for (s in names(results)) message("Server ", s, ": cohorts found -- ",
                                     paste(results[[s]]$cohorts_found, collapse = ", "))
  invisible(newobj)
}

#' @title Objective 1: baseline / D2T-visit descriptive table, by cohort
#' @export
ds.d2t_baseline_table <- function(df, numeric_vars, categorical_vars = NULL,
                                   group_col = "cohort", filter_col = NULL, filter_value = NULL,
                                   first_only = FALSE, pat_id_col = "pat_ID", visit_col = "Visit",
                                   nfilter = 5, max_categories_shown = 20, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  DSI::datashield.aggregate(conns = datasources,
    expr = call("d2t_baseline_tableDS", as.symbol(df), numeric_vars, categorical_vars,
                group_col, filter_col, filter_value, first_only, pat_id_col, visit_col,
                nfilter, max_categories_shown))
}

#' @title Objective 2: between-cohort heterogeneity (Kruskal-Wallis + chi-square)
#' @export
ds.d2t_between_cohort_test <- function(df, numeric_vars = NULL, categorical_vars = NULL,
                                        group_col = "cohort", nfilter = 5, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  out <- list()
  if (!is.null(numeric_vars))
    out$numeric <- DSI::datashield.aggregate(conns = datasources,
      expr = call("kruskal_by_groupDS", as.symbol(df), numeric_vars, group_col, nfilter))
  if (!is.null(categorical_vars))
    out$categorical <- DSI::datashield.aggregate(conns = datasources,
      expr = call("chisq_by_groupDS", as.symbol(df), categorical_vars, group_col, nfilter))
  out
}

#' @title Objective 3: follow-up duration heterogeneity by cohort
#' @export
ds.d2t_followup_stats <- function(df, pat_id_col = "pat_ID",
                                   visit_months_col = "Visit_months_from_diagnosis",
                                   group_col = "cohort", min_months = 24, nfilter = 5,
                                   datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  DSI::datashield.aggregate(conns = datasources,
    expr = call("d2t_followup_statsDS", as.symbol(df), pat_id_col, visit_months_col,
                group_col, min_months, nfilter))
}

#' @title Objective 4: inter-visit interval heterogeneity by cohort
#' @export
ds.d2t_visit_interval_stats <- function(df, pat_id_col = "pat_ID",
                                         visit_months_col = "Visit_months_from_diagnosis",
                                         group_col = "cohort", nfilter = 5, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  DSI::datashield.aggregate(conns = datasources,
    expr = call("d2t_visit_interval_statsDS", as.symbol(df), pat_id_col, visit_months_col,
                group_col, nfilter))
}

#' @title Objective 5: D2T incidence -- overall, by cohort, by diagnosis-year
#'   bin, by disease-duration bin (NOT time-to-event -- see Objective 6 note)
#' @export
ds.d2t_incidence <- function(df, event_col = "D2T", pat_id_col = "pat_ID",
                              visit_months_col = "Visit_months_from_diagnosis",
                              year_diagnosis_col = "Year_diagnosis", group_col = "cohort",
                              year_bin_width = 2, duration_bin_width = 12, nfilter = 5,
                              datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  DSI::datashield.aggregate(conns = datasources,
    expr = call("d2t_incidenceDS", as.symbol(df), event_col, pat_id_col, visit_months_col,
                year_diagnosis_col, group_col, year_bin_width, duration_bin_width, nfilter))
}
