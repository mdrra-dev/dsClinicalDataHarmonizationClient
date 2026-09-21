#' @title D2T study-protocol exploratory analysis (Objectives 1-5)
#' @description Runs the objectives from the D2T study protocol that map
#'   cleanly onto this package's existing generic aggregate infrastructure.
#'   Objective 6 (KM/Cox with left-truncation) is NOT implemented -- see the
#'   message this function prints, and \code{run_D2T_exploratory_orchestrator.R}.
#'   Objective 7 (confounders) is not a standalone analysis here; reuse
#'   \code{multivariable_modelDS}/\code{interaction_modelDS} from the
#'   label-analysis module with \code{cohort} as an added covariate.
#' @export
ds.run_D2T_exploratory <- function(data_object = "Draw_cleaned",
                                    pat_id_col = "pat_ID", visit_col = "Visit",
                                    visit_months_col = "Visit_months_from_diagnosis",
                                    year_diagnosis_col = "Year_diagnosis",
                                    event_col = "D2T",
                                    baseline_numeric_vars = c("Age_diagnosis", "DAS28", "DAS28_CRP",
                                      "SJC28", "TJC28", "CRP", "ESR", "Pat_global", "Pain", "Ph_global",
                                      "HAQ", "eq5d", "GC_dose"),
                                    baseline_categorical_vars = c("Sex", "RF_positivity", "anti_CCP",
                                      "csDMARD1", "csDMARD2", "csDMARD3", "bDMARD", "tsDMARD", "GC"),
                                    min_followup_months = 24,
                                    output_dir = ".", nfilter = 5, datasources = NULL) {

  if (is.null(datasources)) datasources <- datashield.connections_find()
  fig_dir <- .cdh_ensure_dir(file.path(output_dir, "D2T_exploratory"))

  cohort_obj <- ds.add_cohort_column(data_object, pat_id_col = pat_id_col, datasources = datasources)

  avail <- unique(unlist(DSI::datashield.aggregate(
    conns = datasources, expr = call("list_columnsDS", as.symbol(cohort_obj)))))
  num_vars <- intersect(baseline_numeric_vars, avail)
  cat_vars <- intersect(baseline_categorical_vars, avail)
  results <- list()

  message("Objective 1: baseline & D2T-visit characterization by cohort...")
  results$baseline <- ds.d2t_baseline_table(cohort_obj, num_vars, cat_vars, group_col = "cohort",
    first_only = TRUE, pat_id_col = pat_id_col, visit_col = visit_col, nfilter = nfilter,
    datasources = datasources)
  if (event_col %in% avail) {
    results$d2t_visit <- ds.d2t_baseline_table(cohort_obj, num_vars, cat_vars, group_col = "cohort",
      filter_col = event_col, filter_value = 1, pat_id_col = pat_id_col, visit_col = visit_col,
      nfilter = nfilter, datasources = datasources)
  } else {
    message("  (D2T-visit table skipped: '", event_col, "' not available)")
  }

  message("Objective 2: between-cohort heterogeneity...")
  results$between_cohort <- ds.d2t_between_cohort_test(cohort_obj, num_vars, cat_vars, "cohort",
                                                          nfilter, datasources)
  .cdh_save_csv(.d2t_test_table(results$between_cohort$numeric), fig_dir, "03_between_cohort_numeric_kruskal.csv")
  .cdh_save_csv(.d2t_test_table(results$between_cohort$categorical), fig_dir, "03_between_cohort_categorical_chisq.csv")

  message("Objective 3: follow-up duration heterogeneity...")
  results$followup <- ds.d2t_followup_stats(cohort_obj, pat_id_col, visit_months_col, "cohort",
                                             min_followup_months, nfilter, datasources)
  #.cdh_save_csv(.d2t_named_list_to_df(results$followup, "cohort"), fig_dir, "04_followup_duration_by_cohort.csv")

  followup_df <- .d2t_named_list_to_df(
  results$followup,
  "cohort"
)

if (!is.null(followup_df) && nrow(followup_df) > 0L) {
  .cdh_save_csv(
    followup_df,
    fig_dir,
    "04_followup_duration_by_cohort.csv"
  )
} else {
  message(
    "  Follow-up summary produced no releasable cohort-level results."
  )
}
  # if (requireNamespace("ggplot2", quietly = TRUE) && length(results$followup) > 0) {
  #   .cdh_save_plot(.cdh_plot_summary_boxplot(results$followup, "Follow-up duration by cohort", "months"),
  #                  fig_dir, "04_followup_duration_by_cohort.png")
  # }

  if (requireNamespace("ggplot2", quietly = TRUE) &&
    length(results$followup) > 0) {

  followup_plot <- .cdh_plot_summary_boxplot(
    results$followup,
    "Follow-up duration by cohort",
    "months"
  )

  if (!is.null(followup_plot)) {
    .cdh_save_plot(
      followup_plot,
      fig_dir,
      "04_followup_duration_by_cohort.png"
    )
  }
}

  message("Objective 4: visit-interval heterogeneity...")
  results$visit_interval <- ds.d2t_visit_interval_stats(cohort_obj, pat_id_col, visit_months_col,
                                                         "cohort", nfilter, datasources)
  .cdh_save_csv(.d2t_named_list_to_df(results$visit_interval, "cohort"), fig_dir, "05_visit_interval_by_cohort.csv")

  if (event_col %in% avail) {
    message("Objective 5: D2T incidence (cumulative, by cohort, by diagnosis-year, by disease duration)...")
    results$incidence <- ds.d2t_incidence(cohort_obj, event_col, pat_id_col, visit_months_col,
      year_diagnosis_col, "cohort", nfilter = nfilter, datasources = datasources)
    .cdh_save_csv(.d2t_named_list_to_df(results$incidence$by_cohort, "cohort"), fig_dir, "06_incidence_by_cohort.csv")
    .cdh_save_csv(.d2t_named_list_to_df(results$incidence$by_year_bin, "year_bin"), fig_dir, "06_incidence_by_year_bin.csv")
    if (!is.null(results$incidence$by_duration_bin)) {
      dur_df <- data.frame(duration_bin_start = names(results$incidence$by_duration_bin),
                            n_onset = unlist(results$incidence$by_duration_bin), stringsAsFactors = FALSE)
      .cdh_save_csv(dur_df, fig_dir, "06_incidence_by_duration_bin.csv")
    }
  } else {
    message("Objective 5 skipped: '", event_col, "' not available.")
  }

  message("\nObjective 6 (Kaplan-Meier + Cox with left-truncation for bDMARD-entry ",
          "cohorts) is NOT implemented. Correct federated survival analysis requires ",
          "sharing per-event-time risk sets across sites with its own disclosure control ",
          "(the problem dedicated packages like dsSurvival/ds.coxSLMA exist to solve); a ",
          "from-scratch version using only this package's simple aggregate primitives would ",
          "not match a standard non-federated KM/Cox implementation's accuracy, so per your ",
          "instruction this is flagged rather than approximated.")
  message("Objective 7 (confounders) is not a standalone analysis here: reuse ",
          "multivariable_modelDS()/interaction_modelDS() from the label-analysis module ",
          "with 'cohort' (and entry-point type, once available) as added covariates.")

  message("\nResults saved under: ", fig_dir)
  results
}

# #' @export
# .d2t_named_list_to_df <- function(lst, group_name) {
#   if (is.null(lst) || length(lst) == 0) return(NULL)
#   do.call(rbind, lapply(names(lst), function(g) {
#     row <- as.data.frame(lst[[g]][!vapply(lst[[g]], is.list, logical(1))], stringsAsFactors = FALSE)
#     row[[group_name]] <- g
#     row
#   }))
# }

#' @export
.d2t_named_list_to_df <- function(lst, group_name = "cohort") {

  if (is.null(lst) || length(lst) == 0L) {
    return(NULL)
  }

  ## ------------------------------------------------------------
  ## Case 1: DataSHIELD result is nested by server/site
  ## ------------------------------------------------------------
  out <- do.call(rbind, lapply(names(lst), function(site) {

    site_res <- lst[[site]]

    if (is.null(site_res) || length(site_res) == 0L) {
      return(NULL)
    }

    ## site_res should normally be named by cohort
    if (is.list(site_res) && !is.null(names(site_res))) {

      site_df <- do.call(rbind, lapply(names(site_res), function(g) {

        x <- site_res[[g]]

        if (is.null(x) || length(x) == 0L) {
          return(NULL)
        }

        ## Remove nested/list elements
        keep <- !vapply(x, is.list, logical(1))

        if (!any(keep)) {
          return(NULL)
        }

        row <- as.data.frame(
          x[keep],
          stringsAsFactors = FALSE
        )

        if (nrow(row) == 0L) {
          return(NULL)
        }

        row[[group_name]] <- g
        row$server <- site

        row
      }))

      return(site_df)
    }

    NULL
  }))

  if (is.null(out) || nrow(out) == 0L) {
    return(NULL)
  }

  rownames(out) <- NULL
  out
}

#' @export
.d2t_test_table <- function(test_list) {
  if (is.null(test_list) || length(test_list) == 0) return(NULL)
  vars <- names(test_list)
  df <- data.frame(
    variable = vars,
    statistic = vapply(vars, function(v) test_list[[v]]$statistic %||% NA_real_, numeric(1)),
    p_value = vapply(vars, function(v) test_list[[v]]$p_value %||% NA_real_, numeric(1)),
    n = vapply(vars, function(v) test_list[[v]]$n_total %||% NA_integer_, numeric(1)),
    stringsAsFactors = FALSE
  )
  df$fdr <- stats::p.adjust(df$p_value, method = "BH")
  df
}
