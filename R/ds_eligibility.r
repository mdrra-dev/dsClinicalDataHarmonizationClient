#' @title Apply the study's agreed eligibility criteria (Section 2.1)
#' @description Diagnosis >= 2006, age at diagnosis >= 18, >=1 visit at/after
#'   2010, >=24 months follow-up span among those visits. Reports eligibility
#'   by cohort (derived from the \code{pat_ID} prefix) since selection may
#'   differ by centre and must be documented per cohort before pooling --
#'   see \code{apply_eligibility_criteriaDS} for the exact derivation.
#' @export
ds.apply_eligibility_criteria <- function(df, pat_id_col = "pat_ID",
                                           visit_col = "Visit_months_from_diagnosis",
                                           year_diagnosis_col = "Year_diagnosis",
                                           age_diagnosis_col = "Age_diagnosis",
                                           min_year_diagnosis = 2006, min_age = 18,
                                           followup_cutoff_year = 2010,
                                           min_followup_months_after_cutoff = 24,
                                           min_visits_after_cutoff = 1,
                                           nfilter = 5, newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_eligible")
  
  #TODO: use the toSerialize for prefix_regex
  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("apply_eligibility_criteriaDS", as.symbol(df), pat_id_col, visit_col,
                year_diagnosis_col, age_diagnosis_col, min_year_diagnosis, min_age,
                followup_cutoff_year, min_followup_months_after_cutoff,
                min_visits_after_cutoff, nfilter=nfilter, newobj=newobj))
  # results <- DSI::datashield.aggregate(conns = datasources,
  #   expr = call("apply_eligibility_criteriaDS", as.symbol(df), pat_id_col, visit_col,
  #               year_diagnosis_col, age_diagnosis_col, min_year_diagnosis, min_age,
  #               followup_cutoff_year, min_followup_months_after_cutoff,
  #               min_visits_after_cutoff, "^[A-Za-z]+", nfilter, newobj))

  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": ", r$n_patients_eligible, " of ", r$n_patients_before,
            " patients eligible (", r$n_rows_after, " of ", r$n_rows_before, " rows)")
    for (co in names(r$by_cohort)) {
      b <- r$by_cohort[[co]]
      message("  cohort ", co, ": ", b$n_eligible, "/", b$n_patients,
              " (", round(100 * b$proportion, 1), "%) eligible")
    }
  }
  message("Eligible-only dataset saved as: '", newobj, "'")
  invisible(newobj)
}

#' @title Empirical CDF of per-patient follow-up duration
#' @description Disclosure-safe survival-curve-style summary (proportion of
#'   patients with follow-up >= each grid point), for judging how many
#'   patients meet a duration threshold (default 24 months). Never returns
#'   a per-patient value.
#' @export
ds.followup_duration_cdf <- function(df, pat_id_col = "pat_ID",
                                      visit_col = "Visit_months_from_diagnosis",
                                      group_col = "cohort", num_points = 20, min_months = 24,
                                      nfilter = 5, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("followup_duration_cdfDS", as.symbol(df), pat_id_col, visit_col,
                group_col, num_points, min_months, nfilter))

  for (s in names(results)) {
    r <- results[[s]]
    for (g in names(r$groups)) {
      gr <- r$groups[[g]]
      message("Server ", s, ", group '", g, "': n=", gr$n_patients, ", ",
              round(100 * gr$prop_meeting_min, 1), "% meet the ", min_months, "-month minimum")
    }
  }

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    curve_rows <- do.call(rbind, lapply(names(results), function(s) {
      grid <- results[[s]]$grid
      do.call(rbind, lapply(names(results[[s]]$groups), function(g) {
        data.frame(server = s, group = g, months = grid,
                   proportion = results[[s]]$groups[[g]]$survival_curve, stringsAsFactors = FALSE)
      }))
    }))
    fig <- ggplot2::ggplot(curve_rows, ggplot2::aes(x = months, y = proportion, color = group)) +
      ggplot2::geom_step() + ggplot2::facet_wrap(~server) +
      ggplot2::geom_vline(xintercept = min_months, linetype = "dashed") +
      ggplot2::labs(title = paste0("Follow-up duration: proportion of patients with >= x months"),
                    x = "months", y = "proportion") +
      ggplot2::theme_minimal()
    #return(invisible(list(raw = results, plot = fig)))
    return( list(raw = results, plot = fig))
  }

  #invisible(list(raw = results, plot = NULL))
   list(raw = results, plot = NULL)
}
