#' @title Best-effort parser for ds.coxph.SLMA()'s output into a forest-plot table
#' @description WRITTEN FROM DOCUMENTED USAGE PATTERNS, NOT VERIFIED AGAINST
#'   A LIVE RUN. \code{dsSurvivalClient::ds.coxph.SLMA()} follows
#'   DataSHIELD's general "SLMA" (study-level meta-analysis) convention, the
#'   same one \code{ds.glmSLMA} uses: typically a
#'   \code{betamatrix.valid}/\code{sematrix.valid} pair of per-site
#'   coefficient/SE matrices, plus a \code{SLMA.pooled.ests.matrix} with the
#'   meta-analytic pooled estimate. This function tries those field names;
#'   if your installed \code{dsSurvivalClient} version names things
#'   differently, run \code{str(cox_result)} on the raw output
#'   (\code{result$cox_raw} from \code{ds.run_D2T_survival()}) and adjust
#'   the field names here accordingly -- this is exactly the kind of
#'   external-package detail that needs a live check, which isn't possible
#'   in this environment.
#' @return A data.frame (term, source, estimate, se, hr, ci_low, ci_high),
#'   or \code{NULL} if none of the expected fields were found.
#' @export
.cdh_parse_coxphSLMA <- function(cox_result) {
  rows <- list()

  if (!is.null(cox_result$betamatrix.valid) && !is.null(cox_result$sematrix.valid)) {
    beta <- cox_result$betamatrix.valid
    se <- cox_result$sematrix.valid
    for (term in rownames(beta)) {
      for (site in colnames(beta)) {
        est <- beta[term, site]; s <- se[term, site]
        if (!is.na(est) && !is.na(s)) {
          rows[[length(rows) + 1]] <- data.frame(
            term = term, source = site, estimate = est, se = s,
            hr = exp(est), ci_low = exp(est - 1.96 * s), ci_high = exp(est + 1.96 * s),
            stringsAsFactors = FALSE)
        }
      }
    }
  }

  pooled <- cox_result$SLMA.pooled.ests.matrix
  if (!is.null(pooled)) {
    est_col <- intersect(c("beta_SLMA", "pooled.ML", "Estimate"), colnames(pooled))[1]
    se_col <- intersect(c("se_SLMA", "SE"), colnames(pooled))[1]
    if (!is.na(est_col) && !is.na(se_col)) {
      for (term in rownames(pooled)) {
        est <- pooled[term, est_col]; s <- pooled[term, se_col]
        rows[[length(rows) + 1]] <- data.frame(
          term = term, source = "Pooled (SLMA)", estimate = est, se = s,
          hr = exp(est), ci_low = exp(est - 1.96 * s), ci_high = exp(est + 1.96 * s),
          stringsAsFactors = FALSE)
      }
    }
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

#' @title D2T Objective 6: survival analysis via dsSurvival/dsSurvivalClient
#' @description Kaplan-Meier curves, Cox proportional-hazards (study-level
#'   meta-analysis, i.e. federated across sites), and a hazard-ratio forest
#'   plot -- delegated entirely to \code{dsSurvivalClient}'s own validated
#'   implementation. This function's OWN job is exactly the data-prep step
#'   that makes that possible: reducing the visit-level \code{data_object}
#'   to a one-row-per-patient survival table (\code{ds.build_survival_table()}),
#'   nothing more. No survival statistic is computed by this package itself.
#'
#'   LEFT TRUNCATION (Objective 6's requirement for bDMARD-start cohorts,
#'   whose observation begins after an unobserved period since diagnosis):
#'   supply \code{entry_time_col} (that patient's time-since-diagnosis at
#'   bDMARD start); the survival table then carries an \code{entry} column
#'   and \code{ds.Surv(type = "counting")} is used, the standard
#'   counting-process form for delayed entry.
#'
#'   The Kaplan-Meier curve is drawn by \code{dsSurvivalClient::ds.plotsurvfit()},
#'   which is privacy-preserving via an internal LOESS/\code{fANCOVA}-smoothed
#'   curve (per the package's own design) -- there is no separate "fANCOVA
#'   plot" step to add; it's already part of that call.
#'
#'   REQUIRES \code{dsSurvivalClient} to be installed AND \code{dsSurvival}'s
#'   server methods (\code{SurvDS}, \code{survfitDS}, \code{coxphSLMADS},
#'   \code{plotsurvfitDS}, etc.) registered on the connection(s) -- see
#'   \code{run_D2T_exploratory_orchestrator.R} for the DSLite registration
#'   calls. If \code{dsSurvivalClient} isn't installed, this returns
#'   \code{NULL} with a message rather than approximating anything.
#'
#'   The forest-plot parsing step (\code{.cdh_parse_coxphSLMA}) is written
#'   from documented \code{ds.coxph.SLMA()} usage patterns and has NOT been
#'   verified against a live run -- if it can't parse your installed
#'   version's output shape, it says so and still returns the raw Cox
#'   result for manual inspection, rather than silently failing.
#'
#' @param data_object Character, the (cleaned, visit-level) server-side data
#'   frame. Default \code{"Draw_cleaned"}.
#' @param pat_id_col,event_col,time_col,entry_time_col,group_col,covariate_cols
#'   See \code{build_survival_tableDS}.
#' @param output_dir Base directory; results go under
#'   \code{file.path(output_dir, "D2T_survival")}.
#' @param nfilter Disclosure/stability floor (passed through where dsSurvival
#'   itself exposes one; the survival-table build itself has no additional
#'   suppression beyond what dsSurvival applies to KM/Cox outputs).
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return \code{NULL} (with a message) if \code{dsSurvivalClient} isn't
#'   installed. Otherwise a list with \code{survival_table} (object name),
#'   \code{cox_raw} (raw \code{ds.coxph.SLMA()} output), \code{hazard_ratios}
#'   (parsed forest-plot table, or \code{NULL} if parsing failed), \code{fig_dir}.
#' @export
ds.run_D2T_survival <- function(data_object = "Draw_cleaned",
                                 pat_id_col = "pat_ID", event_col = "D2T",
                                 time_col = "Visit_months_from_diagnosis",
                                 entry_time_col = NULL,
                                 group_col = "cohort",
                                 covariate_cols = NULL,
                                 output_dir = ".", nfilter = 5, datasources = NULL) {

  if (is.null(datasources)) datasources <- datashield.connections_find()

  if (!requireNamespace("dsSurvivalClient", quietly = TRUE)) {
    message("dsSurvivalClient not installed -- survival analysis (Objective 6) skipped, ",
            "not approximated. Install from ",
            "https://github.com/neelsoumya/dsSurvivalClient (and dsSurvival server-side, ",
            "https://github.com/neelsoumya/dsSurvival) to enable this.")
    return(NULL)
  }


  fig_dir <- .cdh_ensure_dir(file.path(output_dir, "D2T_survival"))

  message("Building patient-level survival table",
          if (!is.null(entry_time_col)) " (left-truncated, counting-process form)..." else " (right-censored)...")
  surv_obj_name <- ds.build_survival_table(data_object, pat_id_col = pat_id_col, event_col = event_col,
    time_col = time_col, entry_time_col = entry_time_col, group_col = group_col,
    covariate_cols = covariate_cols, datasources = datasources)

  surv_type <- if (!is.null(entry_time_col)) "counting" else "right"

  message("Creating survival object (ds.Surv, type='", surv_type, "')...")
  tryCatch({
    dsSurvivalClient::ds.Surv(
      time = if (surv_type == "counting") "entry" else "time",
      time2 = if (surv_type == "counting") "time" else NULL,
      event = "event", 
      objectname = "surv_S", 
      type = surv_type,
      #objectname = surv_obj_name,
      datasources = datasources)
  }, error = function(e) message("  ds.Surv() failed: ", conditionMessage(e),
                                  " -- check argument names against your installed dsSurvivalClient version."))


# dsSurvivalClient::ds.Surv(time='STARTTIME', time2='ENDTIME', 
#                       event = 'EVENT', objectname='surv_object',
#                       type='counting')
              
coxph_model_full <- dsSurvivalClient::ds.coxph.SLMA(formula = 'surv_S~D$age+D$female')

  covars_rhs <- if (!is.null(group_col)) group_col else "1"

  message("Kaplan-Meier curve(s) by ", covars_rhs, "...")
  km_plot <- tryCatch({
    dsSurvivalClient::ds.survfit(formula = paste0("surv_S ~ ", covars_rhs),
                                  objectname = "survfit_obj", datasources = datasources)
    dsSurvivalClient::ds.plotsurvfit(
      formula = paste0("surv_S ~ ", covars_rhs), objectname = surv_obj_name,
      datasources = datasources, xlab = paste0(time_col, " (months)"),
      ylab = paste0(event_col, "-free survival"), ggplot = TRUE)
  }, error = function(e) { message("  KM step failed: ", conditionMessage(e)); NULL })


  if (!is.null(km_plot) && inherits(km_plot, "ggplot")) {
    .cdh_save_plot(km_plot, fig_dir, "11_kaplan_meier_by_cohort.png")
  } else if (!is.null(km_plot)) {
    message("  ds.plotsurvfit() did not return a ggplot object directly -- ",
            "raw result kept in the return value ($km_plot_raw) for manual inspection.")
  }

  message("Cox proportional hazards model (study-level meta-analysis)...")
  cox_formula <- paste0("surv_S ~ ", paste(c(group_col, covariate_cols), collapse = " + "))
  cox_result <- tryCatch(
    dsSurvivalClient::ds.coxph.SLMA(formula = cox_formula, objectname = surv_obj_name, datasources = datasources),
    error = function(e) { message("  Cox model failed: ", conditionMessage(e)); NULL }
  )

  forest_df <- NULL
  if (!is.null(cox_result)) {
    forest_df <- tryCatch(.cdh_parse_coxphSLMA(cox_result), error = function(e) NULL)
    if (!is.null(forest_df)) {
      .cdh_save_csv(forest_df, fig_dir, "12_cox_hazard_ratios.csv")
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        fp <- ggplot2::ggplot(forest_df, ggplot2::aes(x = hr, y = stats::reorder(term, hr),
                                                        xmin = ci_low, xmax = ci_high, color = source)) +
          ggplot2::geom_pointrange() +
          ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
          ggplot2::scale_x_log10() +
          ggplot2::labs(title = "Hazard ratio forest plot (per site + pooled SLMA)",
                        x = "Hazard ratio (log scale)", y = NULL) +
          ggplot2::theme_minimal()
        .cdh_save_plot(fp, fig_dir, "13_hazard_ratio_forest_plot.png")
      }
    } else {
      message("  Could not auto-parse ds.coxph.SLMA()'s output shape into a forest-plot table ",
              "(see .cdh_parse_coxphSLMA()'s docstring) -- raw result returned as $cox_raw.")
    }
  }

  message("\nResults saved under: ", fig_dir)
  message("Left truncation note: entry_time_col must reflect each bDMARD-start patient's true ",
          "time-since-diagnosis at bDMARD start (per protocol Objective 6) for the counting-process ",
          "form to be valid -- this function does not derive or validate that value itself.")

  list(survival_table = surv_obj_name, cox_raw = cox_result, hazard_ratios = forest_df,
       km_plot_raw = km_plot, fig_dir = fig_dir)
}
