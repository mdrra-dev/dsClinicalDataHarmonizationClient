#' @title Harmonization orchestrator (diagnose / clean / diagnoseclean)
#' @description Thin dispatcher over the two standalone building blocks,
#'   \code{ds.harmonization_diagnose()} and \code{ds.harmonization_clean()}.
#'   In every mode, a report (tables) and, if \code{ggplot2} is installed,
#'   figures are returned -- nothing here ever returns row-level data, only
#'   the aggregate statistics each sub-function already produces.
#'
#' @param mode One of:
#'   \itemize{
#'     \item \code{"diagnose"}: runs \code{ds.harmonization_diagnose(df, ...)}
#'       only. Nothing is modified or assigned server-side.
#'     \item \code{"clean"}: runs \code{ds.harmonization_clean(df, ...)} only
#'       -- cleans, assigns \code{final_newobj}, and reports/plots the
#'       CLEANED data alone (no comparison to the raw input, since it was
#'       never diagnosed in this mode).
#'     \item \code{"diagnoseclean"} (default): diagnoses \code{df} (before),
#'       cleans it into \code{final_newobj}, diagnoses that (after), and
#'       returns a combined before/after report + figures.
#'   }
#' @param df Character string naming the raw server-side input data frame.
#' @param required_vars,optional_vars,pat_id_col,visit_col,zero_prop_threshold,
#'   spike_ratio_threshold,nfilter See \code{ds.harmonization_diagnose()}.
#' @param missing_threshold,missing_type,missing_action,dummy_cols,compute_derived,
#'   force_numeric_cols,force_categorical_cols,imputation_method,post_imputation_drop,
#'   final_newobj See \code{ds.harmonization_clean()}. Ignored when \code{mode = "diagnose"}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return
#'   \itemize{
#'     \item \code{mode = "diagnose"}: the return value of
#'       \code{ds.harmonization_diagnose()} (\code{object}, \code{diagnosis},
#'       \code{report}, \code{figures}).
#'     \item \code{mode = "clean"}: the return value of
#'       \code{ds.harmonization_clean()} (\code{final_object},
#'       \code{cleaning_log}, \code{diagnosis}, \code{report}, \code{figures}).
#'     \item \code{mode = "diagnoseclean"}: a list with \code{final_object},
#'       \code{before}, \code{after} (raw diagnosis snapshots),
#'       \code{cleaning_log}, and a before/after \code{report}/\code{figures}.
#'   }
#' @export
ds.harmonization_orchestrator <- function(df,
                                           mode = c("diagnoseclean", "diagnose", "clean"),
                                           required_vars = NULL, optional_vars = NULL,
                                           pat_id_col = "pat_ID",
                                           visit_col = "Visit_months_from_diagnosis",
                                           missing_threshold = 20,
                                           missing_type = c("combined", "split"),
                                           missing_action = c("drop_column", "drop_rows"),
                                           dummy_cols = c("csDMARD1", "csDMARD2", "csDMARD3",
                                                          "bDMARD", "tsDMARD", "GC_type", "GC"),
                                           compute_derived = TRUE,
                                           force_numeric_cols = NULL, force_categorical_cols = NULL,
                                           imputation_method = c("longitudinal", "global", "none"),
                                           post_imputation_drop = FALSE,
                                           zero_prop_threshold = 0.3, spike_ratio_threshold = 3,
                                           nfilter = 5,
                                           final_newobj = "Draw_cleaned",
                                           datasources = NULL) {

  mode <- match.arg(mode)
  if (is.null(datasources)) datasources <- datashield.connections_find()
  missing_type <- match.arg(missing_type)
  missing_action <- match.arg(missing_action)
  imputation_method <- match.arg(imputation_method)

  if (mode == "diagnose") {
    return(ds.harmonization_diagnose(
      df, required_vars = required_vars, optional_vars = optional_vars,
      pat_id_col = pat_id_col, visit_col = visit_col,
      zero_prop_threshold = zero_prop_threshold, spike_ratio_threshold = spike_ratio_threshold,
      nfilter = nfilter, datasources = datasources
    ))
  }

  if (mode == "clean") {
    return(ds.harmonization_clean(
      df, missing_threshold = missing_threshold, missing_type = missing_type,
      missing_action = missing_action, dummy_cols = dummy_cols,
      compute_derived = compute_derived, force_numeric_cols = force_numeric_cols,
      force_categorical_cols = force_categorical_cols, imputation_method = imputation_method,
      post_imputation_drop = post_imputation_drop, pat_id_col = pat_id_col, visit_col = visit_col,
      nfilter = nfilter, final_newobj = final_newobj, run_post_diagnosis = TRUE,
      required_vars = required_vars, optional_vars = optional_vars,
      zero_prop_threshold = zero_prop_threshold, spike_ratio_threshold = spike_ratio_threshold,
      datasources = datasources
    ))
  }

  # mode == "diagnoseclean"
  message("=== STEP 1: DIAGNOSIS (raw data) ===")
  before_result <- ds.harmonization_diagnose(
    df, required_vars = required_vars, optional_vars = optional_vars,
    pat_id_col = pat_id_col, visit_col = visit_col,
    zero_prop_threshold = zero_prop_threshold, spike_ratio_threshold = spike_ratio_threshold,
    nfilter = nfilter, datasources = datasources
  )

  message("\n=== STEP 2: DATA CLEANING ===")
  clean_result <- ds.harmonization_clean(
    df, missing_threshold = missing_threshold, missing_type = missing_type,
    missing_action = missing_action, dummy_cols = dummy_cols,
    compute_derived = compute_derived, force_numeric_cols = force_numeric_cols,
    force_categorical_cols = force_categorical_cols, imputation_method = imputation_method,
    post_imputation_drop = post_imputation_drop, pat_id_col = pat_id_col, visit_col = visit_col,
    nfilter = nfilter, final_newobj = final_newobj, run_post_diagnosis = FALSE,
    datasources = datasources
  )

  message("\n=== STEP 1 (repeat): DIAGNOSIS (cleaned data) ===")
  after_result <- ds.harmonization_diagnose(
    final_newobj, required_vars = required_vars, optional_vars = optional_vars,
    pat_id_col = pat_id_col, visit_col = visit_col,
    zero_prop_threshold = zero_prop_threshold, spike_ratio_threshold = spike_ratio_threshold,
    nfilter = nfilter, datasources = datasources
  )

  built <- .cdh_before_after_report(before_result$diagnosis, after_result$diagnosis)

  cfb <- built$report$conformity$aggregated
  message("\n=== SUMMARY ===")
  message("Final cleaned object: '", final_newobj, "'")
  message("Missing required variables remaining (aggregated, after): ",
          cfb$n_missing_required_vars[cfb$phase == "after"])
  message("Non-conforming columns remaining (aggregated, after): ",
          cfb$n_invalid_numeric[cfb$phase == "after"] + cfb$n_invalid_categorical[cfb$phase == "after"])

  list(
    final_object = final_newobj,
    before = before_result$diagnosis,
    after = after_result$diagnosis,
    cleaning_log = clean_result$cleaning_log,
    report = built$report,
    figures = built$figures
  )
}
