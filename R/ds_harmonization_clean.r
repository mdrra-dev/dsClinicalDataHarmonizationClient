#' @title Harmonization data cleaning (standalone)
#' @description Runs the data-cleaning pipeline in this fixed order (each
#'   step is one of this package's existing functions; a step is skipped,
#'   not silently altered, if its argument is \code{NULL}/\code{FALSE}):
#'   \enumerate{
#'     \item \code{ds.cast_NA()} -- normalize literal "NA" strings.
#'     \item \code{ds.check_missing_data(..., action = missing_action)} --
#'       drop columns or rows exceeding \code{missing_threshold}.
#'     \item \code{ds.force_convert_types()} (only if \code{force_numeric_cols}
#'       or \code{force_categorical_cols} supplied).
#'     \item \code{ds.compute_derived_variables()} (USPDGS, DAS2C).
#'     \item \code{ds.dummy_encode_ordinal()} (DMARD family by default).
#'     \item \code{ds.impute_missing_data()} (longitudinal, default) or
#'       \code{ds.global_imputation()}, or skipped if \code{imputation_method = "none"}.
#'     \item \code{ds.post_imputation_cleanup()} (only if \code{post_imputation_drop = TRUE}).
#'   }
#'   The final result is assigned, on every server, to \code{final_newobj}
#'   (default \code{"Draw_cleaned"}). If \code{run_post_diagnosis = TRUE}
#'   (default), \code{ds.harmonization_diagnose()} is then run on the cleaned
#'   object and its report/figures are included in the return value -- set
#'   this to \code{FALSE} when the caller (e.g.
#'   \code{ds.harmonization_orchestrator(mode = "diagnoseclean")}) is going
#'   to build its own before/after comparison instead.
#'
#' @param df Character string naming the raw server-side input data frame.
#' @param missing_threshold Numeric, \% missing above which a variable is flagged.
#' @param missing_type \code{"combined"} or \code{"split"} -- see \code{ds.check_missing_data}.
#' @param missing_action \code{"drop_column"} or \code{"drop_rows"}.
#' @param dummy_cols Columns to ordinal-dummy-encode. \code{NULL} to skip.
#' @param compute_derived Logical, run \code{ds.compute_derived_variables()}.
#' @param force_numeric_cols,force_categorical_cols Columns to force-convert.
#'   \code{NULL} (default): step skipped.
#' @param imputation_method \code{"longitudinal"} (default), \code{"global"}, or \code{"none"}.
#' @param post_imputation_drop Logical, drop rows still incomplete after imputation.
#' @param pat_id_col,visit_col Used for longitudinal imputation grouping/ordering,
#'   and (if \code{run_post_diagnosis}) the post-cleaning duplicate check.
#' @param nfilter Disclosure/stability floor used throughout.
#' @param final_newobj Name of the final cleaned object. Default \code{"Draw_cleaned"}.
#' @param run_post_diagnosis Logical, run \code{ds.harmonization_diagnose()} on
#'   the cleaned result and include its report/figures. Default \code{TRUE}.
#' @param required_vars,optional_vars,zero_prop_threshold,spike_ratio_threshold
#'   Passed through to the post-cleaning diagnosis, if run.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects.
#'
#' @return A list:
#'   \itemize{
#'     \item \code{final_object}: \code{final_newobj}.
#'     \item \code{cleaning_log}: named list, one entry per pipeline step actually run.
#'     \item \code{diagnosis}, \code{report}, \code{figures}: present only if
#'       \code{run_post_diagnosis = TRUE} (else \code{NULL}).
#'   }
#' @export
ds.harmonization_clean <- function(df,
                                    missing_threshold = 20,
                                    missing_type = c("combined", "split"),
                                    missing_action = c("drop_column", "drop_rows"),
                                    dummy_cols = c("csDMARD1", "csDMARD2", "csDMARD3",
                                                   "bDMARD", "tsDMARD", "GC_type"),
                                    compute_derived = TRUE,
                                    force_numeric_cols = NULL, force_categorical_cols = NULL,
                                    imputation_method = c("longitudinal", "global", "none"),
                                    post_imputation_drop = FALSE,
                                    pat_id_col = "pat_ID",
                                    visit_col = "Visit",
                                    nfilter = 5,
                                    final_newobj = "Draw_cleaned",
                                    run_post_diagnosis = TRUE,
                                    required_vars = NULL, optional_vars = NULL,
                                    zero_prop_threshold = 0.3, spike_ratio_threshold = 3,
                                    datasources = NULL) {

  if (is.null(datasources)) datasources <- datashield.connections_find()
  missing_type <- match.arg(missing_type)
  missing_action <- match.arg(missing_action)
  imputation_method <- match.arg(imputation_method)

  message("=== DATA CLEANING: '", df, "' -> '", final_newobj, "' ===")
  current <- df
  cleaning_log <- list()

  message("Step 1/7: normalizing 'NA' strings...")
  current <- ds.cast_NA(current, datasources = datasources)
  cleaning_log$cast_na <- current

  message("Step 2/7: handling missing data (threshold = ", missing_threshold,
          "%, action = ", missing_action, ")...")
  mm_result <- ds.check_missing_data(current, threshold = missing_threshold,
                                      type = missing_type, remove = TRUE,
                                      action = missing_action, datasources = datasources)
  if (!is.null(mm_result)) current <- mm_result
  cleaning_log$missing_data <- current

  if (!is.null(force_numeric_cols) || !is.null(force_categorical_cols)) {
    message("Step 3/7: forcing type conversion on requested columns...")
    current <- ds.force_convert_types(current, numeric_cols = force_numeric_cols,
                                       categorical_cols = force_categorical_cols,
                                       datasources = datasources)
  } else {
    message("Step 3/7: skipped (no force_numeric_cols/force_categorical_cols given).")
  }
  cleaning_log$force_convert <- current

  if (isTRUE(compute_derived)) {
    message("Step 4/7: computing derived variables (USPDGS, DAS2C)...")
    current <- ds.compute_derived_variables(current, datasources = datasources)
  } else {
    message("Step 4/7: skipped.")
  }
  cleaning_log$derived_variables <- current

  if (!is.null(dummy_cols)) {
    message("Step 5/7: dummy-encoding ordinal variables (", paste(dummy_cols, collapse = ", "), ")...")
    current <- ds.dummy_encode_ordinal(current, columns = dummy_cols, datasources = datasources)
  } else {
    message("Step 5/7: skipped.")
  }
  cleaning_log$dummy_encoding <- current

  if (imputation_method == "longitudinal") {
    message("Step 6/7: longitudinal (patient-grouped PMM) imputation...")
    current <- ds.impute_missing_data(current, pat_id_col = pat_id_col, visit_col = visit_col,
                                       datasources = datasources)
  } else if (imputation_method == "global") {
    message("Step 6/7: global (pooled) MICE imputation...")
    current <- ds.global_imputation(current, id_col = pat_id_col, datasources = datasources)
  } else {
    message("Step 6/7: skipped (imputation_method = 'none').")
  }
  cleaning_log$imputation <- current

  if (isTRUE(post_imputation_drop) && imputation_method != "none") {
    message("Step 7/7: dropping rows still incomplete after imputation...")
    current <- ds.post_imputation_cleanup(current, datasources = datasources)
  } else {
    message("Step 7/7: skipped.")
  }
  cleaning_log$post_imputation_cleanup <- current

  DSI::datashield.assign.expr(
    conns = datasources, symbol = final_newobj, expr = as.symbol(current)
  )
  message("Cleaned dataset assigned as: '", final_newobj, "'")

  out <- list(final_object = final_newobj, cleaning_log = cleaning_log,
              diagnosis = NULL, report = NULL, figures = NULL)

  if (isTRUE(run_post_diagnosis)) {
    message("\nRunning post-cleaning diagnosis on '", final_newobj, "'...")
    diag_result <- ds.harmonization_diagnose(
      final_newobj, required_vars = required_vars, optional_vars = optional_vars,
      pat_id_col = pat_id_col, visit_col = visit_col,
      zero_prop_threshold = zero_prop_threshold, spike_ratio_threshold = spike_ratio_threshold,
      nfilter = nfilter, datasources = datasources
    )
    out$diagnosis <- diag_result$diagnosis
    out$report <- diag_result$report
    out$figures <- diag_result$figures
  }

  out
}
