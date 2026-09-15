#' @title Run the consolidated server-side diagnosis on one object
#' @description Internal helper shared by \code{ds.harmonization_diagnose()},
#'   \code{ds.harmonization_clean()}, and \code{ds.harmonization_orchestrator()}
#'   so the three functions can never diverge in how a diagnosis snapshot is
#'   taken. Resolves the variable dictionary (fetched from the first server
#'   if not supplied) and calls \code{harmonization_diagnosisDS} via
#'   \code{datashield.aggregate}.
#' @keywords internal
.cdh_run_diagnosis <- function(obj_name, required_vars, optional_vars,
                                pat_id_col, visit_col, nfilter,
                                zero_prop_threshold, spike_ratio_threshold,
                                datasources) {

  if (is.null(required_vars) || is.null(optional_vars)) {
    dict <- DSI::datashield.aggregate(conns = datasources[1],
                                       expr = call("clinical_variable_dictionaryDS"))[[1]]
    if (is.null(required_vars)) required_vars <- dict$required
    if (is.null(optional_vars)) optional_vars <- dict$optional
  }

  diag <- DSI::datashield.aggregate(
    conns = datasources,
    expr = call("harmonization_diagnosisDS", as.symbol(obj_name),
                paste(required_vars, collapse = "$"),
                paste(optional_vars, collapse = "$"),
                pat_id_col, visit_col, nfilter,
                zero_prop_threshold, spike_ratio_threshold)
  )
  attr(diag, "required_vars") <- required_vars
  attr(diag, "optional_vars") <- optional_vars
  diag
}

#' @title Build a single-snapshot (non before/after) diagnosis report
#' @description Internal helper used by \code{ds.harmonization_diagnose()}
#'   (standalone) and \code{ds.harmonization_clean()} (post-cleaning
#'   snapshot, when a before/after comparison isn't being assembled).
#' @keywords internal
.cdh_single_report <- function(diag) {
  server_names <- names(diag)

  missingness_site <- do.call(rbind, lapply(server_names, function(srv) {
    mp <- diag[[srv]]$missing_pct
    data.frame(server = srv, variable = names(mp), missing_pct = as.numeric(mp),
               row.names = NULL, stringsAsFactors = FALSE)
  }))

  n_by_site <- vapply(diag, `[[`, numeric(1), "n")
  all_vars <- unique(unlist(lapply(diag, function(d) names(d$missing_pct))))
  wg <- vapply(all_vars, function(v) {
    vals <- sapply(server_names, function(srv) diag[[srv]]$missing_pct[v])
    w <- n_by_site
    sum(vals * w, na.rm = TRUE) / sum(w[!is.na(vals)])
  }, numeric(1))
  missingness_aggregated <- data.frame(variable = names(wg), missing_pct = wg,
                                        row.names = NULL, stringsAsFactors = FALSE)

  conformity_site <- do.call(rbind, lapply(server_names, function(srv) {
    d <- diag[[srv]]
    data.frame(server = srv,
               n_invalid_numeric = length(d$invalid_numeric),
               n_invalid_categorical = length(d$invalid_categorical),
               n_missing_required_vars = length(d$missing_required),
               na_string_present = d$na_string_present,
               stringsAsFactors = FALSE)
  }))
  conformity_aggregated <- data.frame(
    n_invalid_numeric = sum(conformity_site$n_invalid_numeric),
    n_invalid_categorical = sum(conformity_site$n_invalid_categorical),
    n_missing_required_vars = sum(conformity_site$n_missing_required_vars),
    any_na_string_present = any(conformity_site$na_string_present),
    stringsAsFactors = FALSE
  )

  zero_anomalies_site <- do.call(rbind, lapply(server_names, function(srv) {
    flagged <- Filter(function(x) isTRUE(x$flagged), diag[[srv]]$zero_anomalies)
    data.frame(server = srv, n_flagged = length(flagged),
               flagged_vars = paste(names(flagged), collapse = ", "),
               stringsAsFactors = FALSE)
  }))

  duplicates_site <- do.call(rbind, lapply(server_names, function(srv) {
    data.frame(server = srv,
               duplicate_patient_visit_pairs = diag[[srv]]$duplicate_patient_visit_pairs,
               stringsAsFactors = FALSE)
  }))

  report <- list(
    missingness = list(per_site = missingness_site, aggregated = missingness_aggregated),
    conformity = list(per_site = conformity_site, aggregated = conformity_aggregated),
    zero_anomalies = list(per_site = zero_anomalies_site),
    duplicates = list(per_site = duplicates_site)
  )

  figures <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    figures <- list()
    figures$missingness <- ggplot2::ggplot(
        missingness_site, ggplot2::aes(x = variable, y = missing_pct)) +
      ggplot2::geom_col() + ggplot2::facet_wrap(~server) + ggplot2::coord_flip() +
      ggplot2::labs(title = "Missingness by variable and server", x = NULL, y = "% missing") +
      ggplot2::theme_minimal()

    figures$missingness_aggregated <- ggplot2::ggplot(
        missingness_aggregated, ggplot2::aes(x = variable, y = missing_pct)) +
      ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::labs(title = "Missingness (aggregated, site-weighted)", x = NULL, y = "% missing") +
      ggplot2::theme_minimal()

    figures$conformity <- ggplot2::ggplot(
        conformity_site,
        ggplot2::aes(x = server, y = n_invalid_numeric + n_invalid_categorical)) +
      ggplot2::geom_col() +
      ggplot2::labs(title = "Non-conforming columns per site", x = NULL,
                    y = "# invalid numeric + categorical columns") +
      ggplot2::theme_minimal()

    figures$zero_anomalies <- ggplot2::ggplot(
        zero_anomalies_site, ggplot2::aes(x = server, y = n_flagged)) +
      ggplot2::geom_col() +
      ggplot2::labs(title = "Variables flagged for suspicious zero-inflation", x = NULL,
                    y = "# flagged variables") +
      ggplot2::theme_minimal()
  } else {
    message("ggplot2 not installed -- returning report tables only, no figures.")
  }

  list(report = report, figures = figures)
}

#' @title Build a before/after diagnosis report
#' @description Internal helper used by \code{ds.harmonization_orchestrator()}
#'   in \code{mode = "diagnoseclean"}.
#' @keywords internal
.cdh_before_after_report <- function(before, after) {
  server_names <- names(before)
  n_by_site <- vapply(before, `[[`, numeric(1), "n")

  .missingness_df <- function(diag_list, phase) {
    do.call(rbind, lapply(server_names, function(srv) {
      mp <- diag_list[[srv]]$missing_pct
      data.frame(phase = phase, server = srv, variable = names(mp),
                 missing_pct = as.numeric(mp), row.names = NULL, stringsAsFactors = FALSE)
    }))
  }
  missingness_site <- rbind(.missingness_df(before, "before"), .missingness_df(after, "after"))

  .weighted_global <- function(diag_list) {
    all_vars <- unique(unlist(lapply(diag_list, function(d) names(d$missing_pct))))
    vapply(all_vars, function(v) {
      vals <- sapply(server_names, function(srv) diag_list[[srv]]$missing_pct[v])
      w <- n_by_site
      sum(vals * w, na.rm = TRUE) / sum(w[!is.na(vals)])
    }, numeric(1))
  }
  wg_before <- .weighted_global(before)
  wg_after  <- .weighted_global(after)
  missingness_aggregated <- data.frame(
    variable = c(names(wg_before), names(wg_after)),
    phase = c(rep("before", length(wg_before)), rep("after", length(wg_after))),
    missing_pct = c(wg_before, wg_after),
    row.names = NULL, stringsAsFactors = FALSE
  )

  .conformity_df <- function(diag_list, phase) {
    do.call(rbind, lapply(server_names, function(srv) {
      d <- diag_list[[srv]]
      data.frame(phase = phase, server = srv,
                 n_invalid_numeric = length(d$invalid_numeric),
                 n_invalid_categorical = length(d$invalid_categorical),
                 n_missing_required_vars = length(d$missing_required),
                 na_string_present = d$na_string_present,
                 stringsAsFactors = FALSE)
    }))
  }
  conformity_site <- rbind(.conformity_df(before, "before"), .conformity_df(after, "after"))
  conformity_aggregated <- do.call(rbind, lapply(c("before", "after"), function(ph) {
    sub <- conformity_site[conformity_site$phase == ph, ]
    data.frame(phase = ph,
               n_invalid_numeric = sum(sub$n_invalid_numeric),
               n_invalid_categorical = sum(sub$n_invalid_categorical),
               n_missing_required_vars = sum(sub$n_missing_required_vars),
               any_na_string_present = any(sub$na_string_present),
               stringsAsFactors = FALSE)
  }))

  .zero_anom_df <- function(diag_list, phase) {
    do.call(rbind, lapply(server_names, function(srv) {
      flagged <- Filter(function(x) isTRUE(x$flagged), diag_list[[srv]]$zero_anomalies)
      data.frame(phase = phase, server = srv, n_flagged = length(flagged),
                 flagged_vars = paste(names(flagged), collapse = ", "),
                 stringsAsFactors = FALSE)
    }))
  }
  zero_anomalies_site <- rbind(.zero_anom_df(before, "before"), .zero_anom_df(after, "after"))

  duplicates_site <- do.call(rbind, lapply(c("before", "after"), function(ph) {
    diag_list <- if (ph == "before") before else after
    do.call(rbind, lapply(server_names, function(srv) {
      data.frame(phase = ph, server = srv,
                 duplicate_patient_visit_pairs = diag_list[[srv]]$duplicate_patient_visit_pairs,
                 stringsAsFactors = FALSE)
    }))
  }))

  report <- list(
    missingness = list(per_site = missingness_site, aggregated = missingness_aggregated),
    conformity = list(per_site = conformity_site, aggregated = conformity_aggregated),
    zero_anomalies = list(per_site = zero_anomalies_site),
    duplicates = list(per_site = duplicates_site)
  )

  figures <- NULL
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    figures <- list()
    figures$missingness_before_after <- ggplot2::ggplot(
        missingness_site, ggplot2::aes(x = variable, y = missing_pct, fill = phase)) +
      ggplot2::geom_col(position = "dodge") + ggplot2::facet_wrap(~server) +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Missingness before vs. after cleaning (per site)",
                    x = NULL, y = "% missing") +
      ggplot2::theme_minimal()

    figures$missingness_aggregated_before_after <- ggplot2::ggplot(
        missingness_aggregated, ggplot2::aes(x = variable, y = missing_pct, fill = phase)) +
      ggplot2::geom_col(position = "dodge") + ggplot2::coord_flip() +
      ggplot2::labs(title = "Missingness before vs. after cleaning (aggregated, site-weighted)",
                    x = NULL, y = "% missing") +
      ggplot2::theme_minimal()

    figures$conformity_before_after <- ggplot2::ggplot(
        conformity_site,
        ggplot2::aes(x = server, y = n_invalid_numeric + n_invalid_categorical, fill = phase)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(title = "Non-conforming variables before vs. after cleaning",
                    x = NULL, y = "# invalid numeric + categorical columns") +
      ggplot2::theme_minimal()

    figures$zero_anomalies_before_after <- ggplot2::ggplot(
        zero_anomalies_site, ggplot2::aes(x = server, y = n_flagged, fill = phase)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(title = "Variables flagged for suspicious zero-inflation, before vs. after",
                    x = NULL, y = "# flagged variables") +
      ggplot2::theme_minimal()
  } else {
    message("ggplot2 not installed -- returning report tables only, no figures.")
  }

  list(report = report, figures = figures)
}
