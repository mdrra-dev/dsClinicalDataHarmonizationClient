#' @title Check whether at least one of bDMARD/tsDMARD is present, per row
#' @export
ds.check_btsdmard_presence <- function(df, bDMARD_col = "bDMARD", tsDMARD_col = "tsDMARD",
                                        nfilter = 5, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("check_btsdmard_presenceDS", as.symbol(df), bDMARD_col, tsDMARD_col, nfilter))
  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": ", r$n_either_present, " of ", r$n_rows,
            " rows (", round(100 * r$prop_either_present, 1), "%) have bDMARD or tsDMARD present")
  }
  invisible(results)
}

#' @title Derive btsDMARD from bDMARD + tsDMARD (before imputation)
#' @description bDMARD kept as-is (1-5); tsDMARD recoded onto 6-9
#'   (tofa=1->6, bari=2->7, upa=3->8, filgo=4->9); missing-both imputed to 0.
#'   Run this BEFORE the general imputation step.
#' @export
ds.derive_btsdmard <- function(df, bDMARD_col = "bDMARD", tsDMARD_col = "tsDMARD",
                                newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_btsdmard")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("derive_btsdmardDS", as.symbol(df), bDMARD_col, tsDMARD_col, newobj))
  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": btsDMARD -- ", r$n_from_bDMARD, " from bDMARD, ",
            r$n_from_tsDMARD, " from tsDMARD (recoded), ", r$n_imputed_zero, " imputed to 0",
            if (r$n_both_present_disagreeing > 0)
              paste0(" -- WARNING: ", r$n_both_present_disagreeing, " row(s) had BOTH bDMARD and tsDMARD non-missing"))
  }
  message("Result saved as: '", newobj, "'")
  invisible(newobj)
}
