#' @title Select (keep) specific columns on a server-side data frame
#' @export
ds.select_columns <- function(df, columns, newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_selected")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("select_columnsDS", as.symbol(df), paste(columns, collapse = "$"), newobj))
  for (s in names(results)) message("Server ", s, ": selected columns -- ",
                                     paste(results[[s]]$columns_selected, collapse = ", "))
  invisible(newobj)
}

# #' @title Filter rows of a server-side data frame by a simple condition
# #' @export
# ds.filter_rows <- function(df, column, operator = c("==", "!=", ">", "<", ">=", "<=",
#                                                       "%in%", "is.na", "not.na"),
#                             value = NULL, newobj = NULL, datasources = NULL) {
#   if (is.null(datasources)) datasources <- datashield.connections_find()
#   operator <- match.arg(operator)
#   if (is.null(newobj)) newobj <- paste0(df, "_filtered")

#   results <- DSI::datashield.aggregate(conns = datasources,
#     expr = call("filter_rowsDS", as.symbol(df), column, operator, value, newobj))
#   for (s in names(results)) message("Server ", s, ": kept ", results[[s]]$n_after, " of ",
#     results[[s]]$n_before, " rows (", column, " ", operator,
#     if (!operator %in% c("is.na", "not.na")) paste(" ", deparse(value)), ")")
#   invisible(newobj)
# }
#' @title Filter rows of a server-side data frame by a simple condition
#'
#' @param df Name of the server-side data frame.
#' @param column Character, the column to test.
#' @param operator Text operator:
#'   \code{"eq"} (\code{==}),
#'   \code{"neq"} (\code{!=}),
#'   \code{"gt"} (\code{>}),
#'   \code{"lt"} (\code{<}),
#'   \code{"gte"} (\code{>=}),
#'   \code{"lte"} (\code{<=}),
#'   \code{"in"} (\code{%in%}),
#'   \code{"is_na"} (\code{is.na}),
#'   \code{"not_na"} (\code{not.na}).
#' @param value Comparison value; ignored for \code{is_na} and \code{not_na}.
#' @param newobj Name of the output server-side object.
#' @param datasources DataSHIELD connections.
#' @return Invisibly returns \code{newobj}.
#' @export
ds.filter_rows <- function(df,column,operator = c("eq", "neq", "gt", "lt", "gte", "lte",
                                                "in", "is_na", "not_na"),value = NULL,newobj = NULL,datasources = NULL) {
if (is.null(datasources)) {
datasources <- datashield.connections_find()
}
operator <- match.arg(operator)
if (is.null(newobj)) {
newobj <- paste0(df, "_filtered")
}

results <- DSI::datashield.aggregate(conns = datasources, 
                                      expr = call("filter_rowsDS",as.symbol(df),column,operator,value,newobj)
)

for (s in names(results)) {
message("Server ", s, ": kept ",results[[s]]$n_after, " of ",results[[s]]$n_before," rows (", column, " ", operator,
if (!operator %in% c("is_na", "not_na")) {
  paste(" ", deparse(value))} 
else {""},")")
}
invisible(newobj)}

#' @title Join two server-side data frames on key column(s), no .x/.y suffixes
#' @export
ds.join_dataframes <- function(df1, df2, by, join_type = c("inner", "left", "right", "full"),
                                newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  join_type <- match.arg(join_type)
  if (is.null(newobj)) newobj <- paste0(df1, "_joined")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("join_dataframesDS", as.symbol(df1), as.symbol(df2), paste(by, collapse = "$"), join_type, newobj))
  for (s in names(results)) {
    r <- results[[s]]
    message("Server ", s, ": joined -> ", r$n_rows, " rows",
      if (length(r$overlap_columns_kept_from_df1) > 0)
        paste0("; overlapping column(s) kept from ", df1, ": ", paste(r$overlap_columns_kept_from_df1, collapse = ", ")))
  }
  invisible(newobj)
}

#' @title Set a column's value on a server-side data frame
#' @export
ds.set_column_value <- function(df, column, value = NULL, value_expr = NULL,
                                 newobj = NULL, datasources = NULL) {
  if (is.null(datasources)) datasources <- datashield.connections_find()
  if (is.null(newobj)) newobj <- paste0(df, "_set")

  results <- DSI::datashield.aggregate(conns = datasources,
    expr = call("set_column_valueDS", as.symbol(df), column, value, value_expr, newobj))
  for (s in names(results)) message("Server ", s, ": set column '", results[[s]]$column_set, "'")
  invisible(newobj)
}
