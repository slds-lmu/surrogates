#' @title Reading data
#'
#' @description
#' Loading data from csv
#'
#' @param self :: `Surrogate`\cr
#' @export
load_from_csv = function(self) {
  requireNamespace("data.table")
  # Load and rename column
  data = data.table::fread(self$data_source)
  colnames(data)[colnames(data) == self$eval_measure] = "performance"
  # Scale performance column
  data$performance[data$task_id == self$oml_task_id] = self$scaler$scale(data, oml_task_id = self$oml_task_id)
  # Subset columns, only relevant data
  self$param_names = intersect(getParamIds(self$param_set), colnames(data))
  data = data[data$task_id == self$oml_task_id, c("performance", self$param_names), with = FALSE]
  return(data)
}

#' @title Reading data
#'
#' @description
#' Loading data from RDS
#'
#' @param filepath :: `character()`\cr
#' @export
load_from_rds = function(filepath) {
  data = as.data.frame(readRDS(filepath))
  return(data)
}

#' @title Reading data
#'
#' @description
#' Loading data from ARFF
#'
#' @param filepath :: `character()`\cr
#' @export
load_from_arff = function(filepath) {
  requireNamespace("farff")
  data = as.data.frame(farff::readARFF(filepath))
  return(data)
}

#' @title Reading data
#'
#' @description
#' Loading data from OpenML
#'
# @export
load_from_oml = function() {}
