#' @title Reading data
#'
#' @description
#' Loading data from csv
#'
#' @param filepath :: `character()`\cr
#' @export
load_from_csv = function(filepath) {
  data = as.data.frame(fread(filepath))
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
  data = as.data.frame(farff::readARFF(filepath))
  return(data)
}

# TODO
#' @title Reading data
#'
#' @description
#' Loading data from OpenML
#'
#' @export
load_from_oml = function() {}
