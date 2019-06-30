#' @title Reading data
#'
#' @description
#' Functions to load data from different sources

#' @export
#' @param filepath :: `character()`\cr
load_from_csv = function(filepath) {
  data = as.data.frame(fread(filepath))
  return(data)
}

#' @export
#' @param filepath :: `character()`\cr
load_rds = function(filepath) {
  data = as.data.frame(readRDS(filepath))
  return(data)
}

#' @export
#' @param filepath :: `character()`\cr
load_arff = function(filepath) {
  data = as.data.frame(farff::readARFF(filepath))
  return(data)
}

# TODO
load_oml = function() {}
