#'@title SurrogateLocalFile
#'
#' @name SurrogateLocalFile
#' @format [R6Class] object
#' @description
#'
#' Inherits from [Surrogate].
#'
#' Allows for the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.
SurrogateLocalFile = R6Class("SurrogateLocalFile",
  inherit = Surrogate,
  public = list(
    data_source = NULL,
    initialize = function(data_source, ...) {
      super$initialize(...)
      self$data_source = assert_string(data_source)
    }
  ),
  private = list(
    get_data_from_source = function() {
      d = as.data.frame(fread(self$data_source))
      d = d[d$data_id == self$oml_task_id, c(self$measure_name, self$param_names)]
      colnames(d)[self$measure_name] = "performance"
    }
  )
)

#'@title SurrogateOpenML
#'
#' @name SurrogateOpenML
#' @format [R6Class] object
#' @description
#'
#' Inherits from [Surrogate].
#'
#' Allows for the construction of surrogates from a given meta-data dataset
#' obtained from OpenML.
SurrogateOpenML = R6Class("SurrogateOpenML",
  inherit = Surrogate,
  public = list(
    oml_bot_tag = NULL,
    initialize = function(oml_bot_tag, ...) {
      stop("Not implemented")
      super$initialize(...)
      self$oml_bot_tag = assert_string(oml_bot_tag)
    }
  ),
  private = list(
    get_data_from_source = function() {
      # Some OpenML Magic needs to happen here
    }
  )
)


#'@title SurrogateLocalFile
#'
#' @name SurrogateLocalFile
#' @format [R6Class] object
#' @description
#'
#' Inherits from [Surrogate].
#'
#' Allows for the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.
SurrogateFromRDS = R6Class("SurrogateLocalFile",
  inherit = Surrogate,
  public = list(
    data_source = NULL,
    initialize = function(data_source, ...) {
      super$initialize(...)
      self$data_source = assert_string(data_source)
    }
  ),
  private = list(
    get_data_from_source = function() {
      require(dplyr)
      d = readRDS(self$data_source) %>%
      filter(measure == self$measure_name) %>%
      filter(learner_id == paste0("mlr.classif.", self$baselearner_name)) %>%
      filter(task_id == self$oml_task_id) %>%
      ungroup() %>%
      select(one_of(c("performance", self$param_names))) %>%
      as.data.frame()
    }
  )
)
