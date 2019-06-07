#' @title SurrogateLocalFile
#' @usage NULL
#'
#' @format [R6::R6Class] object inheriting from [Surrogate].
#' @description
#' Allows for the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.
#'
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

#' @title SurrogateFromRDS
#' @usage NULL
#'
#' @format [R6::R6Class] object inheriting from [Surrogate].
#' @description
#' Allows for the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.
#'
SurrogateFromRDS = R6Class("SurrogateFromRDS",
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
      # We globally scale performances before subsetting learner
      require(dplyr)
      d = readRDS(self$data_source) %>%
      ungroup() %>%
      filter(measure == self$measure_name) %>%
      filter(task_id == self$oml_task_id) %>%
      mutate(performance = self$scale_fun(performance)) %>%
      filter(learner_id == paste0("mlr.classif.", self$baselearner_name)) %>%
      select(one_of(c("performance", self$param_names))) %>%
      as.data.frame()
    }
  )
)

#' @title SurrogateFromARFF
#' @usage NULL
#'
#' @format [R6::R6Class] object inheriting from [Surrogate].
#' @description
#' Allows for the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.
#'
SurrogateFromARFF = R6Class("SurrogateFromARFF",
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
      d = as.data.frame(farff::readARFF(self$data_source))
      d = d[d$task_id == self$oml_task_id, c(self$measure_name, self$param_names)]
      colnames(d)[colnames(d) == self$measure_name] = "performance"
      d$performance = self$scale_fun(d$performance)
      return(d)
    }
  )
)

#' @title SurrogateOpenML
#' @usage NULL
#'
#' @format [R6::R6Class] object inheriting from [Surrogate].
#' @description
#' Allows for the construction of surrogates from a given meta-data dataset
#' obtained from OpenML.
#'
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
