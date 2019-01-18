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
SurrogateCollection = R6Class("SurrogateCollection",
  public = list(
    surrogates = NULL,
    initialize = function(surrogates) {
      self$surrogates = surrogates
      names(self$surrogates) = vcapply(self$surrogates, function(x) x$key_base)
    },
    predict = function(newdata, oml_task_ids = NULL, baselearners = NULL, measures = NULL) {

      assert_choice(oml_task_ids, unique(self$oml_task_ids), null.ok = TRUE)
      assert_choice(baselearners, unique(self$baselearners), null.ok = TRUE)
      assert_choice(measures, unique(self$measures), null.ok = TRUE)

      # Deselect surrogates if required
      use_task = seq_along(self$surrogates)
      use_bls  = seq_along(self$surrogates)
      use_meas = seq_along(self$surrogates)
      if (!is.null(oml_task_ids)) use_task = which(self$oml_task_ids %in% oml_task_ids)
      if (!is.null(baselearners)) use_bls = which(self$baselearners %in% baselearners)
      if (!is.null(measures)) use_meas = which(self$measures %in% measures)
      use_surrogates = Reduce(intersect, list(use_task, use_bls, use_meas))

      prds = lapply(self$surrogates[use_surrogates], function(x) x$predict(newdata))
      do.call("cbind", prds)
    }
  ),
  active = list(
    oml_task_ids = function() vnapply(self$surrogates, function(x) x$oml_task_id),
    baselearners = function() vcapply(self$surrogates, function(x) x$baselearner_name),
    measures = function() vcapply(self$surrogates, function(x) x$measure_name),
    surrogate_learners = function() vcapply(self$surrogates, function(x) x$measure_name),
    param_names = function() lapply(self$surrogates, function(x) x$measure_name)
  )
)
