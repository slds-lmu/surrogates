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
    oml_task_ids = NULL,
    holdout_task_id = NULL,
    active = NULL,

    initialize = function(surrogates) {
      self$surrogates = surrogates
      names(self$surrogates) = vcapply(surrogates, function(x) x$key_base)
      self$oml_task_ids = vnapply(self$surrogates, function(x) x$oml_task_id)
      self$active = rep(TRUE, length(self$surrogates))
    },

    subset_surrogates = function(oml_task_ids, baselearner) {
      assert_subset(oml_task_ids, unique(self$oml_task_ids))
      assert_subset(baselearner, unique(self$baselearners))

      # If null, active tasks else specific task
      if (is.null(oml_task_ids))
        use_task = which(self$oml_task_ids %in% self$oml_task_ids_active)
      else use_task = which(self$oml_task_ids %in% oml_task_ids)

      # If no base learner is given, all else only specific bl
      if (is.null(baselearner)) use_bls  = seq_along(self$surrogates)
      else use_bls = which(self$baselearners %in% baselearner)

      Reduce(intersect, list(use_task, use_bls))
    },

    # Predict on a list of [newdata], named with [baselearners]
    predict = function(newdata, oml_task_ids = NULL, baselearners = NULL) {
      assert_list(newdata, names = "named")

      if (is.null(baselearners)) {
        baselearners = setNames(names(newdata), names(newdata))
      }
      if (is.null(oml_task_ids)) {
        oml_task_ids = self$oml_task_ids_active
      }

      prds = lapply(seq_along(baselearners), function(bl) {
        self$predict_bl(newdata[[bl]], oml_task_ids, baselearners[bl])
      })
      names(prds) = baselearners

      # Perhaps aggregate
      if(!is.null(self$aggfun_)) prds = self$aggfun_(prds)

      return(prds)
    },

    # Predict a single base learner
    predict_bl = function(newdata, oml_task_ids = NULL, baselearner = NULL) {
      use_surrogates = self$subset_surrogates(oml_task_ids, baselearner)
      prds = lapply(self$surrogates[use_surrogates], function(x) x$predict(newdata))
      prds = do.call("cbind", prds)
      return(prds)
    },

    # Predict on the held-out data.
    evaluate_holdout_task = function(newdata) {
      self$predict(newdata, self$holdout_task_id, NULL, NULL)
    },
    set_holdout_task = function(oml_task_id) {
      assert_subset(oml_task_id, self$oml_task_ids)
      self$holdout_task_id = oml_task_id
      self$active = !(self$oml_task_ids %in% oml_task_id)
    },
    aggfun_ = function(x) {
      lapply(x, function(y) {
        apply(y, 1, mean)
      })
    }
  ),
  active = list(
    surrogates_active = function() self$surrogates[self$active],
    oml_task_ids_active = function() unique(vnapply(self$surrogates_active, function(x) x$oml_task_id)),
    baselearners = function() vcapply(self$surrogates, function(x) x$base_learner),
    measures = function() vcapply(self$surrogates, function(x) x$measure_name),
    param_names = function() lapply(self$surrogates_active, function(x) x$param_names),
    surrogate_learner = function() vcapply(self$surrogates, function(x) {x$surrogate_learner$short.name}),
    aggfun = function(fun) {
      if(missing(val)) self$aggfun_
      else self$aggfun_ = assert_function(fun)
    }
  )
)
