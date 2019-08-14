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
#' @export
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

    # Predict on a list of [newdata], named with [base_learners]
    predict = function(newdata, oml_task_ids = NULL, base_learners = NULL, rescale = FALSE) {
      assert_flag(rescale)
      assert_list(newdata, names = "named")
      assert_integerish(oml_task_ids, null.ok = TRUE)
      assert_character(base_learners, null.ok = TRUE)

      if (is.null(base_learners)) {
        base_learners = setNames(names(newdata), names(newdata))
      }
      if (is.null(oml_task_ids)) {
        oml_task_ids = self$oml_task_ids[self$active]
      }

      prds = setNames(lapply(seq_along(base_learners), function(bl) {
        private$predict_bl(newdata[[bl]], oml_task_ids, base_learners[bl], rescale)
      }), base_learners)

      return(prds)
    },

    # Set a surrogate as hold out (so predict will not evaluate it)
    set_holdout_task = function(oml_task_id) {
      assert_subset(oml_task_id, self$oml_task_ids)
      self$holdout_task_id = oml_task_id
      self$active = !(self$oml_task_ids %in% oml_task_id)
    },
    # Predict on the held-out data.
    evaluate_holdout_task = function(newdata) {
      self$predict(newdata, self$holdout_task_id, NULL)
    }
  ),

  private = list(
    # Predict a single base learner
    predict_bl = function(newdata, oml_task_ids = NULL, baselearner = NULL, rescale) {
      use_surrogates = private$subset_surrogates(oml_task_ids, baselearner)
      if (length(use_surrogates) == 0) warning("No surrogates to predict on!")
      prds = lapply(self$surrogates[use_surrogates], function(x) x$predict(newdata, rescale))
      prds = do.call("cbind", prds)
      return(prds)
    },
    subset_surrogates = function(oml_task_ids, baselearner) {
      assert_subset(oml_task_ids, unique(self$oml_task_ids))
      assert_subset(baselearner, unique(self$base_learners))

      # If null, active tasks else specific task
      if (is.null(oml_task_ids))
        use_task = which(self$oml_task_ids %in% self$oml_task_ids_active)
      else use_task = which(self$oml_task_ids %in% oml_task_ids)

      # If no base learner is given, all else only specific bl
      if (is.null(baselearner)) use_bls  = seq_along(self$surrogates)
      else use_bls = which(self$base_learners %in% baselearner)
      
      # Return applicable surrogates
      Reduce(intersect, list(use_task, use_bls))
    }  
  ),

  active = list(
    base_learners = function() vcapply(self$surrogates, function(x) x$base_learner),
    measures = function() vcapply(self$surrogates, function(x) x$eval_measure),
    surrogate_learners = function() vcapply(self$surrogates, function(x) {x$surrogate_learner$short.name}),
    scalings = function() {
      scales = unique(sapply(self$surrogates, function(x) x$scaler$scaler_name))
      assert_true(length(scales) == 1L)
      return(scales)
    }
  )
)



