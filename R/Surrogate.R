#' @title Surrogate
#' @usage NULL
#'
#' @format [R6:R6Class] object.
#'
#' @description
#' Allows the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.

#' @section Construction:
#' ```
#'   surr = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet", eval_measure = "auc",
#'                        param_names = "lambda", surrogate_learner = "regr.ranger")
#' ```
#'
#' @section Fields:
#' * `fail_handle` ::
#'
#' * `use_cache` ::
#'
#' * `oml_task_id` :: `integer()`\cr
#' OpenML Task id
#'
#' * `task_info` ::
#'
#' * `eval_measure` :: `character(1)`\cr
#' c('auc', 'acc', 'brier')
#'
#' * `base_learner` :: `integer()`\cr
#'
#' * `surrogate_learner` :: `character(1)`\cr
#'
#' * `param_names` ::
#'
#' * `param_set` ::
#'
#' * `rtask` ::
#'
#' * `model` ::
#'
#' * `resample` ::
#'
#' * `scaling` ::
#'
#' * `handle_prefix` ::
#'
#' * `scale_fun_pairs` ::
#'
#' @section Methods:
#' TODO: define missing return types
#' * `print()`\cr
#' `()` -> `NULL`\cr
#' Description of the method
#'
#' * `predict(newdata, rescale = FALSE)`\cr
#' (`data.frame()`, `logical(1)`) -> `Return Type`\cr
#' Description of the method
#'
#' * `file_rtask_to_disk()`\cr
#' `()` -> \cr
#' Description of the method
#'
#' * `file_model_to_disk()`\cr
#' `()` -> \cr
#' Description of the method
#'
#' * `file_resample_to_disk()`\cr
#' `()` -> \cr
#' Description of the method
#'
#' * `acquire_object()`\cr
#' `()` -> \cr
#' Description of the method
#'
#' * `acquire_rtask()`\cr
#' `()` -> \cr
#' Description of the method
#'
#' * `acquire_model()`\cr
#' `()` -> \cr
#' Description of the method
#'
#' * `acquire_resample()`\cr
#' `()` -> \cr
#' Description of the method
#'
#' * `scale_fun(x)`\cr
#' `` -> \cr
#' Description of the method
#'
#' * `rescale_fun(x)`\cr
#' `` -> \cr
#' Description of the method
#'
#' * `fail_path(handle_prefix)`\cr
#' `` -> \cr
#' Description of the method
#'
#' * `save(keep.model = FALSE, keep.task = FALSE)`\cr
#' (`logical(1)`, `logical()`) -> \cr
#' Description of the method
#'
#' @family Surrogate
#'
#' @examples
#'   surr = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet", eval_measure = "auc",
#'                        param_names = "lambda", surrogate_learner = "regr.ranger")

Surrogate = R6Class("Surrogate",
  public = list(
    use_cache = TRUE,

    oml_task_id = NULL,
    eval_measure = NULL,
    base_learner = NULL,
    surrogate_learner = NULL,
    param_set = NULL,
    param_names = NULL,

    rtask = NULL,
    task_info = NULL,
    model = NULL,
    resample = NULL,
    scaling = 'normalize',

    fail_handle = NULL,
    handle_prefix = ".",

    scale_fun_pars = NULL,

    data = NULL,

    initialize = function(oml_task_id, base_learner, eval_measure, surrogate_learner,
          param_set, param_names, use_cache = TRUE, fail_handle, handle_prefix) {
      self$oml_task_id = assert_int(oml_task_id)

      self$base_learner = mlr::checkLearner(base_learner)
      self$eval_measure = if (assert_true(eval_measure %in% mlr::listMeasures())) eval_measure
      self$surrogate_learner = mlr::checkLearner(surrogate_learner)

      self$param_set = ifelse(missing(param_set), list(getParamSet(self$base_learner)),
                              list(assert_param_set(param_set)))[[1]]
      self$param_names = ifelse(missing(param_names), list(getParamIds(self$param_set)),
                              list(assert_subset(param_names, getParamIds(self$param_set))))[[1]]
      self$use_cache = checkmate::assert_flag(use_cache)

      if (!missing(handle_prefix))
        self$handle_prefix = assert_character(handle_prefix, null.ok = T)

      if (missing(fail_handle))
        self$fail_handle = fail::fail(self$fail_path())
      else
        self$fail_handle = fail::fail(paste(self$handle_prefix, fail_handle, sep = "/"))
    },

    print = function(...) {
      catf("Surrogate for OML task <%i> for measure <%s> for BL <%s>",
        self$oml_task_id, self$measure_name, self$baselearner_name)
      catf("RTask: %s", ifelse(is.null(self$rtask), "no", "yes"))
      catf("Model: %s", ifelse(is.null(self$model), "no", "yes"))
      catf("Performance: %s", ifelse(is.null(self$resample), "N/A", self$resample$aggr))

      # As with any R6 method called for its side effects, $print() should return invisible(self)
      invisible(self)
    },

    fail_path = function() {
      paste(self$handle_prefix, "surrogates", self$base_learner$short.name,
        paste0(self$surrogate_learner$short.name, "_surrogate"),
        self$eval_measure, self$scaling, sep = "/"
      )
    },

    acquire_rtask = function() private$acquire_object("rtask"),
    acquire_model = function() self$acquire_object("model"),
    acquire_resample = function() private$acquire_object("resample"),

    acquire_object = function(id) {
      if (is.null(self[[id]]))
        return()
      # If not in cache, create the object and write it to disk
      if (!self[[sprintf("in_cache_%s", id)]] | is.null(self$fail_handle)) {
        print('Object not in cache')
        #self[[sprintf("file_%s_to_disk", id)]]()
      }
      #else {
      #  self[[id]] = self$fail_handle$get(self[[sprintf("key_%s", id)]])
      #}
    },

    load_csv = function(filepath) {
      # TODO: check if file exists
      data = as.data.frame(fread(filepath))
      data = data[data$task_id == self$oml_task_id, c(self$eval_measure, self$param_names)]
      self$data = data
      colnames(self$data) = c("performance", self$param_names)
    },

    # TODO: implement loading different file formats
    load_rds = function(filepath) {},
    load_arff = function(filepath) {},
    load_oml = function(){},

    save = function(keep.model = FALSE, keep.task = FALSE) {
      if (!keep.model) self$model = NULL
      if (!keep.task) self$rtask = NULL
      self$fail_handle$put(keys = self$key_class, self)
    }
  ),

  active = list(
    key_base = function() sprintf("%i_%s_%s",
      self$oml_task_id, self$eval_measure, self$base_learner$short.name),
    key_model = function() paste0("surr_model_", self$key_base),
    key_rtask = function() paste0("surr_rtask_", self$key_base),
    key_resample = function() paste0("surr_resample_", self$key_base),
    key_class = function() paste0("surrogate_", self$key_base),

    in_cache_rtask = function() self$key_rtask %in% self$fail_handle$ls(),
    in_cache_model = function() self$key_model %in% self$fail_handle$ls(),
    in_cache_resample = function() self$key_resample %in% self$fail_handle$ls()
  ),

  private = list()
)
