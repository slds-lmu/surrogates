#'@title Surrogate
#'
#' @name Surrogate
#' @format [R6Class] object
#' @description
#' Allows for the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.
#'
#' @section Usage:
#'
#' ```
#' # Construction
#' f = Surrogate$new()
#' ```
#'
#' @section Arguments:
#' * `oml_task_id` ([R6 class] or `type`):
#'   OpenML Task Id
#' * `baselearner_name` (`integer()`):
#'   Description of argument1
#' * `measure_name` (`character(1)`):
#'   Identifier for the instance.
#’
#' @section Details:
#' * `id`: A short descriptive identifier.
#' * `member`: Description of member.
#' * `method1()` ([data.table::data.table()]): Description of method1. The returned data table has two columns: ...
#' * `method2()` (`logical(1)`): Description of method2. Return `TRUE` if some condition holds.
#'
#' @family Surrogate
#'
#' @examples
#' f = ClassFoo$new("hello", 123)
#' f$method1(10)

Surrogate = R6Class("Surrogate",
  public = list(
    fail_handle = NULL,
    use_cache = NULL,

    oml_task_id = NULL,
    task_info = NULL,
    measure_name = NULL,
    baselearner_name = NULL,
    surrogate_learner = NULL,
    param_names = NULL,
    param_set = NULL,

    rtask = NULL,
    model = NULL,
    resample = NULL,
    scaling = "normalize",
    scale_fun_pars = NULL,

    initialize = function(oml_task_id, baselearner_name, measure_name, surrogate_learner,
      param_names = NULL, param_set, use_cache = TRUE, fail_handle, data_source) {
      self$oml_task_id = assert_int(oml_task_id)
      self$measure_name = assert_string(measure_name, null.ok = TRUE)
      self$baselearner_name = assert_string(baselearner_name)
      self$param_set = ifelse(missing(param_set), list(get_param_set(self$baselearner_name)), list(assert_param_set(param_set)))[[1]]
      self$param_names = ifelse(missing(param_names), list(getParamIds(self$param_set)) , list(assert_choice(param_names, getParamIds(self$param_set))))[[1]]
      self$surrogate_learner = mlr::checkLearner(surrogate_learner)
      self$use_cache = checkmate::assert_flag(use_cache)
      self$fail_handle = if(missing(fail_handle)) fail::fail(self$fail_path()) else assert_path_for_output(fail_handle)
    },

    print = function(...) {
      catf("Surrogate for OML task <%i> for measure <%s> for BL <%s>",
        self$oml_task_id, self$measure_name, self$baselearner_name)
      catf("RTask: %s", ifelse(is.null(self$rtask), "no", "yes"))
      catf("Model: %s", ifelse(is.null(self$model), "no", "yes"))
      catf("Performance: %s", ifelse(is.null(self$resample), "N/A", self$resample$aggr))
    },

    predict = function(newdata, rescale = FALSE) {
      self$acquire_model()
      checkmate::assert_data_frame(newdata)
      checkmate::assert_subset(self$param_names, colnames(newdata))
      newdata = newdata[, self$param_names, drop = FALSE]
      prd = predict(self$model, newdata = newdata)
      if (rescale) return(self$rescale_fun(prd$data$response))
      else return(prd$data$respons)
    },

    file_rtask_to_disk = function() {
      catf("<Obtaining Data>")
      d = private$get_data_from_source()

      # Take all hyper params if none specified
      if (is.null(self$param_names))
        self$param_names = names(self$param_set)
      # Check if param_names are contained in data
      checkmate::assert_subset(self$param_names, colnames(d))

      # In case no data exists, we always sample 0 performance
      if (nrow(d) == 0L) {
        d = ParamHelpers::generateGridDesign(res = 3L, self$param_set)
        d = d[, self$param_names, drop = FALSE]

        # Convert logicals/character to factors for learner
        which.logical = sapply(d, function(x) is.logical(x) | is.character(x))
        if (sum(which.logical > 0))
          d = do.call("cbind", list(d[, !which.logical], lapply(d[, which.logical], function(x) as.factor(as.character(x)))))
        d$performance = 0.0
      }
      # attributes(d$target) = NULL # https://github.com/imbs-hl/ranger/issues/354
      catf("<Obtaining Task>")
      tsk = makeRegrTask(id = self$key_rtask, data = d, target = "performance")
      self$rtask = removeConstantFeatures(tsk)

      if (self$use_cache) self$fail_handle$put(keys = self$key_rtask, self$rtask)
    },

    file_model_to_disk = function() {
      self$acquire_rtask()
      catf("<Obtaining Model>")
      self$model = train(self$surrogate_learner, self$rtask)
      if (self$use_cache) self$fail_handle$put(keys = self$key_model, self$model)
    },

    file_resample_to_disk = function(cv = cv3, measures = rmse) {
      self$acquire_rtask()
      catf("<Obtaining Resampling>")
      self$resample = resample(self$learner, self$rtask, cv, measures)
      if (self$use_cache) self$fail_handle$put(keys = self$key_resample, self$resample)
    },

    acquire_object = function(id) {
      if (!is.null(self[[id]]))
        return
      if (!self[[sprintf("in_cache_%s", id)]] | is.null(self$fail_handle)) {
        # If not in cache, create the object and write it to disk
        self[[sprintf("file_%s_to_disk", id)]]()
      } else {
        self[[id]] = self$fail_handle$get(self[[sprintf("key_%s", id)]])
      }
    },
    acquire_rtask = function() self$acquire_object("rtask"),
    acquire_model = function() self$acquire_object("model"),
    train = function() self$acquire_model(),
    acquire_resample = function() self$acquire_object("resample"),
    scale_fun = function(x) {
      self$scale_fun_pars = c(min = min(x), max = max(x))
      if (min(x) == max(x)) {
        0.5
      } else {
        (x - min(x)) / (max(x) - min(x))
      }
    },
    fail_path = function() {
      paste("surrogates", self$baselearner_name,
        paste0(self$surrogate_learner$short.name, "_surrogate"),
        self$measure_name, self$scaling, sep = "/"
      )
    },
    save = function(keep.model = FALSE, keep.task = FALSE) {
      if (!keep.model) self$model = NULL
      if (!keep.task) self$rtask = NULL
      self$fail_handle$put(keys = self$key_class, self)
    }
  ),
  active = list(
    key_base = function() sprintf("%i_%s_%s",
      self$oml_task_id, self$measure_name, self$baselearner_name),
    key_rtask = function() paste0("surr_rtask_", self$key_base),
    key_model = function() paste0("surr_model_", self$key_base),
    key_resample = function() paste0("surr_resample_", self$key_base),
    key_class = function() paste0("surrogate_", self$key_base),
    in_cache_rtask = function() self$key_rtask %in% self$fail_handle$ls(),
    in_cache_model = function() self$key_model %in% self$fail_handle$ls(),
    in_cache_resample = function() self$key_resample %in% self$fail_handle$ls(),
    save_path = function() self$fail_path(),
    is_trained = function() self$key_model %in% self$fail_handle$ls() | !is.null(self$model)
  ),
  private = list(
    acquire_task_info = function() {
      fread("oml_task_info.txt")[task.id == self$oml_task_id, ]
    }
  )
)
