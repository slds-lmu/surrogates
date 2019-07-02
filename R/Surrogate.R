#' @title Surrogate
#' @usage NULL
#'
#' @format [R6::R6Class] object.
#' @include load_functions.R
#'
#' @description
#' Allows the construction of surrogates from a given meta-data dataset
#' of hyperparameters and a given performance.

#' @section Construction:
#' ```
#'   surr = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet",
#'                        eval_measure = "auc", param_names = "lambda",
#'                        surrogate_learner = "regr.ranger",
#'                        data_source = "inst/extdata/glmnet_sample.csv", load_fun = load_from_csv)
#' ```
#'
#' @section Fields:
#' * `use_cache` :: `logical(1)`\cr
#'
#' * `oml_task_id` :: `integer(1)`\cr
#' OpenML Task id
#'
#' * `task_info` ::
#'
#' * `eval_measure` :: `character()`\cr
#' c('auc', 'acc', 'brier')
#'
#' * `base_learner` :: `character(1)`\cr
#'
#' * `surrogate_learner` :: `character(1)`\cr
#'
#' * `param_names` :: `character()`\cr
#'
#' * `param_set` :: [ParamSet]
#'
#' * `rtask` ::
#'
#' * `model` ::
#'
#' * `resample` ::
#'
#' * `scaling` ::
#'
#' * `save_path` :: `character(1)`\cr
#' Root directory to save the results
#'
#' * `data_source` :: `character(1)`\cr
#' Directory where the data is stored
#'
#' * `load_fun` :: `FUN`\cr
#' Function to load the data
#'
#' * `data` :: `data.frame()`\cr
#' Loaded data stored in a dataframe
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
#' (`data.frame()`, `logical()`) -> `Return Type`\cr
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
#' @export
#' @examples
#'   ps = get_param_set("glmnet")
#'   ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
#'   surr = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet",
#'                        eval_measure = "auc", param_set = ps,
#'                        param_names = "lambda", surrogate_learner = "regr.ranger",
#'                        data_source = ds, load_fun = load_from_csv)

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

    save_path = ".",
    data_source = NULL,
    load_fun = NULL,
    data = NULL,

    scale_fun_pars = NULL,

    initialize = function(oml_task_id, base_learner, eval_measure, surrogate_learner,
          param_set, param_names, use_cache = TRUE, save_path, data_source, load_fun) {
      self$oml_task_id = assert_int(oml_task_id)

      self$base_learner = mlr::checkLearner(base_learner)
      self$eval_measure = if (assert_true(eval_measure %in% mlr::listMeasures())) eval_measure
      self$surrogate_learner = mlr::checkLearner(surrogate_learner)

      self$use_cache = checkmate::assert_flag(use_cache)

      if (!missing(save_path))
        self$save_path = save_path
      self$save_path = fail::fail(self$fail_path())

      self$data_source = assert_string(data_source, null.ok = TRUE)
      if (file.exists(self$data_source))
        self$data = load_fun(self$data_source)
      else
        stop("Input file doesn't exist!")

      if (missing(param_set))
        stop("Please provide a valid param_set of class ParamSet!")
      self$param_set = assert_class(param_set, "ParamSet")
      if (!missing(param_set) && is.null(param_names)) {
        self$param_names = intersect(getParamIds(param_set), colnames(self$data))
      }

      if (!missing(param_set) && !missing(param_names)) {
        self$param_names = intersect(intersect(getParamIds(param_set), param_names), colnames(self$data))
      }

      self$data = self$data[self$data$task_id == self$oml_task_id, c(self$eval_measure, self$param_names)]
      colnames(self$data) = c("performance", self$param_names)
    },

    # TODO:
    predict = function(newdata, rescale = FALSE) {},
    train = function() {},
    scale = function(x) {},
    rescale = function(x) {},

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
      paste(self$save_path, "surrogates", self$base_learner$short.name,
        paste0(self$surrogate_learner$short.name, "_surrogate"),
        self$eval_measure, self$scaling, sep = "/"
      )
    },

    acquire_rtask = function() self$acquire_object("rtask"),
    acquire_model = function() self$acquire_object("model"),
    acquire_resample = function() self$acquire_object("resample"),

    acquire_object = function(id) {
      if (!is.null(self[[id]]))
        return()
      # If not in cache, create the object and write it to disk
      if (!self[[sprintf("in_cache_%s", id)]] | is.null(self$save_path)) {
        catf('Object not in cache')
        self[[sprintf("file_%s_to_disk", id)]]()
      }
      else {
        self[[id]] = self$save_path$get(self[[sprintf("key_%s", id)]])
      }
    },

    file_model_to_disk = function() {
      self$acquire_rtask()
      catf("<Obtaining Model>")
      self$model = train(self$surrogate_learner, self$rtask)
      if (self$use_cache) self$save_path$put(keys = self$key_model, self$model)
    },

    file_resample_to_disk = function(cv = cv3, measures = rmse) {
      self$acquire_rtask()
      catf("<Obtaining Resampling>")
      self$resample = resample(self$base_learner, self$rtask, cv, measures)
      if (self$use_cache) self$save_path$put(keys = self$key_resample, self$resample)
    },

    file_rtask_to_disk = function() {
      # Take all hyper params if none specified
      if (is.null(self$param_names))
        self$param_names = names(self$param_set)

      # Check if param_names are contained in data
      checkmate::assert_subset(self$param_names, colnames(self$data))

      # In case no data exists, we always sample 0 performance
      if (nrow(self$data) == 0L) {
        self$fit_constant_model()
      }

      # attributes(d$target) = NULL # https://github.com/imbs-hl/ranger/issues/354
      catf("<Obtaining Task>")
      tsk = makeRegrTask(id = as.character(self$oml_task_id), data = self$data, target = "performance")
      self$rtask = removeConstantFeatures(tsk)

      catf("<Writing task to disk>")
      if (self$use_cache) self$save_path$put(keys = self$key_rtask, self$rtask)
    },

    fit_constant_model = function() {
      d = ParamHelpers::generateGridDesign(res = 3L, self$param_set)
      d = d[, self$param_names, drop = FALSE]

      # Convert logicals/character to factors for learner
      which.logical = sapply(d, function(x) is.logical(x) | is.character(x))
      if (sum(which.logical > 0))
        d = do.call("cbind", list(d[, !which.logical], lapply(d[, which.logical],
          function(x) as.factor(as.character(x)))))
      d$performance = 0.0
    },

    save = function(keep.model = FALSE, keep.task = FALSE) {
      if (!keep.model) self$model = NULL
      if (!keep.task) self$rtask = NULL
      self$save_path$put(keys = self$key_class, self)
    }
  ),

  active = list(
    key_base = function() sprintf("%i_%s_%s",
      self$oml_task_id, self$eval_measure, self$base_learner$short.name),
    key_model = function() paste0("surr_model_", self$key_base),
    key_rtask = function() paste0("surr_rtask_", self$key_base),
    key_resample = function() paste0("surr_resample_", self$key_base),
    key_class = function() paste0("surrogate_", self$key_base),

    in_cache_rtask = function() self$key_rtask %in% self$save_path$ls(),
    in_cache_model = function() self$key_model %in% self$save_path$ls(),
    in_cache_resample = function() self$key_resample %in% self$save_path$ls()
  ),

  private = list()
)
