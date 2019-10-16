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
#' * `eval_measure` :: `character()`\cr
#' c('auc', 'acc', 'brier')
#'
#' * `base_learner` :: `character(1)` \cr Does not need to be an mlr learner.
#'
#' * `surrogate_learner` :: `character(1)`\cr An mlr learner.
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
#' * `scaler` ::
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
#' Loaded data stored in a data.frame
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
#'                        surrogate_learner = "regr.ranger",
#'                        save_path = tempdir(),
#'                        data_source = ds, load_fun = load_from_csv)
Surrogate = R6Class("Surrogate",
  public = list(

    oml_task_id = NULL,
    eval_measure = NULL,
    base_learner = NULL,
    surrogate_learner = NULL,
    param_set = NULL,

    model = NULL,
    resample = NULL,
    scaler = NULL,
    load_fun = NULL,

    save_path = ".",
    data_source = NULL,
    data = NULL,

    param_names = NULL,

    initialize = function(oml_task_id, base_learner, eval_measure, 
      surrogate_learner, param_set, save_path, data_source, 
      load_fun, resample = FALSE, scaler = Scaler$new(),) {

      # Info used for sub-setting the data:
      self$oml_task_id = assert_int(oml_task_id)
      self$base_learner = assert_string(base_learner)
      self$eval_measure = assert_string(eval_measure)
      self$scaler = assert_class(scaler, "Scaler")
      self$surrogate_learner = mlr::checkLearner(surrogate_learner)

      if (!missing(save_path)) self$save_path = save_path
      self$save_path = fail::fail(self$fail_path())
      self$data_source = assert_string(data_source, null.ok = TRUE)
      self$load_fun = assert_function(load_fun)

      if (missing(param_set)) stop("Please provide a valid param_set of class ParamSet!")
      self$param_set = assert_class(param_set, "ParamSet")
      invisible(self)
    },

    train = function() {

      if (file.exists(self$data_source))
        self$data = self$load_fun(self)
      else stop("Input file doesn't exist!")

      catf("<Obtaining Task>")
      tsk = makeRegrTask(id = as.character(self$oml_task_id), data = as.data.frame(self$data), target = "performance")
      self$rtask = removeConstantFeatures(tsk)

      catf("<Obtaining Model>")
      self$model = train(self$surrogate_learner, self$rtask)

      catf("<Writing model to disk>")
      self$save_path$put(keys = self$key_model, self$model)

      if (self$resample) {
        catf("<Obtaining Resampling>")
        self$resample = resample(self$surrogate_learner, self$rtask, cv, measures)
        catf("<Writing resample to disk>")
        self$save_path$put(keys = self$key_resample, self$resample)
      }

      invisible(self)
    },

    predict = function(newdata, rescale = FALSE) {
      if(is.null(self$model)) self$train()
      prd = predict(self$model, newdata = newdata)$data$response
      if (rescale)
        prd = self$scaler$rescale(prd)
      return(prd)
    },

    print = function(...) {
      catf("Surrogate for OML task <%i> for measure <%s> for BL <%s>",
        self$oml_task_id, self$measure_name, self$baselearner_name)
      catf("Model: %s", ifelse(is.null(self$model), "untrained", "trained"))
      catf("Performance: %s", ifelse(is.null(self$resample), "N/A", self$resample$aggr))
      invisible(self)
    },

    fail_path = function() {
      paste(self$save_path, "/surrogate", self$base_learner,
        self$eval_measure, self$scaler$scaler_name, 
        paste0(self$surrogate_learner$short.name),
        sep = "_"
      )
    },

  )
)
