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
    surrogate_dir = NULL,

    initialize = function(surrogate_dir) {
      self$surrogate_dir = assert_path_for_output(surrogate_dir)
    },

    predict_atomic = function(newdata, oml_task_id, base_learner, eval_measure) {
      assert_data_frame(newdata)
      assert_integerish(oml_task_id)
      assert_string(base_learner)
      assert_string(eval_measure)

      file_path = self$get_file_path(oml_task_id, base_learner, eval_measure)
      assert_file_exists(file_path)

      readRDS(file_path)$surrogate$predict(newdata)
    },

    get_file_path = function(oml_task_id, base_learner, eval_measure) {
      paste(self$surrogate_dir, "/surrogate", base_learner,
        eval_measure, self$scaler$scaler_name, 
        paste0(self$surrogate_learner$short.name), ".rds",  sep = "_")
    }
  )
)



