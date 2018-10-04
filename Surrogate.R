library(R6)
library(fail)
library(data.table)
library(BBmisc)
library(mlr)


#FIXME: machen wir das scaling hier?
# FIXME: nur für bestimmte hyperpars das SM fitten. wenn man nicht alle will
#FIXME: und es ist sehr bloed wie kommt man an das gesmate parset ran?
# FIXME: wo genau tut man das fail_hanlde am besten hin? global damit man immer rankommt?

Surrogate = R6Class("Surrogate",
  public = list(
    # FIXME: bullshit here
    foo = "~/Downloads/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
    fail_handle = NULL,

    oml_task_id = NULL,
    oml_task_info = NULL,
    measure_name = NULL,
    baselearner_name = NULL,
    hp_names = NULL,
    par_set = NULL,

    rtask = NULL,
    learner = NULL,
    model = NULL,
    resample = NULL,
    scaling = "normalize",

    initialize = function(fail_handle, oml_task_id, measure_name, baselearner_name, hp_names, learner) {
      self$fail_handle = fail_handle
      self$oml_task_id = oml_task_id
      self$oml_task_info = fread("oml_task_info.txt")[task.id == self$oml_task_id, ]
      self$measure_name = measure_name
      self$baselearner_name = baselearner_name
      self$hp_names = hp_names
      self$par_set = get_par_set(baselearner_name)
      self$learner = mlr::checkLearner(learner)
    },

    print = function(...) {
      catf("Surrogate for OML task <%i> for measure <%s> for BL <%s>",
        self$oml_task_id, self$measure_name, self$baselearner_name)
      catf("RTask: %s", ifelse(is.null(self$rtask), "no", "yes"))
      catf("Model: %s", ifelse(is.null(self$model), "no", "yes"))
      catf("Performance: %s", ifelse(is.null(self$resample), "N/A", self$resample$aggr))
    },

    predict = function(newdata) {
      self$acquire_model()
      checkmate::assert_data_frame(newdata)
      checkmate::assert_subset(self$hp_names, colnames(newdata))
      newdata = newdata[, self$hp_names, drop = FALSE]
      predict(self$model, newdata = newdata)
      },

    file_rtask_to_disk = function() {
      d = as.data.frame(fread(self$foo))

      # Take all hyper params if none specified
      if (is.null(self$hp_names))
        self$hp_names = names(self$par_set)
      d = d[d$data_id == self$oml_task_id, c(self$measure_name, self$hp_names)]

      scale_fun = switch(self$scaling,
        "normalize" = function(x) {
        if (min(x) == max(x)) {
          1
          } else {
          (x - min(x)) / (max(x) - min(x))
        }
      })

      d[self$measure_name] = scale_fun(d[self$measure_name])

      attributes(d$target) = NULL #https://github.com/imbs-hl/ranger/issues/354
      self$rtask = makeRegrTask(id = self$key_rtask, data = d, target = self$measure_name)
      self$fail_handle$put(keys = self$key_rtask, self$rtask)
    },

    file_model_to_disk = function() {
      self$acquire_rtask()
      self$model = train(self$learner, self$rtask)
      self$fail_handle$put(keys = self$key_model, self$model)
    },

    file_resample_to_disk = function(cv = cv3, measures = rmse) {
      self$acquire_rtask()
      self$resample = resample(self$learner, self$rtask, cv, measures)
      self$fail_handle$put(keys = self$key_resample, self$resample)
    },

    acquire_object = function(id) {
      if (!is.null(self[[id]]))
        return
      if(!self[[sprintf("in_cache_%s", id)]]) {
        self[[sprintf("file_%s_to_disk", id)]]()
      } else {
        self[[id]] = fail_handle$get(self[[sprintf("key_%s", id)]])
      }
    },
    acquire_rtask = function() self$acquire_object("rtask"),
    acquire_model = function() self$acquire_object("model")
  ),

  active = list(
    key_base = function() sprintf("%i_%s_%s",
      self$oml_task_id, self$measure_name, self$baselearner_name),
    key_rtask = function() paste0("surr_rtask_", self$key_base),
    key_model = function() paste0("surr_model_", self$key_base),
    key_resample = function() paste0("surr_resample_", self$key_base),
    in_cache_rtask = function() self$key_rtask %in% self$fail_handle$ls(),
    in_cache_model = function() self$key_model %in% self$fail_handle$ls(),
    in_cache_resample = function() self$key_resample %in% self$fail_handle$ls()
  )
)
