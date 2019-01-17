library(R6)
library(fail)
library(data.table)
library(BBmisc)
library(mlr)
# FIXME: wo genau tut man das fail_handle am besten hin? global damit man immer rankommt?

Surrogate = R6Class("Surrogate",
  public = list(
    fail_handle = NULL,
    use_cache = NULL,

    data_source = NULL,
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


    initialize = function(
      fail_handle, data_source,
      oml_task_id, measure_name, baselearner_name, hp_names = NULL, par_set,
      learner, use_cache = TRUE) {
      self$data_source = data_source
      self$oml_task_id = oml_task_id
      self$oml_task_info = fread("oml_task_info.txt")[task.id == self$oml_task_id, ]
      self$measure_name = measure_name
      self$baselearner_name = baselearner_name
      self$hp_names = hp_names
      self$par_set = ifelse(missing(par_set),  get_par_set(self$baselearner_name),
        assert_par_set(par_set))
      self$learner = mlr::checkLearner(learner)
      self$use_cache = checkmate::assert_flag(use_cache)
      self$fail_handle = if (missing(fail_handle)) {fail(self$fail_path())} else {fail_handle}
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
      d = as.data.frame(fread(self$data_source))

      # Take all hyper params if none specified
      if (is.null(self$hp_names))
        self$hp_names = names(self$par_set)
      # Check if hp_names are contained in data
      checkmate::assert_subset(self$hp_names, colnames(d))

      # Subset task and scale measure column
      d = d[d$data_id == self$oml_task_id, c(self$measure_name, self$hp_names)]
      d[self$measure_name] = self$scale_fun()(d[self$measure_name])
      attributes(d$target) = NULL #https://github.com/imbs-hl/ranger/issues/354

      self$rtask = makeRegrTask(id = self$key_rtask, data = d, target = self$measure_name)
      if (self$use_cache) self$fail_handle$put(keys = self$key_rtask, self$rtask)
    },

    file_model_to_disk = function() {
      self$acquire_rtask()
      self$model = train(self$learner, self$rtask)
      if (self$use_cache) self$fail_handle$put(keys = self$key_model, self$model)
    },

    file_resample_to_disk = function(cv = cv3, measures = rmse) {
      self$acquire_rtask()
      self$resample = resample(self$learner, self$rtask, cv, measures)
      if (self$use_cache) self$fail_handle$put(keys = self$key_resample, self$resample)
    },

    acquire_object = function(id) {
      if (!is.null(self[[id]]))
        return
      if(!self[[sprintf("in_cache_%s", id)]] | is.null(fail_handle)) {
        # If not in cache, create the object and write it to disk
        self[[sprintf("file_%s_to_disk", id)]]()
      } else {
        self[[id]] = fail_handle$get(self[[sprintf("key_%s", id)]])
      }
    },
    acquire_rtask = function() self$acquire_object("rtask"),
    acquire_model = function() self$acquire_object("model"),
    acquire_resample = function() self$acquire_object("resample"),
    scale_fun = function() {
      switch(self$scaling,
        "normalize" = function(x) {
          if (min(x) == max(x)) {
            1
            } else {
            (x - min(x)) / (max(x) - min(x))
          }
        },
        "scale" = function(x) {
        sd = ifelse(sd(x) == 0, 10^-16, sd(x))
        (x - mean(x)) / sd
        }
      )
    },
    fail_path = function() paste("surrogates", self$baselearner_name, paste0(self$learner$short.name, "_surrogate"), self$measure_name, self$scaling, sep = "/")
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
