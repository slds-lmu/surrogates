library(R6)
library(fail)

#FIXME: machen wir das scaling hier?
# FIXME: nur für bestimmte hyperpars das SM fitten. wenn man nicht alle will
#FIXME: und es ist sehr bloed wie kommt man an das gesmate parset ran?
# FIXME: wo genau tut man das fail_hanlde am besten hin? global damit man immer rankommt?

Surrogate = R6Class("Surrogate",
  public = list(
    # FIXME: bullshit here
    foo = "~/Desktop/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
    fail_handle = NULL,
    
    oml_task_id = NULL,
    measure_name = NULL,
    baselearner_name = NULL,
    hp_names = NULL, 
    par_set = NULL,
   
    rtask = NULL,
    learner = NULL,
    model = NULL,
    
    initialize = function(fail_handle, oml_task_id, measure_name, baselearner_name, hp_names) {
      self$fail_handle = fail_handle
      self$oml_task_id = oml_task_id
      self$measure_name = measure_name
      self$baselearner_name = baselearner_name
      self$hp_names = hp_names
    },

    print = function(...) {
      catf("Surrogate for OML task <%i> for measure <%s> for BL <%s>", 
        self$oml_task_id, self$measure_name, self$baselearner_name)
      catf("RTask: %s", ifelse(is.null(self$rtask), "no", "yes"))
      catf("Model: %s", ifelse(is.null(self$model), "no", "yes"))
    },
    
    file_rtask_to_disk = function() {
      d = as.data.frame(fread(self$foo))
      # data[, target := scale_fun(get(measure)), by = data_id]
      # attributes(data$target) = NULL #https://github.com/imbs-hl/ranger/issues/354
      data = d[, c(self$measure_name, self$hp_names)]
      self$rtask = makeRegrTask(id = self$key_rtask, data = d, target = self$measure_name)
      self$fail_handle$put(keys = self$key_rtask, self$rtask)
    },

    file_model_to_disk = function() {
      self$acquire_rtask()
      print(55)
      self$model = train(self$learner, self$rtask) 
      print(56)
      self$fail_handle$put(keys = self$key_model, self$model)
    },
    
    acquire_object = function(id) {
      if (!is.null(self[[id]]))
        return
      print(33)
      print(id)
      if(!self[[sprintf("in_cache_%s", id)]]) {
        self[[sprintf("file_%s_to_disk", id)]]
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
    in_cache_rtask = function() self$key_rtask %in% self$fail_handle$ls(),
    in_cache_model = function() self$key_model %in% self$fail_handle$ls()
  )
)
