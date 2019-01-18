
s = Surrogate$new(oml_task_id = 31, baselearner_name = "glmnet",
  data_source = "~/Downloads/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
  measure_name = "auc", hp_names = "lambda", learner = "regr.ranger")

print(s)
s$oml_task_info
print(s$in_cache_rtask)
print(s$in_cache_model)
s$file_rtask_to_disk()
print(s$in_cache_rtask)
print(s$in_cache_model)
print(s$rtask)
s$acquire_rtask()
s$acquire_model()
print(s)
print(s$model)
s$predict(data.frame("lambda" = c(0.1, 1)))


s = Surrogate$new(oml_task_id = 31, baselearner_name = "glmnet",
  data_source = "~/Downloads/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
  measure_name = "auc", hp_names = "lambda", learner = "regr.cubist")
s$acquire_resample()

surrogate.mlr.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)
s = Surrogate$new(oml_task_id = 31, baselearner_name = "glmnet",
  data_source = "~/Downloads/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
  measure_name = "auc", learner = surrogate.mlr.lrn)
s$acquire_resample()

s = Surrogate$new(oml_task_id = 31, baselearner_name = "glmnet",
  data_source = "~/Downloads/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
  measure_name = "auc", learner = "regr.ranger")
s$acquire_resample()


