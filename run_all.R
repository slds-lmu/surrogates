source("Surrogate.R")
source("par_sets.R")
source("config.R")

# FIXME: Iterate over oml_task_id, baselearner_name, measure_name(?) learner(?)
# FIXME: load correct data_source
# FIXME: For now decide on scaling

s = Surrogate$new(oml_task_id = 31, baselearner_name = "glmnet",
  data_source = "~/Downloads/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
  measure_name = "auc", hp_names = "lambda", learner = "regr.ranger")
