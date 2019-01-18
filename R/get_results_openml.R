# FIXME: This is the start of some functions that allow us to get
#        Results from OpenML in order to directly create the tasks.
#        As this is not entirely straightforward, development is
#        paused for here now.
# Current Problems:
#        listOMLSetup does not allow us to subset for task.id,
#        passing setup.ids explodes the url string and without subsetting
#        we have to query way too much unneccessary data.


# Put the results into a table
# get performances

# listOMLRunEvaluations(tag = "botV1",
#  evaluation.measure = "area_under_roc_curve",
#   limit = 1000))
# # subset results on some information(s) and measure(s)
# my_runs = my_runs[, c("run.id", "task.id", "area.under.roc.curve" )]

# # get hyperparameters
# runs = listOMLRuns(tag = "botV1", task.id = 31, limit = 10000)
# paras = listOMLSetup(runs$setup.id)
# paras_names = names()
# paras = paras[paras$parameter.name %in% paras_names, c("setup.id", "parameter.name", "value")]
# library(tidyr)
# library(dplyr)
# paras = spread(paras, key = parameter.name, value = value)
# paras = merge(paras, runs[, c("run.id", "setup.id")], key = "setup.id")
# paras = select(paras, -setup.id)

# # Put things together
# results = merge(my_runs, paras, by = "run.id")
# # Put it in a nice order
# results = results[, c(setdiff(names(results), "area.under.roc.curve"), "area.under.roc.curve")]
# # Now you can compare the performances of your different hyperparameters
# print(head(results))



get_flow_ids = function(lrn) {
  flow = listOMLFlows(tag = "botV1")
  flow = flow[flow$name == paste0("mlr.classif.", learner), ]
  return(flow$flow.id)
}

get_runs = function(base_learner, task.id) {

  flows = get_flow_ids(base_learner)


  i = 0
  results_left = TRUE
  runs = data.frame()

  while(results_left) {

    data = listOMLRuns(tag = "botV1",
      task.id = task_id,
      flow.id = flows,
      limit = 10000,
      offset = i * 10000
    )

    runs = rbind(runs, data)
    if (nrow(data) == 0) {
      results_left = FALSE
    }
    i = i + 1
  }
  return(runs)
}


get_params = function(base_learner, task.id, paras_names) {
  flows = get_flow_ids(base_learner)
  runs = get_runs(base_learner, task.id)
  setups = split(runs$setup.id, ceiling(seq_len(nrow(runs))/100))
  for(i in seq_len(length(setups))) {
    data = listOMLSetup(flow.id = flows, setup.id = setups[[i]])
    data = data[data$parameter.name %in% paras_names, c("setup.id", "parameter.name", "value")]
    data = tidyr::spread(data, key = parameter.name, value = value)
    params = dplyr::bind_rows(params, data)
  }

  return(params)
}

# base_learner = "glmnet"
# runz = get_runs(base_learner, 31)
# parz = get_params(base_learner, c("alpha", "lambda"))
# data = merge(data, runs[, c("run.id", "setup.id")], key = "setup.id")
