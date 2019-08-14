context("surrogate_collection_class")

test_that("initialization", {
  ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")

  s1 = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  expect_r6(s1, classes = "Surrogate")
  s2 = Surrogate$new(oml_task_id = 37, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  expect_r6(s2, classes = "Surrogate")
  sc = SurrogateCollection$new(list(s1, s2))
  expect_r6(sc, classes = "SurrogateCollection")
  expect_list(sc$surrogates, names = "named")
  expect_true(all(sc$active))
  expect_true(all(sc$oml_task_ids %in% c(3, 37)))
})

test_that("predict", {
  ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")
  s1 = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  s2 = Surrogate$new(oml_task_id = 37, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  sc = SurrogateCollection$new(list(s1, s2))
  des = list("regr.glmnet" = generateDesign(10, ps))
  prds = sc$predict(des)
  expect_list(prds, names = "named")
  expect_matrix(prds[[1]], mode = "numeric", ncols = 2)
  cns = colnames(prds[[1]])
  expect_true(all(grepl("glmnet", cns)))
  expect_true(all(grepl("3_", cns)|grepl("37_", cns)))
})



test_that("predict for different holdout tasks", {
  ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")
  s1 = Surrogate$new(oml_task_id = 43, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  s2 = Surrogate$new(oml_task_id = 37, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  sc = SurrogateCollection$new(list(s1, s2))
  des = list("regr.glmnet" = generateDesign(10, ps))
  
  # One Task
  sc$set_holdout_task(43)
  expect_true(!sc$active[sc$oml_task_ids == 43])
  expect_true(sc$active[sc$oml_task_ids == 37])
  prds = sc$predict(des)
  expect_list(prds, names = "named")
  expect_matrix(prds[[1]], mode = "numeric", ncols = 1)
  cns = colnames(prds[[1]])
  expect_true(all(grepl("glmnet", cns)))
  expect_true(all(grepl("37_", cns)))
  expect_true(!(grepl("43_", cns)))

  # Other Task
  sc$set_holdout_task(37)
  expect_true(sc$active[sc$oml_task_ids == 43])
  expect_true(!sc$active[sc$oml_task_ids == 37])
  prds_2 = sc$predict(des)
  expect_list(prds_2, names = "named")
  expect_matrix(prds_2[[1]], mode = "numeric", ncols = 1)
  cns = colnames(prds_2[[1]])
  expect_true(all(grepl("glmnet", cns)))
  expect_true(!(grepl("37_", cns)))
  expect_true(grepl("43_", cns))

  # No Task
  sc$set_holdout_task(NULL)
  expect_true(sc$active[sc$oml_task_ids == 43])
  expect_true(sc$active[sc$oml_task_ids == 37])
  prds_3 = sc$predict(des)
  expect_list(prds_3, names = "named")
  expect_matrix(prds_3[[1]], mode = "numeric", ncols = 2)
  cns = colnames(prds_3[[1]])
  expect_true(all(grepl("glmnet", cns)))
  expect_true(all(grepl("37_", cns) | grepl("43_", cns)))

  # Predictions match
  expect_true(all(prds[[1]] == prds_3[[1]][, 2, drop = FALSE]))
  expect_true(all(prds_2[[1]] == prds_3[[1]][, 1, drop = FALSE]))

  expect_error(sc$set_holdout_task(3))
  expect_error(sc$set_holdout_task("37"))
})

test_that("eval holdout task", {
  ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")
  s1 = Surrogate$new(oml_task_id = 43, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  s2 = Surrogate$new(oml_task_id = 37, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  sc = SurrogateCollection$new(list(s1, s2))
  des = list("regr.glmnet" = generateDesign(10, ps))
  
  sc$set_holdout_task(43)
  prds = sc$evaluate_holdout_task(des)
  expect_list(prds, names = "named")
  expect_matrix(prds[[1]], mode = "numeric", ncols = 1)
  cns = colnames(prds[[1]])
  expect_true(all(grepl("glmnet", cns)))
  expect_true(!(grepl("37_", cns)))
  expect_true(grepl("43_", cns))
})


test_that("active bindings", {
  ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")
  s1 = Surrogate$new(oml_task_id = 43, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  s2 = Surrogate$new(oml_task_id = 37, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  sc = SurrogateCollection$new(list(s1, s2))
  des = list("regr.glmnet" = generateDesign(10, ps))
  
  expect_true(sc$scalings == "range")
  n_surrogates = length(sc$oml_task_ids)
  expect_true(length(sc$surrogate_learners) == n_surrogates)
  expect_true(length(sc$base_learners) == n_surrogates)
  expect_true(length(sc$measures) == n_surrogates)
})