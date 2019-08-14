context("surrogate_collection_class")

test_that("initialization", {
  ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")

  s1 = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  expect_r6(s1, classes = "Surrogate")
  s2 = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  expect_r6(s2, classes = "Surrogate")
  sc = SurrogateCollection$new(list(s1, s2))
  expect_r6(sc, classes = "SurrogateCollection")
  expect_list(self$surrogates)
  expect_true(all(sc$active))
  expect_true(all(sc$oml_task_ids %in% c(3, 31)))
})

test_that("prediction", {
  ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")

  s1 = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
  expect_r6(s1, classes = "Surrogate")
  s2 = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)
})