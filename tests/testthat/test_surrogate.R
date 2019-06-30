context("surrogate_class")

test_that("initialization", {
  ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")
  surr = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, param_names = "lambda", surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)

  expect_r6(surr)
  expect_equal(colnames(surr$data), c("performance", surr$param_names))
})

test_that("print_fun", {
  ds = system.file("extdata/", "glmnet_sample.csv", package = "surrogates")
  ps = get_param_set("glmnet")
  surr = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_set = ps, param_names = "lambda", surrogate_learner = "regr.ranger",
    data_source = ds, load_fun = load_from_csv)

  expect_invisible(print(surr))
})

#test_that("acquire_object_fun", {
#  ds = system.file("inst/extdata/", "glmnet_sample.csv", package = "surrogates")
#  surr = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
#    param_names = "lambda", surrogate_learner = "regr.ranger",
#    data_source = ds, load_fun = load_from_csv)
#
#  surr$acquire_model()
#})
