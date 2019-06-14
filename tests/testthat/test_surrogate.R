context("surrogate_class")

test_that("initialization", {
  surr = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet", eval_measure = "auc",
                        param_names = "lambda", surrogate_learner = "regr.ranger")
  expect_r6(surr)
})

test_that("print_fun", {
  surr = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet", eval_measure = "auc",
    param_names = "lambda", surrogate_learner = "regr.ranger")

  expect_invisible(print(surr))
})

test_that("load_csv_fun", {
  if (file.exists(system.file("extdata", "glmnet_sample.csv", package = "surrogates"))) {
    ds = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
  }

  surr = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet", eval_measure = "auc",
    param_names = "lambda", surrogate_learner = "regr.ranger")

  surr$load_csv(ds)
  expect_data_frame(surr$data)
  expect_equal(colnames(surr$data), c("performance", surr$param_names))
})

test_that("acquire_object_fun", {
  surr = Surrogate$new(oml_task_id = 31, base_learner = "regr.glmnet", eval_measure = "auc",
    param_names = "lambda", surrogate_learner = "regr.ranger")

  surr$acquire_model()
})
