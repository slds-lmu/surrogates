context("surrogate_class")

test_that("initialization", {
  surr = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_names = "lambda", surrogate_learner = "regr.ranger", data_source = "glmnet_sample.csv")
  expect_r6(surr)
})

test_that("print_fun", {
  surr = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_names = "lambda", surrogate_learner = "regr.ranger", data_source = "glmnet_sample.csv")

  expect_invisible(print(surr))
})

test_that("load_csv_fun", {
  surr = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_names = "lambda", surrogate_learner = "regr.ranger", data_source = "glmnet_sample.csv")

  data = surr$load_csv()
  expect_data_frame(data)
  expect_equal(colnames(data), c("performance", surr$param_names))
})

test_that("acquire_object_fun", {
  surr = Surrogate$new(oml_task_id = 3, base_learner = "regr.glmnet", eval_measure = "auc",
    param_names = "lambda", surrogate_learner = "regr.ranger", data_source = "glmnet_sample.csv")

  surr$acquire_model()
})
