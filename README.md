[![Build Status](https://travis-ci.com/compstat-lmu/surrogates.svg?branch=master)](https://travis-ci.com/compstat-lmu/surrogates)

# Surrogates for Machine Learning Algorithms

The purpose of this model is to create a single **API** for different surrogates across algorithms, datasets and scalings.

What we want to achieve:
- Make training surrogate models across learners / datasets possible, allow training from different sources
  (csv / arff, OpenML, ...)
- Unify storing of models and allow for fast read/write where requried.
- Fast access to predictions for search on top of surrogates.
- Consistency checks for models and parameters / parameter spaces.
- Beeing able to incorporate different surrogate learners / scalings.
- Clean and fast **API** for training and obtaining predictions.


## Surrogates

```r
# Train surrogate from a local file.
file = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
s = Surrogate$new(oml_task_id = 9952L, base_learner = "glmnet", data_source = file,
  param_set = get_param_set("classif.glmnet"), eval_measure = "auc",
  surrogate_learner = "regr.ranger", load_fun = load_from_csv)
s$train()
prd = s$predict(data.frame("lambda" = seq(from = 0, to = 10, by = 0.1), "alpha" = 1:101))
```

## SurrogateCollection

```r
file = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
surrs = lapply(c(3, 37, 43, 49), function(tid) {
  s = Surrogate$new(oml_task_id = tid, base_learner = "glmnet", data_source = file,
    param_set = get_param_set("classif.glmnet"), eval_measure = "auc",
    surrogate_learner = "regr.ranger", load_fun = load_from_csv)
})

sc = SurrogateCollection$new(surrs)
sc$predict(list("glmnet" =
  data.frame("lambda" = seq(from = 0, to = 10, by = 0.1), "alpha" = 1:101)
))
```

## Config

```r
# Ids for the  38 Task from the RandomBot
OML_DATASET_IDS = c(3,31,37,44,50,151,312,333,334,335,1036,1038,1043,1046,1049,1050,
  1063,1067,1068,1120,1176,1461,1462,1464,1467,1471,1479,1480,1485,1486,
  1487,1489,1494,1504,1510,1570,4134,4534)
# And corresponding task.ids
OML_TASK_IDS = c(3L, 31L, 37L, 43L, 49L, 219L, 3485L, 3492L, 3493L, 3494L, 3889L,
3891L, 3896L, 3899L, 3902L, 3903L, 3913L, 3917L, 3918L, 3954L,
34536L, 14971L, 14965L, 10093L, 10101L, 9980L, 9983L, 9970L,
9971L, 9976L, 9977L, 9978L, 9952L, 9957L, 9967L, 9946L, 9914L,
14966L, 34539L, 34537L)

BASE_LEARNERS = c("glmnet", "rpart", "kknn", "svm", "ranger", "xgboost")
MEASURES = c("auc", "acc", "brier")

SURROGATE_LEARNERS = c("regr.ranger", "regr.fixcubist")
```

## Benchmark:

```r
# Use cubist as a learner.
source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
file = system.file("extdata", "glmnet_sample.csv", package = "surrogates")
learners = list(makeLearner("regr.ranger", num.trees = 20L), makeLearner("regr.fixcubist", committees = 20L), makeLearner("regr.RcppHNSW"))

scs = lapply(learners, function(surr) {
  surrs = lapply(c(3, 37, 43, 49), function(tid) {
    s = Surrogate$new(oml_task_id = tid, base_learner = "glmnet", data_source = file,
      param_set = get_param_set("classif.glmnet"), eval_measure = "auc",
      surrogate_learner = surr, load_fun = load_from_csv)$train()
  })
  SurrogateCollection$new(surrs)
})

df = data.frame("lambda" = seq(from = 0, to = 10, by = 0.01), "alpha" = 1:1001)
df = list("glmnet" = df)

library(microbenchmark)
microbenchmark(
  "ranger" = scs[[1]]$predict(df),
  "cubist" = scs[[2]]$predict(df),
  "hnsw" = scs[[3]]$predict(df),
  times = 2
)
```


## Different Data Sources

```r

file = "../surrogates_data/data/rbv2_mlr_classif.glmnet.arff"

split_dataset_col = function(df) {
  df[, "task_id" := gsub(dataset, pattern = "(.*)\\.([^.]*)$", replacement = "\\2")]
  return(df)
}

get_task_ids = function(file) {
  df = data.table(farff::readARFF(file))
  df = split_dataset_col(df)
  task_ids = unique(df$task_id)
}

arff = function(self) {
  requireNamespace("data.table")
  requireNamespace("farff")
  # Load and rename column
  data = data.table(farff::readARFF(self$data_source))
  data = split_dataset_col(data)
  colnames(data)[colnames(data) == self$eval_measure] = "performance"
  if (!(self$oml_task_id %in% data$task_id)) stopf("task_id: %s not found in data", self$oml_task_id)
  # Scale performance column
  data$performance[data$task_id == self$oml_task_id] = self$scaler$scale(data, oml_task_id = self$oml_task_id)
  # Subset columns, only relevant data
  self$param_names = intersect(getParamIds(self$param_set), colnames(data))
  data = data[data$task_id == self$oml_task_id, c("performance", self$param_names), with = FALSE]
  to_factor =  names(Filter(is.character, data))
  data[, to_factor] = data[, lapply(.SD, as.factor), .SDcols = to_factor]
  return(data)
}

task_ids = get_task_ids(file)

surrs = lapply(task_ids, function(tid) {
  s = Surrogate$new(oml_task_id = tid, base_learner = "glmnet", data_source = file,
    param_set = get_param_set("rbv2_classif.glmnet"), eval_measure = "perf.logloss",
    surrogate_learner = "regr.ranger", load_fun = arff)
})

sc = SurrogateCollection$new(surrs)

sc$predict(list("glmnet" =
  data.frame("alpha" = seq(from = 0, to = 10, by = 0.1), "s" = 1:101, num.impute.selected.cpo = "impute.median")
))
```

## xgboost


```r
# Helper functions to read data and obtain task_ids

split_dataset_col = function(df) {
  df[, "task_id" := gsub(dataset, pattern = "(.*)\\.([^.]*)$", replacement = "\\2")]
  return(df)
}

get_task_ids = function(file) {
  df = data.table(farff::readARFF(file))
  df = split_dataset_col(df)
  task_ids = as.integer(unique(df$task_id))
}

arff = function(self) {
  requireNamespace("data.table")
  requireNamespace("farff")
  # Load and rename column
  data = data.table(farff::readARFF(self$data_source))
  data = split_dataset_col(data)
  colnames(data)[colnames(data) == self$eval_measure] = "performance"
  if (!(self$oml_task_id %in% data$task_id)) stopf("task_id: %s not found in data", self$oml_task_id)
  # Scale performance column
  data$performance[data$task_id == self$oml_task_id] = self$scaler$scale(data, oml_task_id = self$oml_task_id)
  # Subset columns, only relevant data
  self$param_names = intersect(getParamIds(self$param_set), colnames(data))
  data = data[data$task_id == self$oml_task_id, c("performance", self$param_names), with = FALSE]
  to_factor =  names(Filter(is.character, data))
  data[, to_factor] = data[, lapply(.SD, as.factor), .SDcols = to_factor]
  return(data)
}
```

```r
file = "../surrogates_data/data/rbv2_mlr_classif.xgboost_minimal.arff"
task_ids = get_task_ids(file)
lrn = makeLearner("regr.ranger", num.trees = 64L)

surrs = lapply(task_ids, function(tid) {
  s = Surrogate$new(oml_task_id = tid, base_learner = "xgboost", data_source = file,
    param_set = get_param_set("rbv2_classif.xgboost"), eval_measure = "perf.logloss",
    surrogate_learner = lrn, load_fun = arff)
})

sc = SurrogateCollection$new(surrs)

df = generateRandomDesign(10L, par.set = get_param_set("rbv2_classif.xgboost"), trafo = TRUE)
sc$predict(list("xgboost" = df))
```
