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
s = SurrogateLocalFile$new(oml_task_id = 31, baselearner_name = "glmnet",
  data_source = "~/Downloads/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
  measure_name = "auc", param_names = "lambda", surrogate_learner = "regr.ranger")
fail::fail(s$fail_path())
s$file_rtask_to_disk()
s$file_model_to_disk()
s$predict(data.frame("lambda" = seq(from = 0, to = 10, by = 0.1)))
```
## SurrogateCollection

```r
source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
surrogate.mlr.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)

s = SurrogateLocalFile$new(oml_task_id = 31, baselearner_name = "glmnet",
  data_source = "~/Downloads/OpenMLRandomBotResultsFinal_mlr.classif.glmnet.csv",
  measure_name = "auc", param_names = "lambda", surrogate_learner = "regr.ranger")

fail::fail(s$fail_path())
s$file_rtask_to_disk()
s$file_model_to_disk()
s$predict(data.frame("lambda" = seq(from = 0, to = 10, by = 0.1)))
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
SCALINGS = c("normalize", "scale")

```



