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

source("https://raw.githubusercontent.com/pfistfl/mlr-extralearner/master/R/RLearner_regr_fixcubist.R")
surrogate.mlr.lrn = makeLearner("regr.cubist", committees = 20, extrapolation = 20)

SURROGATE_LEARNERS = c("regr.ranger", "regr.fixcubist")
SCALINGS = c("normalize", "scale")
