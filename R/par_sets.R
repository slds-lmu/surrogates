assert_param_set = function(ps) {
  if (ParamHelpers::checkParamSet(ps))
    return(ps)
}

#' Dictionary for ParamSets
#'
#' Parameter sets define the space in which we want to search for defaults.
#' @export
ParamSetDict = R6::R6Class("ParamSetDict",
  public = list(
    items = NULL,
    prefix = NULL,
    initialize = function(prefix) {
      self$prefix = assert_string(prefix)
      self$items = list()
    },
    add = function(key, value) {
      key = self$fixup_key(key)
      self$items[[key]] = assert_param_set(value)
    },
    get = function(key) {
      key = self$fixup_key(key)
      assert_choice(key, choices = self$keys())
      self$items[[key]]
    },
    keys = function() {
      names(self$items)
    },
    fixup_key = function(key) {
      assert_string(key)
      if (!stringi::stri_detect_fixed(key, self$prefix))
        key = stringi::stri_paste(self$prefix, key)
      return(key)
    }
  )
)

#' Dictionary of ParamSets for several mlr algorithms.
#'
#' Parameter sets define the space in which we want to search for defaults.
#' @export
param_sets = ParamSetDict$new(prefix = "classif.")

param_sets$add("classif.glmnet",
  makeParamSet(
    makeNumericParam("alpha", lower = 0, upper = 1, default = 1),
    makeNumericVectorParam("lambda", len = 1L, lower = -10, upper = 10, default = 0, trafo = function(x) 2^x)
))

param_sets$add("classif.rpart",
    makeParamSet(
      makeNumericParam("cp", lower = 0, upper = 1, default = 0.01),
      makeIntegerParam("maxdepth", lower = 1, upper = 30, default = 30),
      makeIntegerParam("minbucket", lower = 1, upper = 60, default = 1),
      makeIntegerParam("minsplit", lower = 1, upper = 60, default = 20)
))

param_sets$add("classif.kknn",
  makeParamSet(
      makeIntegerParam("k", lower = 1, upper = 30)
))

param_sets$add("classif.svm",
  makeParamSet(
    makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
    makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
    makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial"))
))

param_sets$add("classif.ranger",
  makeParamSet(
    makeIntegerParam("num.trees", lower = 1, upper = 2000),
    makeLogicalParam("replace"),
    makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
    makeNumericParam("mtry", lower = 0, upper = 1),
    makeLogicalParam(id = "respect.unordered.factors"),
    makeNumericParam("min.node.size", lower = 0, upper = 1)
))

param_sets$add("classif.xgboost",
  makeParamSet(
    makeIntegerParam("nrounds", lower = 1, upper = 5000),
    makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
    makeNumericParam("eta", lower = -10, upper = 0, trafo = function(x) 2^x),
    makeNumericParam("subsample",lower = 0.1, upper = 1, requires = quote(booster == "gbtree")),
    makeIntegerParam("max_depth", lower = 1, upper = 15, requires = quote(booster == "gbtree")),
    makeNumericParam("min_child_weight", lower = 0, upper = 7, requires = quote(booster == "gbtree"),
      trafo = function(x) 2^x),
    makeNumericParam("colsample_bytree", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
    makeNumericParam("colsample_bylevel", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
    makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x,
      requires = quote(booster == "gblinear")),
    makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x,
      requires = quote(booster == "gblinear"))
))

param_sets$add("rbv2_classif.glmnet",
  makeParamSet(
    makeNumericParam("alpha", lower = 0, upper = 1, default = 1, trafo = function(x) max(0, min(1, x))),
    makeNumericVectorParam("s", len = 1L, lower = -10, upper = 10, default = 0, trafo = function(x) 2^x),
    makeDiscreteParam("num.impute.selected.cpo", values = c("impute.median", "impute.mean", "impute.hist"))
))

get_param_set = function(key) {param_sets$get(key)}
