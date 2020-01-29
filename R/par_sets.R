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

param_sets$add("rbv2_classif.glmnet",
  makeParamSet(
    makeNumericParam("alpha", lower = 0, upper = 1, default = 1, trafo = function(x) max(0, min(1, x))),
    makeNumericVectorParam("s", len = 1L, lower = -10, upper = 10, default = 0, trafo = function(x) 2^x),
    makeDiscreteParam("num.impute.selected.cpo", values = c("impute.median", "impute.mean", "impute.hist"))
))

param_sets$add("rbv2_classif.rpart",
  makeParamSet(
    makeNumericParam("cp", lower = 0, upper = 1, default = 0.01),
    makeIntegerParam("maxdepth", lower = 1, upper = 30, default = 30),
    makeIntegerParam("minbucket", lower = 1, upper = 60, default = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 60, default = 20),
    makeDiscreteParam("num.impute.selected.cpo", values = c("impute.median", "impute.mean", "impute.hist"))
))

param_sets$add("rbv2_classif.svm",
  makeParamSet(
    makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
    makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x), # Discuss bounds -10, 3
    makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")), # Discuss bounds -10, 3
    makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial")),
    makeNumericParam("tolerance", lower = 0.001, upper = 1),
    makeLogicalParam("shrinking"),
    makeDiscreteParam("num.impute.selected.cpo", values = c("impute.median", "impute.mean", "impute.hist"))
))

param_sets$add("rbv2_classif.ranger.pow",
  makeParamSet(
    makeIntegerParam("num.trees", lower = 1, upper = 2000), # Discuss bounds to 1,500
    makeLogicalParam("replace"),
    makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
    makeIntegerParam("mtry.power", lower = 0, upper = 1),
    makeDiscreteParam("respect.unordered.factors", values = c("ignore", "order", "partition")),
    makeIntegerParam("min.node.size", lower = 1, upper = 100),
    makeDiscreteParam("splitrule", values = c("gini", "extratrees")),
    makeIntegerParam("num.random.splits", lower = 1, upper = 100, default = 1L, requires = quote(splitrule == "extratrees")),
    makeDiscreteParam("num.impute.selected.cpo", values = c("impute.median", "impute.mean", "impute.hist"))
))

param_sets$add("rbv2_classif.RcppHNSW",
  makeParamSet(
    makeIntegerParam(id = "k", lower = 1L, upper = 50),
    makeDiscreteParam(id = "distance", values = c("l2", "cosine", "ip"), default = "l2"),
    makeIntegerParam(id = "M", lower = 18, upper = 50),
    makeNumericParam(id = "ef", lower = 4, upper = 8, trafo = function(x) round(2^x)),
    makeNumericParam(id = "ef_construction", lower = 4, upper = 8, trafo = function(x) round(2^x)),
    makeDiscreteParam(id = "num.impute.selected.cpo", values = c("impute.median", "impute.mean", "impute.hist"))
))

param_sets$add("rbv2_classif.kerasff",
  makeParamSet(
    makeNumericParam(id = "epochs", lower = 3, upper = 7, trafo = function(x) round(2^x)),
    makeDiscreteParam(id = "optimizer",
      values = c("sgd", "rmsprop", "adam")),
    makeNumericParam(id = "lr", lower = -5, upper = 0, trafo = function(x) 5^x),
    makeNumericParam(id = "decay", lower = -8, upper = 0, trafo = function(x) 5^x),
    makeNumericParam(id = "momentum", lower = -8, upper = 0,trafo = function(x) 5^x,
      requires = quote(optimizer == "sgd")),
    makeNumericParam(id = "rho", lower = -8, upper = 0,trafo = function(x) 5^x,
      requires = quote(optimizer == "rmsprop")),
    makeNumericParam(id = "beta_1", lower = -8, upper = 0, trafo = function(x) 1 - 5^x,
      requires = quote(optimizer %in% c("adam", "nadam"))),
    makeNumericParam(id = "beta_2", lower = -8, upper = 0, trafo = function(x) 1 - 5^x,
      requires = quote(optimizer %in% c("adam", "nadam"))),
    makeIntegerParam(id = "layers", lower = 1L, upper = 4L),
    makeDiscreteParam(id = "batchnorm_dropout", values = c("batchnorm", "dropout", "none")),
    makeNumericParam(id = "input_dropout_rate", lower = 0, upper = 1, requires = quote(batchnorm_dropout == "dropout")),
    makeNumericParam(id = "dropout_rate", lower = 0, upper = 1, requires = quote(batchnorm_dropout == "dropout")),
    # Neurons / Layers
    makeIntegerParam(id = "units_layer1", lower = 3L, upper = 9,  trafo = function(x) round(2^x)),
    makeIntegerParam(id = "units_layer2", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 2)),
    makeIntegerParam(id = "units_layer3", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 3)),
    makeIntegerParam(id = "units_layer4", lower = 3L, upper = 9, trafo = function(x) round(2^x), requires = quote(layers >= 4)),
    # Activations
    makeDiscreteParam(id = "act_layer", values = c("relu", "selu", "tanh")),
    # Initializers
    makeDiscreteParam(id = "init_layer",
      values = c("glorot_normal", "glorot_uniform", "he_normal", "he_uniform")),
    # Regularizers
    makeNumericParam(id = "l1_reg_layer",
      lower = -10, upper = -1, trafo = function(x) 5^x),
    makeNumericParam(id = "l2_reg_layer",
      lower = -10, upper = -1, trafo = function(x) 5^x),
    makeLogicalParam(id = "learning_rate_scheduler", default = FALSE),
    makeDiscreteParam(id = "init_seed", values = c(1L, 11L, 101L, 131L, 499L)),
    makeDiscreteParam(id = "num.impute.selected.cpo", values = c("impute.median", "impute.mean", "impute.hist"))
))

param_sets$add("rbv2_classif.xgboost",
  makeParamSet(
    makeDiscreteParam("booster", values = c("gblinear", "gbtree", "dart")),
    makeIntegerParam("nrounds", lower = 3, upper = 11, trafo = function(x) round(2^x)),
    makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("subsample",lower = 0.1, upper = 1),
    makeNumericParam("eta",   lower = -10, upper = 0, trafo = function(x) 2^x, requires = quote(booster %in% c("dart", "gbtree"))),
    makeNumericParam("gamma", lower = -15, upper = 3, trafo = function(x) 2^x, requires = quote(booster %in% c("dart", "gbtree"))),
    makeIntegerParam("max_depth", lower = 1, upper = 15, requires = quote(booster %in% c("dart", "gbtree"))),
    makeNumericParam("min_child_weight",  lower = 0, upper = 7, trafo = function(x) 2^x, requires = quote(booster %in% c("dart", "gbtree"))),
    makeNumericParam("colsample_bytree",  lower = 0, upper = 1, requires = quote(booster %in% c("dart", "gbtree"))),
    makeNumericParam("colsample_bylevel", lower = 0, upper = 1, requires = quote(booster %in% c("dart", "gbtree"))),
    makeNumericParam("rate_drop", lower = 0, upper = 1, requires = quote(booster %in% c("dart"))),
    makeNumericParam("skip_drop", lower =  0, upper = 1, requires = quote(booster %in% c("dart")))
))

get_param_set = function(key) {param_sets$get(key)}
