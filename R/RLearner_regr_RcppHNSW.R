#' @export
makeRLearner.regr.RcppHNSW = function() {
  makeRLearnerRegr(
    cl = "regr.RcppHNSW",
    package = "RcppHNSW",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "k", lower = 1L, default = 1L, when = "predict", upper = 50L),
      makeDiscreteLearnerParam(id = "distance", values = c("euclidean", "l2", "cosine", "ip"), default = "euclidean"),
      makeIntegerLearnerParam(id = "M", lower = 2, upper = Inf, default = 16),
      makeIntegerLearnerParam(id = "ef", lower = 1, upper = Inf, default = 10, when = "predict"),
      makeIntegerLearnerParam(id = "ef_construction", lower = 1, upper = Inf, default = 200),
      makeLogicalLearnerParam(id = "verbose", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(k = 1, M = 16, ef_construction = 40),
    properties = "numerics",
    name = "Approximate neares neighbours",
    short.name = "hnsw",
    callees = "hnsw"
  )
}

#' @export
trainLearner.regr.RcppHNSW = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  pv = list(...)
  ann = RcppHNSW::hnsw_build(as.matrix(data$data), distance = pv$distance, ef = pv$ef_construction, M = pv$M)
  return(list(ann = ann, target = data$target))
}

#' @export
predictLearner.regr.RcppHNSW = function(.learner, .model, .newdata, ...) {
  nns = RcppHNSW::hnsw_search(as.matrix(.newdata), .model$learner.model$ann, ef = 40, ...)
  .model$learner.model$target[nns$idx]
}
