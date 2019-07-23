#' Scaler Class
#' Scales data according to a given method
Scaler = R6Class("Scaler",
  public = list(
    method = NULL,
    values = NULL,
    initialize = function(method = "range") {
      assert_choice(method, c("standardize", "range"))
      self$method = method
    },
    scale = function(data, oml_task_id, runtime) {
      x = data[data$task_id == oml_task_id, ]$performance
      if (length(x) > 0) {
        # Save transformation
        self$values = switch(self$method,
          "standardize" = c("mean" = mean(x), "sd" = sd(x)),
          "range" = range(x)
        )
        BBmisc::normalize(x, self$method, on.constant = "quiet")
      }
    },
    rescale = function(x) {
      switch(self$method,
        "standardize" = x * self$values["sd"] + self$values["mean"],
        "range" = BBmisc::normalize(x, "range", self$values)
      )
    }
  ),
  active = list(
    scaler_name = function() {self$method}
  )
)

#' Scales data according to a given method
#' dividing by a function of the time it took to train the model.
ScalerTimeCrit = R6Class("ScalerTimeCrit",
  public = list(
    power = NULL,
    base = NULL,
    method = NULL,
    state = NULL,
    initialize = function(method = "range", base, power) {
      super$initialize(method)
      self$base = assert_number(base)
      self$power = assert_number(power)
    },
    scale = function(x, time, data, oml_task_id, runtime) {
      # Save transformation
      self$values = range(x)
      x = BBmisc::normalize(x, on.constant = "quiet")
      x / (log(time + self$base, base = self$base)^self$power)
    },
    rescale = function(x) {
      BBmisc::normalize(x, "range", self$values)
    }
  ),
  active = list(
    scaler_name = function() {paste0("range_timecrit_b_" , self$base, "p_", self$power)}
  )
)
