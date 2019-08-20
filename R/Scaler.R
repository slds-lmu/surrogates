#' Scaler Class
#' Scales data according to a given method
Scaler = R6Class("Scaler",
  public = list(
    method = NULL,
    values = NULL,
    initialize = function(method = "range") {
      assert_choice(method, c("standardize", "range", "none"))
      self$method = method
    },
    scale = function(data, oml_task_id, ...) {
      x = data[data$task_id == oml_task_id, ]$performance
      if (is.null(self$values)) {
        # Save transformation
        self$values = switch(self$method,
          "standardize" = self$set_values(data.frame("mean" = mean(x), "sd" = sd(x))),
          "range" = self$set_values(data.frame("min" = min(x), "max" = max(x))),
          "none" = self$set_values(data.frame("constant" = 0))
        )
        if (self$method != "none") x = BBmisc::normalize(x, self$method, on.constant = "quiet")
      } else {
        if (self$method == "range") {
          div = self$values[["max"]] - self$values[["min"]]
          if (div == 0) x = 0
          else x = (x - self$values[["min"]]) / div
        } else stop("Error, other methods not implemented yet")
      }
      return(x)
    },
    rescale = function(x) {
      switch(self$method,
        "standardize" = x * self$values["sd"] + self$values["mean"],
        "range" = BBmisc::normalize(x, "range", self$values),
        "none" = x
      )
    },
    set_values = function(x) {
      assert_data_frame(x, nrows = 1, types = "numeric")
      self$values = unlist(x)
      invisible(self)
    }
  ),
  active = list(
    scaler_name = function() {self$method}
  )
)


#' Scales data according to a given method
#' dividing by a function of the time it took to train the model.
ScalerTimeCrit = R6Class("ScalerTimeCrit",
  inherit = Scaler,
  public = list(
    power = NULL,
    base = NULL,
    runtime_values = NULL,

    initialize = function(method = "range", base = 1, power = 0.5) {
      super$initialize(method)
      self$base = assert_number(base)
      self$power = assert_number(power)
    },
    scale = function(data, oml_task_id) {
      data = data[data$task_id == oml_task_id, ]
      x = data$performance
      if (is.null(self$values)) {
        # Save transformation
        self$values = switch(self$method,
          "standardize" = self$set_values(data.frame("mean" = mean(x), "sd" = sd(x))),
          "range" = self$set_values(data.frame("min" = min(x), "max" = max(x))),
          "none" = self$set_values(data.frame("constant" = 0))
        )
        if (self$method != "none") x = BBmisc::normalize(x, self$method, on.constant = "quiet")
      } else {
        if (self$method == "range") {
          div = (self$values[["max"]] - self$values[["min"]])
          if (div == 0) x = 0
          else x = (x - self$values[["min"]]) / div
        } else stop("Error, other methods not implemented yet")
      }
      # Scale runtime to [min/max; 1]
      time = data[, .(runtime = runtime / self$runtime_values[["max"]]), by = .(task_id)][["runtime"]]

      # Either just use sqrt scaling or log(x, base)^pow
      if (self$base == 1) x / (time^self$power)
      else x / (log(time + self$base, base = self$base)^self$power)
    },
    rescale = function(x) {
      BBmisc::normalize(x, "range", self$values)
    },
    set_runtime_values = function(x) {
      assert_data_frame(x, nrows = 1, types = "numeric")
      self$runtime_values = unlist(x)
      invisible(self)
    }
  ),
  active = list(
    scaler_name = function() {paste0("range_timecrit_b_" , self$base, "p_", self$power)}
  )
)
