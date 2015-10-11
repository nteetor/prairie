# Encapsulates a method and callback
layer <- R6::R6Class(
  'layer',
  public = list(
    method = NULL,
    callback = NULL,
    path = NULL,
    .ALL = NULL,

    initialize = function(method, path, callback, all) {
      self$method <- method
      self$callback <- callback
      self$path <- path
      self$.ALL <- all

      invisible(self)
    },
    matches = function(method, path) {
      (self$method %in% c(method, self$.ALL)) && self$path == path
    }
  )
)
