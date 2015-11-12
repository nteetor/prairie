#' @docType class
#' @keywords internal
#' @name route-class
route__ <- R6::R6Class(
  'route__',
  public = list(
    method = NULL,
    path = NULL,
    handler = NULL,
    
    initialize = function(method, path, handler) {
      self$method <- method
      self$path <- self$sanitize_path(path)
      self$handler <- handler

      invisible(self)
    },

    is = function(method, path) {
      method == self$method && path == self$path
    },
    matches = function(method, path) {
      method == self$method && grepl(self$path, path)
    },
    dispath = function(req, res, args = character(0)) {
      if (length(formals(self$handler)) == 2) {
        self$handler(req, res)
      } else {
        self$handler(req, res, args)
      }
    },
    
    sanitize_path = function(path) {
      # I typically would not modify the user's specification, however
      # the fact that ^$, in R, does not match the empty string is bonkers
      if (path == '^$') '^\\B$' else paste(path, collapse = '|')
    }
  )
)
