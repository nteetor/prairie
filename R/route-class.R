is.route <- function(obj) inherits(obj, 'route__')

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
      self$handler <- function(req, res) {
        handler(req, res)
        list(
          status = 500,
          headers = list('Content-Type' = 'text/plain'),
          body = 'end() never called'
        )
      }

      invisible(self)
    },

    is = function(method, path) {
      method == self$method && path == self$path
    },
    matches = function(method, path) {
      (method == self$method || self$method == 'all') && grepl(self$path, path)
    },
    dispath = function(req, res) {
      self$handler(req, res)
    },
    
    sanitize_path = function(path) {
      # I typically would not modify the user's specification, however
      # the fact that ^$, in R, does not match the empty string is bonkers
      if (path == '^$') '^\\B$' else paste(path, collapse = '|')
    }
  )
)
