#' Route class
#'
#' This class is a glorified container for a url and the corresponding callback
#' function. Some handy utility functions are included as private methods. As
#' dull middleware begins to take shape this class will evolve more.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{uri_matches(path)}: TRUE if the route
#'   uri, as a regular expression, matches path
#'  \item \code{assign_callback(method, callback)}: set the callback function for a
#'   specific HTTP method
#'  \item \code{callback_for(method)}: return the callback
#'   function for a specific HTTP method
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom assertthat assert_that
#' @export
#' @name route-class
route <- R6::R6Class(
  'route',
  public = list(
    strict = NULL,

    initialize = function(options = list()) {
      assert_that(is.null(options$strict) || is.logical(options$strict))

      self$strict <- options$strict %||% FALSE

      private$stack <- list()
      private$path <- '/'

      invisible(self)
    },

    all = function(path, callback, ...) {
      if (missing(path)) path <- private$path
      private$push(private$.ALL, path, list(callback, ...))
      invisible(self)
    },
    get = function(path, callback, ...) {
      if (missing(path)) path <- private$path
      private$push('GET', path, list(callback, ...))
      invisible(self)
    },
    put = function(path, callback, ...) {
      if (missing(path)) path <- private$path
      private$push('PUT', path, list(callback, ...))
      invisible(self)
    },
    post = function(path, callback, ...) {
      if (missing(path)) path <- private$path
      private$push('POST', path, list(callback, ...))
      invisible(self)
    },
    route = function(path) {
      private$path <- private$sanitize_path(path)
      invisible(self)
    },
    use = function(path = private$path, fn, ...) {
      private$push(private$.ALL, path, list(fn, ...))
      invisible(self)
    },

    dispath = function(req, res) {
      method <- toupper(req$method)

      for (layer in private$stack) {
        if (layer$matches(method, req$path)) {
          return(layer$callback(req, res))
        }
      }

      NULL
    }
  ),
  private = list(
    stack = NULL,
    path = NULL,
    method = NULL,
    .ALL = 'ALL',

    push = function(method, path, callbacks) {
      path <- private$sanitize_path(path)
      private$stack <- append(private$stack, lapply(callbacks, function(c) {
        layer$new(method, path, c, private$.ALL)
      }))
    },
    sanitize_path = function(path) {
      # I typically would not modify the user's specification, however
      # the fact that ^$, in R, does not match the empty string is bonkers
      if (path == '^$') '^\\B$' else paste(path, collapse = '|')
    }
  )
)
