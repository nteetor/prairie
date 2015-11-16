#' Test if route
#' 
#' Checks if \code{obj} is of class \code{route}.
#' 
#' @param obj Object to test
#' 
#' @return
#' 
#' \code{TRUE} if \code{obj} is of class \code{route}
#' 
#' @export
#' @name is.route
#' @examples
#' # One of these is not like the others
#' is.route('way')
#' is.route(90)
#' is.route(route('get', '^$', function(req, res) res$body('TRUE!')$send()))
is.route <- function(obj) inherits(obj, 'route')

#' Converting objects to routes
#' 
#' Routes may be defined with the 
#' 
#' @export
#' @name as.route
as.route <- function(obj, ...) UseMethod('as.route')

#' @param rte An object of class \code{rte}
#' @export
#' @rdname as.route
as.route.route <- function(rte) rte

#' @param file A file which defines a route
#' @param path Folder path to route file
#' @export
#' @rdname as.route
as.route.character <- function(file, path = 'views') {
  assert_that(
    file.exists(file.path(path, file)),
    is.readable(file.path(path, file))
  )
  
  route <- source(file.path(path, file))$value
  if (!is.route(route)) stop('Could not parse "', file.path(path, file), '" as route', call. = FALSE)
  
  route
}

#' @param x A \code{list} with items \code{method}, \code{path}, and
#'   \code{handler}
#' @export
#' @rdname as.route
as.route.list <- function(x) {
  assert_that(
    x %has_name% method,
    x %has_name% path,
    x %has_name% handler
  )
  route__$new(x$method, x$path, x$handler)
}

#' @param other An object of an unsupported class
#' @export
#' @rdname as.route
as.route.default <- function(other) {
  stop('Cannot create route from object of class ', class(other), call. = FALSE)
}

#' @docType class
#' @keywords internal
#' @name route-class
route__ <- R6::R6Class(
  'route',
  public = list(
    method = NULL,
    path = NULL,
    handler = NULL,
    
    initialize = function(method, path, handler) {
      assert_that(
        is.character(method),
        is.character(path),
        length(path) == 1,
        is.function(handler),
        has_args(handler, c('req', 'res'), exact = TRUE)
      )
      
      self$method <- tolower(method)
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
