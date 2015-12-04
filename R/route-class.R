#' Coercing Objects to Routes
#' 
#' The function \code{as.route} provides an alternative way to create 
#' \code{\link[=route]{routes}} from lists or files.
#' 
#' @details
#' 
#' If \code{x} is a list, \code{x} must have the following named items: 
#' \code{method}, \code{path}, and \code{handler}.
#' 
#' If \code{x} is a character vector, \code{x} is interpreted as a file name. 
#' The file must contain a route defined using the \link{route} function. The 
#' default directory for route files is "routes", but a different folder may be 
#' specified by the argument \code{path}.
#' 
#' The S3 generic function \code{as.route} is exported by prairie to encourage 
#' creation of \code{as.route.*} functions. Custom \code{as.route} functions 
#' allow users to coerce their classes to routes and quickly serve them over
#' HTTP.
#' 
#' @return
#' 
#' An object of class \code{route}.
#' 
#' @export
#' @name as.route
NULL

#' @param x An \R object.
#' @param \ldots Arguments passed on to other methods.
#' @export
as.route <- function(x, ...) UseMethod('as.route')

#' @export
#' @rdname as.route
as.route.route <- function(x, ...) x

#' @param path System path to file.
#' @export
#' @rdname as.route
as.route.character <- function(x, path = 'routes', ...) {
  assert_that(
    file.exists(file.path(path, x)),
    is.readable(file.path(path, x))
  )
  
  route <- tryCatch(source(file.path(path, x))$value, error = function(e) NULL)
  if (!is.route(route)) stop('Could not parse route from "', file.path(path, x), '"', call. = FALSE)
  
  route
}

#' @export
#' @rdname as.route
as.route.list <- function(x, ...) {
  assert_that(
    x %has_name% 'method',
    x %has_name% 'path',
    x %has_name% 'handler'
  )
  route__$new(x$method, x$path, x$handler)
}

#' @export
#' @rdname as.route
is.route <- function(x) inherits(x, 'route')

route__ <- R6Class(
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
        is.function(handler)
      )
      
      self$method <- tolower(method)
      self$path <- path
      self$handler <- handler

      invisible(self)
    },

    is = function(methods, path) {
      all(methods %in% self$method) && path == self$path
    },
    matches = function(method, path) {
      (method %in% self$method || self$method == 'all') && grepl(self$path, path)
    },
    dispatch = function(request_environment) {
      assign('__request_environment', request_environment)
      self$handler()
    }
  )
)
