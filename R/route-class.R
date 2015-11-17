#' Coercing to a route
#' 
#' The function \code{as.route} provides an alternative way to create 
#' \code{\link[=route]{routes}} from lists or files.
#' 
#' @param x any \R object.
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
as.route <- function(x, ...) UseMethod('as.route')

#' @inheritParams as.route
#' @export
#' @rdname as.route
as.route.route <- function(x) x

#' @inheritParams as.route
#' @param path System path to file.
#' @export
#' @rdname as.route
as.route.character <- function(x, path = 'routes') {
  assert_that(
    file.exists(file.path(path, x)),
    is.readable(file.path(path, x))
  )
  
  route <- source(file.path(path, x))$value
  if (!is.route(route)) stop('Could not parse route from "', file.path(path, x), '"', call. = FALSE)
  
  route
}

#' @inheritParams as.route
#' @export
#' @rdname as.route
as.route.list <- function(x) {
  assert_that(
    x %has_name% 'method',
    x %has_name% 'path',
    x %has_name% 'handler'
  )
  route__$new(x$method, x$path, x$handler)
}

#' @inheritParams as.route
#' @export
#' @rdname as.route
is.route <- function(x) inherits(x, 'route')

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

    is = function(methods, path) {
      all(methods %in% self$method) && path == self$path
    },
    matches = function(method, path) {
      (method %in% self$method || self$method == 'all') && grepl(self$path, path)
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
