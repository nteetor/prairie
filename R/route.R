#' Routing
#' 
#' Within prairie, a route is thought of as \emph{a mapping between any number 
#' of methods, specified by} \code{method}\emph{, and a URI,} \code{path}. A 
#' route is never assigned more than a single path. However, because \code{path}
#' is treated as a \link[base:regex]{regular expression} a single route could 
#' match many different client requests. Further details below.
#' 
#' @param method A character vector specifying an HTTP method(s), such as 
#'   \code{"get"}, \code{"post"}, or \code{"put"}, case-insensitive.
#' @param path A character string specifying which URI the route will handle.
#' @param handler A function whose return value is an object of class 
#'   \code{response} object, see details below.
#'   
#' @details
#' 
#' \strong{\code{method}}
#' 
#' \code{method} is a character vector which specifies at least one HTTP method.
#' Alternatively, the keywords \code{"all"} or \code{"ALL"} may be used to 
#' specifiy the route must accept any HTTP method. Custom methods may be used, 
#' but are not advised.
#' 
#' \code{method} is converted to lower case, so \code{"GET"} and \code{"get"} 
#' are equivalent.
#' 
#' \strong{\code{path}}
#' 
#' \code{path} is a character string and is treated as a regular expression. 
#' When specifying a \code{path} it is unnecessary to include a beginning 
#' \code{/} and a resource located at the root of a web application is specified
#' by \code{^$}.
#' 
#' \strong{\code{handler}}
#' 
#' \code{handler} is a function with a single argument \code{req}. When an 
#' application receives a request from a client said request is parsed into a
#' \link{request} object and is made available to \code{handler}. This allows
#' routes to handle specific HTTP header fields included by the client as part 
#' of their request.
#' 
#' @return
#' 
#' A route object.
#' 
#' @seealso \code{\link{request}}, \code{\link{response}}
#'   
#' @export
#' @name routing
#' @examples
#' # typically, route() is called inside of 
#' # app(), but creation of standalone route 
#' # objects is possible
#' 
#' # accepts only GET requests
#' # path has capture groups, include args
#' route(
#'   'get',
#'   '^transformers/(?<series>[a-z_]+)$',
#'   function(args) {
#'     res <- response()
#'     
#'     if (args['series'] == 'beast_wars') {
#'       body(res) <- 'Right on!'
#'     } else {
#'       body(res) <- 'I can dig that.'
#'     }
#'     
#'     res
#'   }
#' )
#' 
#' # accepts both GET and POST requests
#' # no capture groups, no need for args
#' route(
#'   c('get', 'post'),
#'   '^blog/comments$',
#'   function() {
#'     req <- request()
#'     res <- response()
#'     
#'     if (method(req) == 'get') {
#'       body(res) <- 'Get your own comments!'
#'     } else {
#'       body(res) <- 'Thanks for commenting'
#'     }
#'     
#'     res
#'   }
#' )
route <- function(method, path, handler) {
  assert_that(
    is.character(method),
    is.character(path),
    length(path) == 1,
    is.function(handler)
  )
  
  structure(
    list(
      method = tolower(method),
      path = path,
      handler = handler
    ),
    class = 'route'
  )
} 
R6::R6Class(
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
      self$handler(request_environment)
    }
  )
)

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
  route(x$method, x$path, x$handler)
}

#' @export
#' @rdname as.route
is.route <- function(x) inherits(x, 'route')

