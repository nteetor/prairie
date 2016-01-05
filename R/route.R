#' Routing
#'
#' Within prairie, a route is thought of as \emph{a mapping between any number
#' of methods, specified by} \code{method}\emph{, and a URI,} \code{path}. A
#' route is never assigned more than a single path. However, because \code{path}
#' is treated as a \link[base:regex]{regular expression} a single route may be
#' created to match different client requests. Further details below.
#'
#' @param method A character vector specifying an HTTP method(s), such as
#'   \code{"get"}, \code{"post"}, or \code{"put"}, case-insensitive.
#' @param path A character string specifying which URI the route will handle.
#' @param handler A function whose return value is an object of class
#'   \code{response}, see the Details section below.
#'
#' @details
#'
#' \subsection{Arguments:}{
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
#' \code{/}. To create a route for the root resource, \code{'/'}, one may
#' specify \code{'^$'} as \code{path}.
#'
#' \strong{\code{handler}}
#'
#' \code{handler} is a function with a single argument \code{req}. When an
#' application receives a request, this HTTP request is parsed into a
#' \link{request} object and is made available to \code{handler} as \code{req}.
#' This allows routes to handle specific HTTP header fields included in the
#' request as well as arguments passed as part of the URI.
#'
#' }
#'
#' \subsection{Matching:}{
#'
#' An incoming request is matched to a route by pattern matching each route's
#' \code{path} to the request's URI. Matches are tested for using
#' \code{\link{grepl}}. The order routes are added to an application is
#' important as matches are checked for sequentially and only the handler of the
#' first matching route is run.
#'
#' }
#'
#' @return An object of class route.
#'
#' @seealso \code{\link{request}}, \code{\link{response}}
#'
#' @export
#' @name routing
#' @examples
#' # Typically, routes are created and added to an
#' # application inside app(), but standalone route
#' # objects may be created and added later.
#'
#' # matches only GET requests
#' route(
#'   'GET',
#'   '^transformers/[a-z_]+$',
#'   function(req) {
#'     res <- response()
#'
#'     if (uri(req) == '/transformers/beast_wars') {
#'       body(res) <- 'Right on!'
#'     } else {
#'       body(res) <- 'I can dig that.'
#'     }
#'
#'     res
#'   }
#' )
#'
#' # matches both GET and POST requests
#' route(
#'   c('GET', 'POST'),
#'   '^blog/comments$',
#'   function(req) {
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
  if (length(formals(handler)) != 1) {
    stop('`handler` must be a function with a single argument')
  }
  
  structure(
    list(
      method = tolower(method),
      path = path,
      handler = handler
    ),
    class = 'route'
  )
}

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
#' @rdname as.route
as.route <- function(x, ...) UseMethod('as.route')

#' @export
#' @rdname as.route
as.route.route <- function(x, ...) x

#' @param path System path to the folder containing the route file.
#' @export
#' @rdname as.route
#' @examples
#' # Easily reuse routes and keep your
#' # application modular by storing routes
#' # in separate files.
#'
#' \dontrun{
#'
#' as.route('file1.R')
#'
#' as.route('file2.R', path = 'app')
#' }
#'
#' # but, choose better file names
#'
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
#' @examples
#' # as.route.list is a thin wrapper
#'
#' route_route <- route(
#'   'POST',
#'   '^$',
#'   function(req) response()
#' )
#'
#' list_route <- list(
#'   method = 'POST',
#'   path = '^$',
#'   handler = function(req) response()
#' )
#'
#' all.equal(route_route, list_route)
#'
#' is.route(route_route)
#' is.route(list_route)
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

#' Printing Routes and Mockups
#'
#' Prints a route or mockup. A mockup prints the underlying route object.
#'
#' @param x A \code{route} or route \code{mockup}.
#' @param \ldots Additional arguments passed on to methods.
#'
#' @seealso \code{\link{route}}, \code{\link{mockup}}
#'
#' @export
#' @name print.route
print.route <- function(x, ...) {
  cat('Method:', paste(x$method, collapse = ', '), '\n')
  cat('  Path:', x$path)
}
