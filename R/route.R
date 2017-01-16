#' Routing
#'
#' Within prairie, a route is thought of as a mapping between any number
#' of methods, specified by \code{method}\emph{, and a URI,} \code{path}. A
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
#' specifiy the route to accept any HTTP method. Custom methods may be used,
#' but are not advised.
#'
#' \code{method} is converted to upper case, so \code{"GET"} and \code{"get"}
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
#' @return
#'
#' A route object.
#'
#' @seealso \code{\link{request}}, \code{\link{response}}
#'
#' @export
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
  if (!is.character(method)) {
    stop('argument `method` must be of class character', call. = FALSE)
  }

  if (!is.character(path)) {
    stop('argument `path` must be of class character', call. = FALSE)
  }

  if (length(path) != 1) {
    stop('argument `path` must be a single character string', call. = FALSE)
  }

  if (!is.function(handler)) {
    stop('argument `handler` must be a function', call. = FALSE)
  }

  if (length(formals(handler)) != 1) {
    formals(handler) <- alist(.req = )
  }

  method <- toupper(method)

  structure(
    list(
      method = method,
      path = path,
      handler = handler
    ),
    class = 'route'
  )
}

#' @rdname route
#' @export
is.route <- function(x) {
  inherits(x, 'route')
}

#' Coercing Objects to Routes
#'
#' The function \code{as.route} provides an alternative means of creating an
#' application route.
#'
#' @param x An \R object.
#' @param \ldots Additional arguments passed on to methods.
#'
#' @details
#'
#' If \code{x} is a list, \code{x} must have the following named elements:
#' \code{method}, \code{path}, and \code{handler}.
#'
#' If \code{x} is a character vector, \code{x} is interpreted as a file name.
#' The file must contain a route defined using the \code{route} function. The
#' default directory for route files is "routes", but a different folder may be
#' specified with the \code{directory} argument.
#'
#' The S3 generic function \code{as.route} is exported by prairie to encourage
#' creation of \code{as.route.*} functions. Custom \code{as.route} functions
#' allow users to coerce their classes to routes and quickly serve them over
#' HTTP.
#'
#' @return
#'
#' A \code{route} object.
#'
#' @seealso \code{\link{route}}
#'
#' @name as.route
#' @export
#' @examples
#' # Easily reuse routes and keep applications
#' # modular by storing routes in separate files.
#'
#' tmp <- tempfile()
#' writeLines(
#'   'route("GET", "^$", function(req) response())\n',
#'   con = tmp
#' )
#'
#' as.route(tmp, dir = '')
#'
#' file.remove(tmp)
#'
#' # as.route.list is a minimal wrapper
#' # around route()
#'
#' route(
#'   'POST',
#'   '^$',
#'   function(req) {
#'     response()
#'   }
#' )
#'
#' as.route(
#'   list(
#'     method = 'POST',
#'     path = '^$',
#'     handler = function(req) {
#'       response()
#'     }
#'   )
#' )
as.route <- function(x, ...) {
  UseMethod('as.route')
}

#' @export
#' @rdname as.route
as.route.route <- function(x, ...) {
  x
}

#' @param directory System path to the folder containing the route file.
#' @export
#' @rdname as.route
as.route.character <- function(x, directory = 'routes', ...) {
  path <- file.path(directory, x)

  if (!file.exists(path)) {
    stop('file "', path, '" does not exist', call. = FALSE)
  }

  if (!is_readable(path)) {
    stop('do not have read permissions for "', path, '"', call. = FALSE)
  }

  route <- tryCatch(
    source(path)$value,
    error = function(e) NULL
  )

  if (!is.route(route)) {
    stop('could not parse route from "', path, '"', call. = FALSE)
  }

  # drop 'srcref' from handler function
  attributes(route$handler) <- NULL

  route
}

#' @export
#' @rdname as.route
as.route.list <- function(x, ...) {
  if (is.null(x[['method']])) {
    stop('cannot coerce list to route, missing method', call. = FALSE)
  }
  if (is.null(x[['path']])) {
    stop('cannot coerce list to route, missing path', call. = FALSE)
  }
  if (is.null(x[['handler']])) {
    stop('cannot coerce list to route, missing handler', call. = FALSE)
  }

  route(x[['method']], x[['path']], x[['handler']])
}

#' Printing Routes and Mockups
#'
#' Prints a route or mockup. Printing a mockup prints the underlying route.
#'
#' @param x A route or route mockup.
#' @param \ldots Ignored.
#'
#' @seealso \code{\link{route}}, \code{\link{mockup}}
#'
#' @keywords internal
#' @export
#' @examples
#' route(
#'   c('GET', 'POST'),
#'   '^path$',
#'   function(req) {
#'     response()
#'   }
#' )
#'
#' route(
#'   'put',
#'   '^another/path$',
#'   function(req) {
#'     response()
#'   }
#' )
print.route <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

#' @rdname print.route
#' @export
format.route <- function(x, ...) {
  str_m <- collapse(x[['method']])
  str_p <- x[['path']]
  str_h <- paste(gsub('\\s+', '', deparse(args(x[['handler']]))[1]), '{..}')

  width <- max(nchar(str_m), nchar(str_p), nchar(str_h))
  frmt <- paste0('%', width, 's')

  formatted <- c(
    'A route:',
    paste(sprintf(frmt, str_m), '<methods>'),
    paste(sprintf(frmt, str_p), '<path>'),
    paste(sprintf(frmt, str_h), '<handler>')
  )

  paste('#', formatted, collapse = '\n')
}
