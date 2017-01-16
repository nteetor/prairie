#' Create an Application
#'
#' A prairie application, simply put, is a list of route objects. When creating
#' an application, \code{\ldots} may be any combination of \R objects with a
#' \code{as.route} implementation, see details.
#'
#' @param \ldots \R objects, coerced to routes.
#'
#' @details
#'
#' In order for prairie to best help \R programmers grow web applications from
#' their existing code many coercion generics have been created and are
#' available for implementation. The, arguably, most important of generic is
#' \code{as.route}. CRAN is ripe with packages which include custom S3 classes.
#' prairie aims to provide a streamlined process for serving these classes
#' across the web. An implementation of \code{as.route} might choose a resouce
#' path based on a class attribute or choose the HTTP method based on object
#' permissions (GET for a read-only database connection or GET and POST for a
#' read/write connection).
#'
#' Furthmore, prairie is meant to work on top of, around, and with existing \R
#' code. While writing a good implementation of \code{as.route} may take time,
#' if this package is succesful, one will not need to modify existing code. You
#' have worked hard on your code and prairie is along to grow that code onwards
#' to the web.
#'
#' @seealso \code{\link{as.route}}, \code{\link{run}}
#'
#' @export
#' @examples
#' app(
#'   route(
#'     'GET',
#'     '^',
#'     function(req) {
#'       res <- response()
#'       body(res) <- 'Hello, world!'
#'       res
#'     }
#'   )
#' )
#'
application <- function(...) {
  routes <- lapply(list(...), as.route)

  structure(
    list(
      routes = routes
    ),
    class = 'application'
  )
}

#' @rdname application
#' @export
app <- application

#' @rdname application
#' @export
is.application <- function(x) {
  inherits(x, 'application')
}

#' Print an Application
#'
#' Prints an application.
#'
#' @param x An application.
#' @param \ldots Ignored.
#'
#' @keywords internal
#' @export
print.application <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

#' @rdname print.application
#' @export
format.application <- function(x, ...) {
  paste(
    paste('# An application:', length(x[['routes']]), 'routes'),
    paste(
      vapply(x[['routes']], format, character(1)),
      collapse = '\n'
    ),
    sep = '\n'
  )
}

#' Start Up a Prairie Application
#'
#' Run an application at host \code{host} on port number \code{port}.
#'
#' @param app An application object.
#' @param host A character string specifying the host name.
#' @param port An numeric specifying the port number.
#'
#' @export
#' @name run
run <- function(app, host, port) {
  if (!is.application(app)) {
    stop('argument `app` must be of class application', call. = FALSE)
  }

  if (!is.character(host)) {
    stop('argument `host` must be of class character', call. = FALSE)
  }

  if (!is.numeric(port)) {
    stop('argument `port` must be of class numeric', call. = FALSE)
  }

  httpuv::runServer(
    host,
    port,
    list(
      call = function(req) {
        req <- as.request(req)

        matching_route <- find_route(app[['routes']], req)

        if (is.null(matching_route)) {
          return(
            list(
              status = 404,
              headers = list(`Content-Type` = 'text/plain'),
              body = 'Sorry, page not found.'
            )
          )
        }

        req$ROUTE_PATH <- matching_route[['path']]
        res <- matching_route[['handler']](req)

        if (!is.response(res)) {
          return(
            list(
              status = 500,
              headers = list(`Content-Type` = 'text/plain'),
              body = 'Internal error, no response generated.'
            )
          )
        }

        as.list(res)
      }
    )
  )
}

find_route <- function(routes, req) {
  Find(function(r) is_match(r, req), routes, nomatch = NULL)
}

is_match <- function(rte, req) {
  if (!is.route(rte)) {
    stop('argument `rte` must be of class route', call. = FALSE)
  }

  if (!is.request(req)) {
    stop('argument `req` must be of class request', call. = FALSE)
  }

  grepl(rte[['path']], req[['uri']]) &&
    (req[['method']] %in% rte[['method']] || rte[['method']] == 'all')
}
