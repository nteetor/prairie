#' Test dull applications
#'
#' A set of functions that expand upon the \code{expect_*} functions of the
#' \code{testthat} package.
#'
#' @param app A dull application object
#' @param method A character vector of the HTTP method with which to query
#'   \code{url}
#' @param uri A character vector of the resource path to query
#' @param status An integer, the expected status
#' @param body A character vector, the expected response body
#' @param \dots Additional parameters to pass to \code{\link[httr]{VERB}}
#'
#' @details These functions are currently very limited and would be considered
#'   experimental if not for the \code{testthat} package. There is plenty more
#'   to come in the future.
#'
#'   \code{uri} must begin with a \dQuote{/} and is used to construct the
#'   complete URL queried. The host name and port used are extracted from
#'   \code{app}.
#'
#' @name sharpen
NULL

#' @export
#' @rdname sharpen
expect_status <- function(app, method, uri, status, ...) {
  if (!requireNamespace('httr')) {
    stop('package httr must be installed in order to use `expect_response`')
  }
  if (!requireNamespace('testthat')) {
    stop('package testthat must be installed in order to use `expect_response`')
  }

  default_host <- '127.0.0.1'
  default_port <- 3030

  app_handle <- httpuv::startDaemonizedServer(
    host = app$host %||% default_host,
    port = app$port %||% default_port,
    app = app
  )

  on.exit(httpuv::stopDaemonizedServer(app_handle))

  url <- paste0(app$host %||% default_host, ':', app$port %||% default_port, uri)
  res <- httr::VERB(method, url = url, ...)

  eval(bquote(testthat::expect_equal(.(httr::status_code(res)), status)))
}

#' @export
#' @rdname sharpen
expect_body <- function(app, method, uri, body, ...) {
  if (!requireNamespace('httr')) {
    stop('package httr must be installed in order to use `expect_body`')
  }
  if (!requireNamespace('testthat')) {
    stop('package testthat must be installed in order to use `expect_response`')
  }

  default_host <- '127.0.0.1'
  default_port <- 3030

  app_handle <- httpuv::startDaemonizedServer(
    host = app$host %||% default_host,
    port = app$port %||% default_port,
    app = app
  )

  on.exit(httpuv::stopDaemonizedServer(app_handle))

  url <- paste0(app$host %||% default_host, ':', app$port %||% default_port, uri)
  res <- httr::VERB(method, url = url, ...)

  eval(bquote(testthat::expect_equal(.(httr::content(res, as = 'text')), body)))
}
