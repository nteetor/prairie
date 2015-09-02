#' Test a dull application
#' 
#' A set of functions to expand upon the \code{expect_*} functions of the 
#' \code{testthat} package.
#' 
#' @param .app A dull application
#' @param method A character vector of the HTTP method with which to query 
#'   \code{url}
#' @param uri A character vector of the resource path to query
#' @param status An integer, the expected status
#' @param \dots Additional parameters to pass to \code{\link[httr]{VERB}}
#'   
#' @details 
#' These functions are currently very limited and would be considered
#' experimental if not for the \code{testthat}. There is plenty more to come in the
#' future. 
#' 
#' \code{uri} must begin with a \dQuote{/} and is used to construct the complete
#' URL queried. The host name and port used are extracted from the \code{.app}
#' object.
#' 
#' @name sharpen
NULL

#' @importFrom httpuv startDaemonizedServer stopDaemonizedServer
#' @importFrom httr VERB
#' @importFrom testthat expect_equal
#' @export
#' @rdname sharpen
expect_response <- function(.app, method, uri, status, ...) {
  app_handle <- httpuv::startDaemonizedServer(host = .app$host, port = .app$port, app = .app)
  
  on.exit(httpuv::stopDaemonizedServer(app_handle))
  
  url <- paste0(.app$host, ':', .app$port, uri)
  res <- httr::VERB(method, url = url, ...)
  
  eval(bquote(testthat::expect_equal(.(httr::status_code(res)), status)))
}

#' @export
#' @rdname sharpen
expect_get <- function(.app, uri, status, ...) {
  expect_response(.app, 'GET', uri, status, ...)
}

#' @export
#' @rdname sharpen
expect_post <- function(.app, uri, status, ...) {
  expect_response(.app, 'POST', uri, status, ...)
}

#' @export
#' @rdname sharpen
expect_put <- function(.app, uri, status, ...) {
  expect_response(.app, 'PUT', uri, status, ...)
}
