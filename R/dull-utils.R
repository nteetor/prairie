#' Add routes
#'
#' Add a route to a dull application object. A route is comprised of an HTTP
#' method, a URI, and a callback function.
#'
#' @param .app A \code{dull_app} R6 class object
#' @param method An HTTP method, see section \bold{HTTP} Methods for details
#' @param uri The route URI, treated as a regular expression, see section URIs
#' @param callback A function of two arguments \code{request} and \code{response}, see section Callback functions for details
#'
#' @section HTTP Methods:
#' TODO
#'
#' @section URIs:
#' TODO
#'
#' @name adding-routes
NULL

#' @rdname adding-routes
#' @export
method <- function(.app, method, uri, callback) {
  .app$add_route(method, uri, callback)
}

#' @rdname adding-routes
#' @export
get <- function(.app, uri, callback) {
  method(.app, 'GET', uri, callback)
}

#' @rdname adding-routes
#' @export
post <- function(.app, uri, callback) {
  method(.app, 'POST', uri, callback)
}

#' @rdname adding-routes
#' @export
put <- function(.app, uri, callback) {
  method(.app, 'PUT', uri, callback)
}

#' Listen for connections
#'
#' Start a dull application listening for connections on a specified host and
#' port
#'
#' @param .app A \code{dull_app} R6 class object
#' @param host A valid IPv4 address as a string, use \sQuote{0.0.0.0} to broadcast on all addresses
#' @param port A string or number identifying the server port to listen on
#'
#' @export
listen <- function(.app, host, port) {
  .app$run(host, port)
}

#' @importFrom magrittr %>%
#' @name %>%
#' @export
#' @rdname chain
#' @usage LHS \%>\% RHS
NULL
