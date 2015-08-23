#' Add routes
#' 
#' Add a route to a dull application object. A route is comprised of an HTTP
#' method, a URI, and a callback function.
#' 
#' @section Usage
#' \code{method(.app, method, uri, callback)}
#' \code{get(.app, uri, callback)}
#' \code{post(.app, uri, callback)}
#' \code{put(.app, uri, callback)}
#' 
#' @param .app A dull_app R6 class object
#' @param method An HTTP method, see section \bold{HTTP} Methods for details
#' @param uri The route URI, treated as a regular expression, see section URIs
#' @param callback A function of two arguments \code{request} and \code{response}, see section Callback functions for details
#' 
#' @section HTTP Methods
#' TODO
#' 
#' @section URIs
#' TODO
#' 
#' @section Callback functions
#' TODO
#' 
NULL

method <- function(.app, method, uri, callback) {
  .app$add_route(method, uri, callback)
}

get <- function(.app, uri, callback) {
  method(.app, 'GET', uri, callback)
}

post <- function(.app, uri, callback) {
  method(.app, 'POST', uri, callback)
}

put <- function(.app, uri, callback) {
  method(.app, 'PUT', uri, callback)
}

#'
#'
#'
#'
listen <- function(.app, host, port) {
  .app$run(host, port)
}