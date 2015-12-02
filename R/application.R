#' Create an application
#' 
#' An application in prairie is defined most basically as \emph{a list of 
#' \link[=route]{routes}}. The application handles dispatching HTTP requests to routes 
#' and sending back HTTP responses. Route handlers are pure functions, therefore
#' for a given HTTP request an application will always return the same HTTP
#' response.
#' 
#' @param \ldots Routes or objects to coerce to routes.
#'   
#' @export
#' @name app
app <- function(...) {
  routes <- lapply(list(...), as.route)
  
  application__$new(routes)  
}

#' Run Prairie Application
#'
#' Starts an \code{\link{app}} at \code{host} on \code{port}. 
#' 
#' @param app An \code{application} object.
#' @param host A character vector specifying the host name.
#' @param port A numeric specifying the port number.
#'
#' @export
#' @name start
start <- function(app, host, port) {
  assert_that(is.application(app), is.character(host), is.numeric(port))
  
  app$listen(host, port)
}