#' Create application
#' 
#' An application in prairie is simply defined as \emph{a list of 
#' \link{routes}}. The application handles dispatching HTTP requests to routes 
#' and sending back HTTP responses. Route handlers are pure functions and 
#' therefore for a given HTTP request an application will always return the same
#' HTTP response.
#' 
#' @param \ldots Route objects
#' 
#' @details
#' 
#' 
#'   
#' @export
#' @name app
app <- function(...) {
  routes <- lapply(list(...), as.route)
  
  application__$new(routes)  
}

#' Run application
#'
#' Starts \code{application} at \code{host} on \code{port}. 
#'
#' @export
#' @name start
start <- function(application, host, port) {
  assert_that(is.application(application), is.character(host), is.numeric(port))
  application$listen(host, port)
}