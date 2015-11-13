#' Create application
#' 
#' Applications in prairie are naively defined as a \emph{list of 
#' \link{routes}}. The application handles dispatching HTTP requests to the 
#' routes and responding with HTTP responses. However, response building happens
#' within each route.
#' 
#' @param \ldots Route objects
#' @param host host address to run the application on
#' @param port port to run the application on
#' 
#' @export
#' @name app
app <- function(...) {
  assertthat::assert_that(all(vapply(list(...), is.application, logical(1))))
  application__(list(...))  
}

#' Run application
#'
#' Starts \code{application} on \code{host}
#'
#' @export
start <- function(application, host, port) {
  assertthat::assert_that(is.application(application), is.character(host), is.numeric(port))
  application$listen(host, port)
}