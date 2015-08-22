#' Create a new dull application object
#' 
#' Use to start building a dull application. The dull applicatoin object
#' encapsulates the routes, callback functions, and [ADD MORE] that comprise a
#' web application.
#' 
#' @section Usage 
#' \code{dull()}
#'   
#' @section Details 
#' See \link{dull_app} for full details of the underlying R6 object of the dull
#' application.
#' 
#' @section Value
#' A \code{dull_app} R6 class object.
#'   
dull <- function() {
  dull_app$new()
}
