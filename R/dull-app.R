#' Create a new dull application object
#' 
#' Create a dull application object. The dull application object
#' encapsulates route URIs, callback functions, and [ADD MORE] that together form a web application.
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
