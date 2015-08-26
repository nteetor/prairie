#' dull, building web applications too easily
#' 
#' \code{dull} provides a straight-forward syntax to build web applications in 
#' R. Functions like \link{get}, \link{post}, and \link{put} add route callbacks
#' to a \link{dull} application.
#' 
#' @details 
#' For information regarding route callbacks and route URI specification refer
#' to \link{dull}.
#' 
#' @docType package
#' @name dull-package
NULL

#' Create a new dull application object
#' 
#' Create a dull application object. The dull application object
#' encapsulates route URIs, callback functions, and [ADD MORE] that together form a web application.
#'   
#' @details 
#' See \link{dull_app} for full details of the underlying R6 object of the dull
#' application.
#' 
#' @return 
#' A \code{dull_app} R6 class object.
#' 
#' @export
dull <- function() {
  dull_app$new()
}
