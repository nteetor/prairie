#' Get an HTTP Request
#' 
#' Request objects, yeah
#' 
#' @param envir An optional environment to create the request from.
#' 
#' @details
#' 
#' The \code{envir} argument is necessary for testing and other fringe cases.
#' Typically a \code{request} object is implicity instantiated from the Rook
#' request environment \code{httpuv} passes to the \code{call} function. See the
#' \code{httpuv::startServer} documentation for more information.
#'
#' @export
#' @name request
request <- function(envir) {
  if (missing(envir)) {
    if (exists('__request_environment', sys.frame(1), inherits = FALSE)) {
      envir <- get('__request_environment', sys.frame(1))
    } else {
      envir <- list()
    }
  }
  request__$new(envir)
}
