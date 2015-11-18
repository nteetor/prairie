#' Get an HTTP Request
#' 
#' Request objects, yeah
#'
#' @export
#' @name request
request <- function() {
  if (exists('request_environment', sys.frame(1), inherits = FALSE)) {
    envir <- get('request_environment', sys.frame(1))
  } else {
    envir <- list()
  }
  request__$new(envir)
}