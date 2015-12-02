#' Get an HTTP Request
#' 
#' Request objects, yeah
#'
#' @export
#' @name request
request <- function() {
  if (exists('__request_environment', sys.frame(1), inherits = FALSE)) {
    envir <- get('__request_environment', sys.frame(1))
  } else {
    envir <- list()
  }
  request__$new(envir)
}
