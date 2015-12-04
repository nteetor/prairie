#' Get an HTTP Request
#' 
#' Request objects, yeah
#'
#' @export
#' @name request
request <- function(envir) {
  if (missing(envir)) {
    envir <- list()
  }
  request__$new(envir)
}
