#' HTTP Response Status Code
#' 
#' Get or set the status code of a \code{response} object.
#' 
#' @export
#' @name status
NULL

#' @param x An \R object
#' @export
#' @rdname status
status <- function(x) UseMethod('status')

#' @param value HTTP status code, 1xx through 5xx
#' @export
#' @rdname status
`status<-` <- function(x, value) UseMethod('status<-')

#' @export
#' @rdname status
status.response <- function(x) {
  x$status
}

#' @export
#' @rdname status
`status<-.response` <- function(x, value) {
  x$status <- value
  invisible(x)
}
