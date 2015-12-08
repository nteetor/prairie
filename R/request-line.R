#' HTTP Request Line
#' 
#' @param x A \code{request} object.
#' 
#' @name request line
NULL

#' @export
#' @rdname request line
method <- function(x) {
  assert_that(is.request(x))
  x$method
}

#' @export
#' @rdname request line
url <- function(x) {
  assert_that(is.request(x))
  x$url
}

#' @export
#' @rdname request line
protocol <- function(x) {
  assert_that(is.request(x))
  x$protocol
}
