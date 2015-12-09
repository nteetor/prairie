#' HTTP Request Line
#' 
#' Get information about a request such as method type, url of the requested
#' resource, or the requset HTTP protocol.
#' 
#' @param x A \code{request} object.
#' 
#' @name request-line
#' @examples
#' # A simple route to catch and log all incoming requests
#' route(
#'  'ALL',
#'  '^',
#'  function() {
#'    req <- request()
#'    res <- response()
#'    
#'    print(paste('[ LOG ]', Sys.time(), method(req), url(req), protocol(req)))
#'    
#'    body(res) <- 'Thanks for checking in'
#'    
#'    res
#'  }
#' )
NULL

#' @export
#' @rdname request-line
method <- function(x) {
  assert_that(is.request(x))
  x$method
}

#' @export
#' @rdname request-line
url <- function(x) {
  assert_that(is.request(x))
  x$url
}

#' @export
#' @rdname request-line
protocol <- function(x) {
  assert_that(is.request(x))
  x$protocol
}
