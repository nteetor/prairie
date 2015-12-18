#' Mockup a Route
#' 
#' Creates a mockup of a route object. A mockup simulates a route's response to
#' a particular method and resource combination.
#' 
#' @export
#' @name mockup
#' @examples
#' logger <- route(
#'   'GET',
#'   '^',
#'   function(req) {
#'     print(req)
#'     
#'     response()
#'   }
#' )
#' 
#' logger_m <- mockup(logger)
#' logger_m('GET', '/yellow/brick/path')
#' logger_m('GET', '/pho', headers = list(Accepts = 'text/html'))
NULL

#' @param x Any \R object
#' @export
#' @rdname mockup
mockup <- function(x) UseMethod('mockup')

#' @export
#' @rdname mockup
mockup.route <- function(x) {
  structure(
    function(method, resource, headers = list()) {
      e <- new.env(parent = baseenv())
      e$REQUEST_METHOD <- method
      e$PATH_INFO <- resource
      lapply(headers, function(h) paste0('HTTP_', h))
      req <- as.request(e)
      res <- x$handler(req)
      if (!is.response(res)) {
        warning(quote(x), 'returned object of class', class(res), 'instead of response')
      }
      res
    },
    class = c('mockup', class(x))
  )
}