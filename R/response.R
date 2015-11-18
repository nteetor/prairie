#' Create an HTTP response
#' 
#' A response object represents the HTTP response returned by a route handler.
#' 
#' @seealso \code{\link{route}}, \code{\link{req}}
#'   
#' @export
#' @name response
#' @examples
#' # simple 301 response
#' res_301 <- resposne()
#'     
#' res[['Connection']] <- 'close'
#' status(res_301) <- 301
#' body(res_301) <- 'Sorry, this page has moved'
#' 
#' # simple 404 response
#' res_404 <- response()
#' 
#' res[['Connection']] <- 'close'
#' status(res_404) <- 404
#' body(res_404) <- 'Sorry, page not found'
response <- function() {
  response__$new()
}
