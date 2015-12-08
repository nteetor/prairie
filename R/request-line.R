#' HTTP Request Host
#' 
#' The host portion of the Host request header field.
#' 
#' @param req A \code{request} object
#'   
#' @seealso \code{\link{body}}, \code{\link{host}}, \code{\link{type}},
#'   \code{\link{url}}
#'   
#' @export
#' @name host
#' @examples
#' route(
#'   'get',
#'   '^.*',
#'   function() {
#'     req <- request()
#'     res <- response()
#'     
#'     status(res) <- 200
#'     body(res) <- paste0('<p>Hey there ', host(req), '!</p>')
#'     res[['Content-Type']] <- 'text/html'
#'     
#'     res
#'   })
host <- function(req) {
  assert_that(is.request(req))
  req$host_name
}

#' HTTP Request URL
#' 
#' Get the URL for a request.
#' 
#' @param req A \code{request} object
#'   
#' @seealso \code{\link{body}}, \code{\link{host}}, \code{\link{type}},
#'   \code{\link{url}}
#'   
#' @export
#' @name url
url <- function(req) {
  assert_that(is.request(req))
  req$url
}