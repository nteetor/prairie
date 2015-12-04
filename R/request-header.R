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

#' HTTP Request Content Type
#' 
#' Get the content types of a request.
#' 
#' @param req A \code{request} object
#'   
#' @seealso \code{\link{body}}, \code{\link{host}}, \code{\link{type}},
#'   \code{\link{url}}
#'   
#' @export
#' @name type
type <- function(req) {
  assert_that(is.request(req))
  req$get('Content-Type')
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

#' HTTP Request Header Fields
#' 
#' Get and header field values for \code{request} objects.
#' 
#' @details
#' 
#' For more information regarding specific HTTP request header fields refer to 
#' \url{http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html}.
#' 
#' @name request-headers
#' @examples
#' req <- request()
#' 
#' req[['Accept']] <- 'text/*'
#' req[['From']] <- '127.0.0.1'
NULL

#' @param x A \code{request} object.
#' @param field An HTTP request header field name.
#' @export
#' @rdname request-headers
`[.request` <- function(x, field) sapply(field, x$get, simplify = FALSE, USE.NAMES = TRUE)

#' @export
#' @rdname request-headers
`[[.request` <- function(x, field) x$get(field)

#' @param value Value to assign to \code{field}.
#' @export
#' @rdname request-headers
`[[<-.request` <- function(x, field, value) x$set(field, value)
