#' HTTP Response Header Fields
#' 
#' Within prairie, getting and setting the fields of the \code{\link{response}} 
#' header is much like assigning values to lists. One can use \code{[}, 
#' \code{[[} to extract one or more fields or replace a single 
#' field.
#' 
#' @return
#' 
#' A list of corresponding header field values. If \code{field} is not set for 
#' \code{res} then \code{NULL} is returned.
#' 
#' @seealso
#' 
#' For more information regarding specific HTTP response header fields refer to 
#' \url{http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html}.
#' 
#' @name response-headers
#' @examples
#' # create new response
#' res <- response()
#' 
#' # add single field to header
#' res[["Connection"]] <- "keep-alive"
#' 
#' # add multiple fields in one go
#' res[] <- list(
#'   Date = Sys.time(),
#'   Server = 'R/prairie'
#' )
#' 
#' res
NULL

#' @param x A \code{response} object.
#' @param field An HTTP response header field name.
#' @export
#' @rdname response-headers
`[[.response` <- function(x, field) x$get(field)

#' @param value Value to assign to \code{field}.
#' @export
#' @rdname response-headers
`[[<-.response` <- function(x, field, value) x$set(field, value)

#' @export 
#' @rdname response-headers
`[.response` <- function(x, field) x$get_all(field)

#' @export
#' @rdname response-headers
`[<-.response` <- function(x, field, value) {
  if (missing(field)) {
    assert_that(is_named(value))
    
    x$set_all(names(value), value)
  } else {
    x$set_all(field, value)    
  }
}

