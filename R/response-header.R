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
`[[.response` <- function(x, field) {
  assert_that(is.character(field))
  x$headers[[field]]
}

#' @param value Value to assign to \code{field}.
#' @export
#' @rdname response-headers
`[[<-.response` <- function(x, field, value) {
  assert_that(is.character(field))
  x$headers[[field]] <- value
  invisible(x)
}

#' @export 
#' @rdname response-headers
`[.response` <- function(x, field) {
  assert_that(is.character(field))
  sapply(field, function(f) x$headers[[f]], simplify = FALSE, USE.NAMES = TRUE)
}

#' @export
#' @rdname response-headers
`[<-.response` <- function(x, field, value) {
  if (missing(field)) {
    assert_that(is_named(value))
    
    x$headers <- value
  } else {
    assert_that(is.character(field), length(field) == length(value))
    
    x$headers <- setNames(value, field)
  }
  
  invisible(x)
}

