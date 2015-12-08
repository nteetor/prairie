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
#' 
#' req[] <- list(
#'  Connection = 'close',
#'  Date = Sys.time()
#' )
#' 
#' req
NULL


#' @param x A \code{request} object.
#' @param field An HTTP request header field name.
#' @export
#' @rdname request-headers
`[[.request` <- function(x, field) x$get(field)

#' @param value A value to assign to \code{field}.
#' @export
#' @rdname request-headers
`[[<-.request` <- function(x, field, value) x$set(field, value)

#' @export
#' @rdname request-headers
`[.request` <- function(x, field) x$get_all(field)

#' @export
#' @rdname request-headers
`[<-.request` <- function(x, field, value) {
  if (missing(field)) {
    assert_that(is_named(value))
    x$set_all(names(value), value)
  } else {
    x$set_all(field, value)
  }
  invisible(x)
}
