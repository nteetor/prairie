#' Request Header Fields
#' 
#' To get the values of request header fields [ or [[ may be used to get a 
#' single or multiple values respectively. Request field names are 
#' case-insensitive, so \code{"Accept"} and \code{"accept"} are equivalent.
#' 
#' @details
#' 
#' For more information regarding specific HTTP request header fields please
#' refer to \url{https://tools.ietf.org/html/rfc2616#section-5.3}.
#' 
#' @name request-headers
#' @examples
#' req <- request()
#' 
#' req[['Accept']] # NULL
#' req[['From']]   # NULL
#' 
#' route(
#'   'POST',
#'   '^$',
#'   function(req) {
#'     # Now there will be values
#'     print(req[['Accept']])
#'     print(req[['From']])
#'     
#'     response()
#'   }
#' )
NULL


#' @param x A \code{request} object.
#' @param field An HTTP request header field name.
#' @export
#' @rdname request-headers
`[[.request` <- function(x, field) {
  assert_that(is.character(field))
  
  if (field %in% c('Referer', 'Referrer')) {
    x$headers$referer %||% x$headers$referrer
  } else {
    x$headers[[tolower(field)]]
  }
}

#' @export
#' @rdname request-headers
`[.request` <- function(x, field) {
  assert_that(is.character(field))
  
  lapply(field, function(f) x[[f]])
}
