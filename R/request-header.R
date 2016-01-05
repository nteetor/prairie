#' Request Header Fields
#' 
#' To get the values of request header fields [ or [[ may be used to get a 
#' single or multiple values respectively. Request field names are 
#' case-sensitive.
#' 
#' @details
#' 
#' For more information regarding HTTP request header fields please refer to
#' \url{https://tools.ietf.org/html/rfc2616#section-5.3}.
#' 
#' @name request-headers
#' @examples
#' req <- request()
#' 
#' req[['Accept']] # NULL
#' req[['From']]   # NULL, boring
#' 
#' checkin <- route(
#'   'POST',
#'   '^$',
#'   function(req) {
#'     print(req[['Accept']])
#'     print(req[['From']])
#'     
#'     response()
#'   }
#' )
#' 
#' checkin_m <- mockup(checkin)
#' 
#' # More interesting output
#' checkin_m(
#'   'POST', 
#'   '/', 
#'   headers = list(
#'     Accept = 'text/html', 
#'     From = 'Russia w/ Love'
#'   )
#' )
NULL


#' @param x A \code{request} object.
#' @param field An HTTP request header field name.
#' @export
#' @rdname request-headers
`[[.request` <- function(x, field) {
  assert_that(is.character(field))
  
  if (field %in% c('Referer', 'Referrer')) {
    x$headers$Referer %||% x$headers$Referrer
  } else {
    x$headers[[field]]
  }
}

#' @export
#' @rdname request-headers
`[.request` <- function(x, field) {
  assert_that(is.character(field))
  
  setNames(lapply(field, function(f) x[[f]] %||% NA), field)
}
