#' Request Header Fields
#'
#' To get the values of request header fields \code{[} or \code{[[} may be used
#' to get a single or multiple values respectively. Request field names are
#' case-sensitive.
#'
#' @param x A \code{request} object.
#' @param field An HTTP request header field name.
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
#' mkup_checkin <- mockup(
#'   route(
#'     'POST',
#'     '^$',
#'     function(req) {
#'       print(req[['Accept']])
#'       print(req[['From']])
#'
#'       response()
#'     }
#'   )
#' )
#'
#' # More interesting output
#' mkup_checkin(
#'   'POST',
#'   '/',
#'   headers = list(
#'     Accept = 'text/html',
#'     From = 'Russia w/ Love'
#'   )
#' )
#'
`[.request` <- function(x, field) {
  if (!is.character(field)) {
    stop('argument `field` must be of class character', call. = FALSE)
  }

  if (length(field) != 1) {
    stop('argument `field` must be a character string', call. = FALSE)
  }

  if (field %in% c('Referer', 'Referrer')) {
    x[['headers']][['Referer']] %||% x[['headers']][['Referrer']]
  } else {
    x[['headers']][[field]]
  }
}
