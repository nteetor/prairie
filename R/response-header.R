#' HTTP Response Header Fields
#'
#' Within prairie, getting and setting the fields of the \code{\link{response}}
#' header is much like assigning values to lists. One can use \code{[} and
#' \code{[[} to extract one or more fields or replace a single
#' field.
#'
#' @param x A response object.
#' @param field An HTTP response header field name.
#'
#' @details
#'
#' For more information regarding specific HTTP response header fields refer to
#' \url{http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html}.
#'
#' @return
#'
#' A list of corresponding header field values. If the response does not contain
#' a certain field then \code{NULL} is returned.
#'
#' @name response-headers
#' @export
#' @examples
#' # create new response
#' res <- response()
#'
#' # add single field to header
#' res["Connection"] <- "keep-alive"
#'
#' # add multiple fields at once
#' res[] <- list(
#'   Date = Sys.time(),
#'   Server = 'R/prairie'
#' )
#'
#' res
#'
`[.response` <- function(x, field) {
  if (!is.character(field)) {
    stop('argument `field` must be of class character', call. = FALSE)
  }

  if (length(field) != 1) {
    stop('argument `field` must be a single character string', call. = FALSE)
  }

  x[['headers']][[field]]
}

#' @rdname response-headers
#' @export
`[<-.response` <- function(x, field, value) {
  if (missing(field)) {
    if (!is_named(value)) {
      stop('argument `value` must be a named list', call. = FALSE)
    }

    x[['headers']] <- value

  } else {
    if (!is.character(field)) {
      stop('argument `field` must be of class character', call. = FALSE)
    }

    if (length(field) != 1) {
      stop('argument `field` must be a character string', call. = FALSE)
    }

    if (inherits(x, 'Date') || inherits(x, 'POSIXt')) {
      value <- http_date(value)
    }

    x[['headers']][[field]] <- value
  }

  invisible(x)
}
