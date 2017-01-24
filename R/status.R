#' HTTP Response Status Code
#'
#' Get or set the status code of a response object.
#'
#' @param x A response object.
#' @param value An HTTP status code, \code{1xx} through \code{5xx}, see
#'   \code{\link{response}} for details.
#'
#' @export
#' @examples
#' # create a new response
#' res <- response()
#'
#' # default response status
#' status(res)  # 200
#'
#' # set the response status
#' status(res) <- 301
#' status(res)
#'
status <- function(x) {
  if (!is.response(x)) {
    stop('argument `x` must be of class response', call. = FALSE)
  }
  x[['status_code']]
}

#' @rdname status
#' @export
`status<-` <- function(x, value) {
  if (!is.response(x)) {
    stop('argument `x` must be of class response', call. = FALSE)
  }
  x[['status_code']] <- value
  invisible(x)
}

#' Status Code Reason Phrase
#'
#' Get the corresponding reason phrase for a status code.
#'
#' @param code An HTTP status code.
#'
#' @return
#'
#' The corresponding description of \code{code}, otherwise the empty
#' string.
#'
#' @keywords internal
#' @export
#' @examples
#' reason_phrase(200)
#' reason_phrase('404')
#'
#' reason_phrase(531)
#'
reason_phrase <- function(code) {
  if (!(is.numeric(code) || is.character(code))) {
    stop('argument `code` must be of class numeric or character',
         call. = FALSE)
  }
  switch(
    as.character(code),
    '100' = "Continue",
    '101' = "Switching Protocols",
    '200' = "OK",
    '201' = "Created",
    '202' = "Accepted",
    '203' = "Non-Authoritative Information",
    '204' = "No Content",
    '205' = "Reset Content",
    '206' = "Partial Content",
    '300' = "Multiple Choices",
    '301' = "Moved Permanently",
    '302' = "Found",
    '303' = "See Other",
    '304' = "Not Modified",
    '305' = "Use Proxy",
    '307' = "Temporary Redirect",
    '400' = "Bad Request",
    '401' = "Unauthorized",
    '402' = "Payment Required",
    '403' = "Forbidden",
    '404' = "Not Found",
    '405' = "Method Not Allowed",
    '406' = "Not Acceptable",
    '407' = "Proxy Authentication Required",
    '408' = "Request Timeout",
    '409' = "Conflict",
    '410' = "Gone",
    '411' = "Length Required",
    '412' = "Precondition Failed",
    '413' = "Request Entity Too Large",
    '414' = "Request-URI Too Long",
    '415' = "Unsupported Media Type",
    '416' = "Requested Range Not Satisifable",
    '417' = "Expectation Failed",
    '500' = "Internal Server Error",
    '501' = "Not Implemented",
    '502' = "Bad Gateway",
    '503' = "Service Unavailable",
    '504' = "Gateway Timeout",
    '505' = "HTTP Version Not Supported",
    ""
  )
}
