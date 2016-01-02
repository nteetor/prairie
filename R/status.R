#' HTTP Response Status Code
#' 
#' Get or set the status code of a \code{response} object.
#' 
#' @export
#' @name status
NULL

#' @param x An \R object
#' @export
#' @rdname status
status <- function(x) UseMethod('status')

#' @param value HTTP status code, 1xx through 5xx
#' @export
#' @rdname status
`status<-` <- function(x, value) UseMethod('status<-')

#' @export
#' @rdname status
status.response <- function(x) {
  x$status
}

#' @export
#' @rdname status
`status<-.response` <- function(x, value) {
  x$status_code <- value
  invisible(x)
}

#' Status Code Reason Phrase
#' 
#' Get the corresponding reason phrase for a particular status code.
#' 
#' @param status_code An HTTP status code.
#' 
#' @return
#' 
#' If \code{status_code} is not found the empty string is returned.
#'
#' @keywords internal
#' @export
#' @examples
#' reason_phrase(200)
#' reason_phrase('404')
#' 
#' reason_phrase(531)
reason_phrase <- function(status_code) {
  assert_that(is.numeric(status_code) || is.character(status_code))
  switch(
    as.character(status_code),
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
