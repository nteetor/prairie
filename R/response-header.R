#' HTTP Response Header Fields
#' 
#' Within prairie, getting and setting the fields of the \code{\link{response}} 
#' header is much like assigning values to lists. One can use \code{[}, 
#' \code{[[} to extract one or more fields or replace a single 
#' field.
#' 
#' @param res A \code{response} object
#' @param name HTTP response header field(s), case sensitive
#'   
#' @return
#' 
#' A list of corresponding header field values. If \code{name} is not set for 
#' \code{res} then \code{NULL} is returned.
#' 
#' @seealso
#' 
#' For more information regarding specific HTTP response header fields refer to 
#' \url{http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html}.
#' 
#' @export
#' @name response-headers
#' @examples
#' # create new response
#' res <- response()
#' 
#' res[["Content-Type"]] <- "text/html"
`[.response` <- function(res, name) sapply(name, res$get, simplify = FALSE, USE.NAMES = TRUE)

#' @export
#' @rdname response-headers
`[[.response` <- function(res, name) res$get(name)

#' @param value Value to assign to field \code{name}
#' @export
#' @rdname response-headers
`[[<-.response` <- function(res, name, value) res$set(name, value)
