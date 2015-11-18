#' HTTP Response Headers
#' 
#' Within prairie, getting and setting the fields of the \code{\link{response}} 
#' header is much like assigning values to lists. One can use \code{[}, 
#' \code{[[}, and \code{$} to extract one or more fields or replace a single 
#' field.
#' 
#' @param res A \code{response} object
#' @param name HTTP response header field(s), case sensitive
#'   
#' @details
#' 
#' Use \code{[[} to access fields with non-alphanumeric characters, such as 
#' \emph{Content-Type} which contains \emph{-}. If your heart is set on 
#' accessing fields with \code{$} backticks may also be used.
#' 
#' @return
#' 
#' A named character vector of field values, where the item names are
#' \code{name}. If \code{res} was missing field \code{name} then \code{NULL} is
#' returned instead.
#' 
#' @seealso
#' 
#' For more information regarding specific HTTP response header fields refer to 
#' \url{http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html}.
#' 
#' @export
#' @name response-headers
#' @examples
#' res <- response()
#' 
#' # equivalent
#' res[["Content-Type"]] <- "text/plain"
#' res$`Content-Type` <- "text/plain"
`[.response` <- function(res, name) sapply(name, res$get, simplify = FALSE, USE.NAMES = TRUE)

#' @export
#' @rdname response-headers
`[[.response` <- function(res, name) res$get(name)

#' @param value Value to assign to field \code{name}
#' @export
#' @rdname response-headers
`[[<-.response` <- function(res, name, value) res$set(name, value)

#' @export
#' @rdname response-headers
`$.response` <- function(res, name) res$get(name)

#' @export
#' @rdname response-headers
`$<-.response` <- function(res, name, value) res$set(name, value)
