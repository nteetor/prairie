#' HTTP Message Body
#' 
#' Get the message body of a \code{request} or \code{response} object. The body 
#' of \code{response} objects may be set.
#' 
#' @return
#' 
#' A character string if the request contains a message body, otherwise 
#' \code{NULL}.
#' 
#' @details
#' 
#' The default versions of both functions call the base version.
#' 
#' @seealso
#' 
#' For more information about HTTP requests please refer to the 
#' \href{http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html}{Request} section
#' of www.w3.org.
#' 
#' @export
#' @name body
#' @examples
#' transmog <- route(
#'   'POST',
#'   '^transmogrify/json$',
#'   function() {
#'     req <- request()
#'     res <- response()
#' 
#'     # get body of request
#'     raw_body <- body(req)
#'     data <- jsonlite::fromJSON(raw_body)
#' 
#'     mogrified <- lapply(
#'       data,
#'       function(col) paste0(as.character(col), '!!!')
#'     )
#' 
#'     status(res) <- 200
#'     body(res) <- as.json(mogrified)
#' 
#'     res
#'   }
#' )
NULL

#' @param x An \R object.
#' @export
#' @rdname body
body <- function(x) UseMethod('body')

#' @param value An \R object, complex objects are converted to JSON, see details.
#' @export
#' @rdname body
`body<-` <- function(x, value) UseMethod('body<-')

#' @export
#' @rdname body
body.request <- function(x) {
  x$body
}

#' @export
#' @rdname body
body.response <- function(x) {
  x$body
}

#' @export
#' @rdname body
`body<-.response` <- function(x, value) {
  if (is.json(x)) x[['Content-Type']] <- 'application/json'
  x$body <- value
  invisible(x)
}

#' @export
body.default <- base::body

#' @export
`body<-.default` <- base::`body<-`
