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
#' @seealso
#' 
#' Additional \code{request} functions: \code{\link{body}}, \code{\link{host}}, 
#' \code{\link{type}}, \code{\link{url}}
#' 
#' For more information about HTTP requests please refer to the 
#' \href{http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html}{Request}
#' section of www.w3.org.
#' 
#' @export
#' @name body
#' @examples
#' route(
#'   'post',
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
#'     
#'     # set body of response as data.frame
#'     # coerced to JSON string with as.JSON
#'     body(res) <- mogrified
#'     
#'     res
#'   }
#' )
NULL

body <- function(x, ...) UseMethod('body')
`body<-` <- function(x, value) UseMethod('body<-')

#' @param req A \code{request} object.
#' @export
#' @rdname body
body.request <- function(req) {
  req$body
}

#' @param res A \code{response} object.
#' @export
#' @rdname body
body.response <- function(res) {
  res$get_body()
}

#' @param value Character vector to set as message body of \code{res}.
#' @export
#' @rdname body
`body<-.response` <- function(res, value) {
  # TODO
  # value <- as.JSON(value)
  res$add_body(value)
}
