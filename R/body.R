#' HTTP Message Body
#'
#' Get the message body of a request or response or set the body of a response.
#'
#' @param x An \R object.
#' @param value The response body, if JSON the \code{Content-Type} is set to
#'   \code{application/json}.
#'
#' @details
#'
#' For more information about HTTP requests please refer to the
#' \href{http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html}{Request} section
#' of www.w3.org.
#'
#' @return
#'
#' \code{body} resturns a character string if the request or response contains a
#' message body, otherwise \code{NULL}.
#'
#' \code{body<-} invisibly returns the response.
#'
#' @export
#' @examples
#' transmog <- route(
#'   'POST',
#'   '^transmogrify/json$',
#'   function(req) {
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
body <- function(x) {
  if (!(is.request(x) || is.response(x))) {
    stop('cannot get body of a ', class(x), call. = FALSE)
  }
  unclass(x)[['body']]
}

#' @rdname body
#' @export
`body<-` <- function(x, value) {
  if (!is.response(x)) {
    stop('cannot set the body of a ', class(x), call. = FALSE)
  }

  orig <- class(x)
  x <- unclass(x)

  x[['body']] <- value

  if (is.json(value)) {
    x[['headers']][['Content-Type']] <- 'application/json'
  }

  class(x) <- orig

  invisible(x)
}
