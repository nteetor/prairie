#' Create an HTTP Request
#'
#' Request objects store relevant information sent from the client to the server
#' as a part of an HTTP request. Request objects are not typically explicitly
#' created. Instead, a request object is passed as an argument to a route
#' handler.
#'
#' @details
#'
#' Request objects contain the following information.
#'
#' \subsection{method:}{
#'
#' Most often \code{GET} or \code{POST}, the method indicates what action to
#' take for a specified resource. This value may be accessed with
#' \code{\link{method}}.
#'
#' }
#'
#' \subsection{uri:}{
#'
#' The uri indicates the server resource requested by the client. A request
#' object's uri may be accessed with \code{\link{uri}}.
#'
#' }
#'
#' \subsection{query:}{
#'
#' A request query is set of key value pairs following the uri. A query is
#' indicated by a ? and is, optionally, ended with a #. Query keys are case-
#' sensitive. A request object's query list may be accessed with \link{query}.
#' If an incoming request does not have a query string then \code{query} will
#' return an empty list.
#'
#' }
#'
#' \subsection{headers:}{
#'
#' Request header fields may be accessed by treating a request object like a
#' list. Using [ or [[, one can get a single or multiple header field values.
#' Header fields are case-insensitive.
#'
#' }
#'
#' \subsection{body:}{
#'
#' The body message of a request object may be retreived with \link{body}.
#'
#' }
#'
#' @export
#' @examples
#' # not much to see here
#' req <- request()
#' print(req)
#'
#' # the request object is loaded with information
#' # from the client
#' printreq <- route(
#'   'GET',
#'   '^/print/request$',
#'   function(req) {
#'     print('Request received:')
#'     print(req)
#'
#'     response()
#'   }
#' )
#'
#' # create mockup
#' printreq_m <- mockup(printreq)
#'
#' # now there's something to see
#' printreq_m('get', '/print/request')
#' printreq_m('get', '/print/request',
#'            headers = list(
#'              Accept = 'text/html',
#'              Host = 'with the most'
#'            )
#' )
request <- function() {
  structure(
    list(
      method = NULL,
      uri = NULL,
      query = NULL,
      headers = list(`Content-Type` = 'plain/text'),
      body = ''
    ),
    class = 'request'
  )
}

#' @rdname request
#' @export
is.request <- function(x) {
  inherits(x, 'request')
}

#' Printing Requests
#'
#' Print a request object.
#'
#' @param x A request object.
#' @param \ldots Ignored.
#'
#' @seealso \code{\link{request}}
#'
#' @keywords internal
#' @export
#' @examples
#' print(request())
#'
print.request <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

#' @rdname print.request
#' @export
format.request <- function(x, ...) {
  str_m <- x[['method']] %||% 'NULL'
  str_u <- x[['uri']] %||% 'NULL'
  str_q <- x[['query']] %||% 'NULL'
  str_b <- if (x[['body']] == '') '""' else x[['body']]
  str_h <- paste0(names(x[['headers']]), ': ', ifelse(is.date(x[['headers']]),
                                                      http_date(x[['headers']]),
                                                      x[['headers']]))

  width <- max(nchar(str_m), nchar(str_u), nchar(str_q), nchar(str_h))
  frmt <- paste0('%', width, 's')

  formatted <- c(
    'A request:',
    paste(sprintf(frmt, str_m), '<method>'),
    paste(sprintf(frmt, str_u), '<uri>'),
    paste(sprintf(frmt, str_q), '<query>'),
    paste(sprintf(frmt, str_h), '<header>'),
    paste(sprintf(frmt, str_b), '<body>')
  )

  paste('#', formatted, collapse = '\n')
}

#' Coerce Rook Environments to Requests
#'
#' Internally, this function is used to coerce the request environment objects
#' \code{httpuv} passes to an application's \code{call} function. Request
#' environment objects are coerced to objects.
#'
#' @param x An \R object.
#'
#' @seealso \code{\link{request}}
#'
#' @keywords internal
#'
#' @export
#' @examples
#' e <- new.env(parent = emptyenv())
#'
#' e$REQUEST_METHOD <- 'GET'
#' e$PATH_INFO <- '/file/download'
#' e$HTTP_ACCEPT <- 'application/json'
#' e$HTTP_CONTENT_LENGTH <- '0'
#'
#' req <- as.request(e)
#' is.request(req)  # TRUE
#'
#' method(req)
#' uri(req)
#' req[['Accept']]
#' req[['Content-Length']]
#'
as.request <- function(x) {
  UseMethod('as.request')
}

#' @rdname as.request
#' @export
as.request.environment <- function(x) {
  req <- request()

  req$method <- toupper(x$REQUEST_METHOD)
  req$uri <- sub('^/', '', x$PATH_INFO)

  if (length(x$QUERY_STRING) && x$QUERY_STRING != "?") {
    query <- strsplit(sub("^\\?", "", x$QUERY_STRING), "&", fixed = TRUE)[[1]]
    req$query <- as.list(sub("^.*=", "", query))
    names(req$query) <- sub("=.*$", "", query)
  } else {
    req$query <- list()
  }

  print(req$query)

  headers <- grep('^HTTP_', names(x), value = TRUE)
  req$headers <- mget(headers, envir = x)
  names(req$headers) <- vapply(headers, frmt_header, character(1))

  if (is.function(x[["rook.input"]][["read_lines"]])) {
    req$body <- x[['rook.input']][['read_lines']]()
  }

  req
}
