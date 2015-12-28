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
#' Most often \code{GET} or \code{POST} the method indicates what action to take
#' for a specified resource. This value may be accessed with \link{method}.
#' 
#' }
#' 
#' \subsection{uri:}{
#' 
#' The uri indicates the server resource requested by the client. A request 
#' object's uri may be accessed with \link{uri}.
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
#' @name request
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
      query = list(),
      headers = list(),
      body = ''
    ),
    class = 'request'
  )
}

#' Printing Requests
#' 
#' Print a request.
#' 
#' @param x Object of class \code{request}.
#' 
#' @details
#' 
#' Formats the request as an HTTP request.
#' 
#' @seealso
#' 
#' \code{\link{request}}
#' 
#' @keywords internal
#' @export
#' @name print.request
print.request <- function(x, ...) {
  headers <- lapply(
    x$headers,
    function(hdr) if (is.time(hdr) || is.date(hdr)) http_date(hdr) else as.character(hdr)
  )
  names(headers) <- sapply(names(headers), stringr::str_to_title)
  
  cat(paste(x$method, x$uri, 'HTTP/1.1', '\r\n'))
  
  if (!is.null(headers) && length(headers)) {
    cat(paste0(names(headers), ': ', headers, collapse = '\r\n'))
  }
  
  if (nchar(x$body) > 0) {
    cat(
      '\r\n\r\n',
      paste0(x$body),
      '\r\n',
      sep = ''
    )
  }
}

#' Coerce Request Environments
#' 
#' Internally, this function is used to coerce the request environment objects 
#' \code{httpuv} passes to an application's \code{call} function. Request 
#' environment objects are coerced to objects.
#' 
#' @seealso \code{\link{request}}
#'   
#' @keywords internal
#'   
#' @export
#' @name as.request
#' @examples
#' e <- new.env(parent = baseenv())
#' 
#' e$REQUEST_METHOD <- 'GET'
#' e$PATH_INFO <- '/file/download'
#' e$HTTP_ACCEPT <- 'application/json'
#' e$HTTP_CONTENT_LENGTH <- '3030'
#' 
#' req <- as.request(e)
#' is.request(req) # TRUE
#' 
#' method(req)
#' uri(req)
#' req[['Accept']]
#' req[['Content-Length']]
NULL

#' @param x Any \R object.
#' @export
#' @rdname as.request
as.request <- function(x) UseMethod('as.request')

#' @export
#' @rdname as.request
as.request.environment <- function(x) {
  req <- request()
  
  req$method <- x$REQUEST_METHOD
  req$uri <- x$PATH_INFO
  
  req$headers <- mget(grep('^HTTP_', names(x), value = TRUE), envir = x)
  names(req$headers) <- gsub('^HTTP_', '', names(req$headers))
  names(req$headers) <- gsub('_', '-', names(req$headers))
  names(req$headers) <- tolower(names(req$headers))
  
  req$body <- if (!is.null(x$envir.input)) x$envir.input$read_lines() else ''
  
  req
}

#' @export
#' @rdname as.request
is.request <- function(x) inherits(x, 'request')
