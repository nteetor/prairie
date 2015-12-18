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
#' # try out this route to see more
#' route(
#'   'GET',
#'   '^/print/request$',
#'   function(req) {
#'     print('Request received:')
#'     print(req)
#'     
#'     response()
#'   }
#' )
NULL

#' @export
#' @rdname request
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

#' @param x A request object.
#' @export
#' @rdname request
print.request <- function(x) {
  cat(paste(x$method, x$uri, 'HTTP/1.1', '\r\n'))
  
  headers <- lapply(
    x$headers,
    function(hdr) if (is.time(hdr) || is.date(hdr)) http_date(hdr) else as.character(hdr)
  )
  names(headers) <- sapply(names(headers), stringr::str_to_title)
  
  if (!is.null(headers)) {
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

#' Coerce Requests
#' 
#' 
#' 
#' @keywords internal
#' @export
#' @rdname as.request
#' @examples
#' e <- new.env(parent = baseenv())
#' 
#' e$REQUEST_METHOD <- 'GET'
#' e$PATH_INFO <- '/file/download'
#' 
#' as.request(e)
as.request <- function(x) UseMethod('as.request')

#' @export
#' @rdname as.request
as.request.environment <- function(envir) {
  req <- request()
  
  req$method <- envir$REQUEST_METHOD
  req$uri <- envir$PATH_INFO
  
  req$headers <- mget(grep('^HTTP_', names(envir), value = TRUE), envir = envir)
  names(req$headers) <- gsub('^HTTP_', '', names(req$headers))
  names(req$headers) <- gsub('_', '-', names(req$headers))
  names(req$headers) <- tolower(names(req$headers))
  
  req$body <- if (!is.null(envir$envir.input)) envir$envir.input$read_lines() else ''
  
  req
}
