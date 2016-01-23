#' Create an HTTP Response
#'
#' A response object represents the HTTP response returned by a route handler. A
#' response is typically made up of a status, HTTP headers, and a body. A
#' response body is optional.
#'
#' @section Components:
#'
#'   \subsection{status:}{
#'
#'   Set the status of a response to indicate what action, if any, the client
#'   needs to take. Otherwise a status of 2XX indicates a client request was
#'   valid and the response object contains the requested resource.
#'
#'   Below are descriptions for each status code class,
#'
#'   \describe{
#'
#'   \item{1xx:}{Informational - Request received, continuing process}
#'
#'   \item{2xx:}{Success - The action was successfully received, understood, and
#'   accepted}
#'
#'   \item{3xx:}{Redirection - Further action must be taken in order to complete
#'   the request}
#'
#'   \item{4xx:}{Client Error - The request contains bad syntax or cannot be
#'   fulfilled}
#'
#'   \item{5xx:}{Server Error - The server failed to fulfill an apparently valid
#'   request}
#'
#'   }
#'
#'   }
#'
#'   \subsection{headers:}{
#'
#'   stub
#'
#'   }
#'
#'   \subsection{body:}{
#'
#'   stub
#'
#'   }
#'
#' @seealso \code{\link{route}}, \code{\link{request}}
#'
#' @export
#' @name response
#' @examples
#' # a route to return a client-requested status
#' # and reason phrase
#' phrase10 <- route(
#'   'GET',
#'   '^\\d+',
#'   function(req) {
#'     stat <- sub('/', '', uri(req))
#'
#'     res <- response()
#'
#'     status(res) <- 200
#'     body(res) <- paste(stat, reason_phrase(stat))
#'
#'     print(res)
#'
#'     res
#'   }
#' )
#'
#' phrase10_m <- mockup(phrase10)
#'
#' phrase10_m('get', '302')
#' phrase10_m('get', '203')
response <- function() {
  structure(
    list(
      status_code = 200,
      headers = list(
        `Content-Type` = 'text/plain'
      ),
      body = ''
    ),
    class = 'response'
  )
}

#' Coerce Objects to Response
#'
#' TBD, see jsonify.
#'
#' @name as.response
NULL

#' @param x Any \R object.
#' @param \ldots Additional arguments passed on to methods.
#' @export
#' @rdname as.response
as.response <- function(x, ...) UseMethod('as.response')

#' @param directory A character string specifying the system folder of the file
#'   \code{x}.
#' @param collapse A character string specifying how to collapse the lines read
#'   from \code{x}.
#' @export
#' @rdname as.response
as.response.character <- function(x, directory = 'views', collapse = '\n', ...) {
  path <- file.path(directory, x)

  assert_that(
    file.exists(path),
    is.readable(path)
  )

  contents <- paste(readLines(path, warn = FALSE), collapse = collapse)

  res <- response()
  res[['Content-Type']] <- mime::guess_type(path)
  body(res) <- contents
  res
}

#' @export
#' @rdname as.response
as.response.data.frame <- function(x, ...) {
  res <- response()
  body(res) <- as.json(x)
  res
}

#' @export
#' @name as.response
#' @examples
#' is.response(logical(1))
#' is.response(response())
#' is.response(3030)
is.response <- function(x) inherits(x, 'response')


#' Printing Responses
#'
#' Print a response object.
#'
#' @param x Object of class \code{response}.
#' @param \ldots Ignored.
#'
#' @details
#'
#' Formats the response as an HTTP response.
#'
#' @seealso \code{\link{response}}
#'
#' @keywords internal
#' @export
#' @name print.response
print.response <- function(x, ...) {
  headers <- lapply(x$headers, as.character)

  cat('HTTP/1.1', x$status_code, reason_phrase(x$status_code), '\r\n')

  if (!is.null(headers) && length(headers)) {
    cat(paste0(names(headers), ': ', headers, collapse = '\r\n'), sep = '')
  }

  if (nchar(x$body) > 0) {
    cat('\r\n\r\n', as.character(x$body), '\r\n', sep = '')
  }
}
