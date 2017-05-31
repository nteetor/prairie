#' Create an HTTP Response
#'
#' A response object represents the HTTP response returned by a route handler. A
#' response is typically made up of a status, HTTP headers, and a body. A
#' response body is optional.
#'
#' @param status (numeric) HTTP status code, e.g. 200 for OK, 404 for Not Found
#' @param content_type (character) MIME content type, e.g., "text/plain", "text/html".
#' @param body (character) Body of the response
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
#' mkup <- mockup(
#'   route(
#'     'GET',
#'     '^\\d+',
#'     function(req) {
#'       stts <- sub('/', '', uri(req))
#'
#'       res <- response()
#'
#'       status(res) <- 200
#'       body(res) <- paste(stts, reason_phrase(stts))
#'
#'       res
#'     }
#'   )
#' )
#'
#' mkup('get', '302')
#' mkup('get', '203')
#'
response <- function(status=200, content_type='text/plain', body='') {
  structure(
    list(
      status_code = status,
      headers = list(
        `Content-Type` = content_type
      ),
      body = body
    ),
    class = 'response'
  )
}

#' Coerce Objects to Responses
#'
#' The \code{as.response} functions makes serving up R objects as server
#' responses straightfoward.
#'
#' @param x Any \R object.
#' @param \ldots Additional arguments passed on to methods.
#'
#' @details
#'
#' \code{as.response.character} expects \code{x} is a character string
#' specifying a file name. The default directory for the file is "views", but a
#' different path may be specified with the \code{directory} argument. If the
#' file exists the contents are read and set as the response body. The response
#' Content-Type is guessed from the file extension using
#' \code{\link[mime]{guess_type}}.
#'
#' \code{as.response.data.frame} coerces and serves up a data frame as JSON. The
#' data frame is coerced using the \code{\link{as.json}} function and additional
#' arguments may be passed to \code{as.json} using \code{\ldots}.
#'
#' @rdname as.response
#' @export
#' @examples
#'
as.response <- function(x, ...) {
  UseMethod('as.response')
}

#' @param directory A character string specifying the system folder of the file
#'   \code{x}.
#' @param collapse A character string specifying how to collapse the lines read
#'   from \code{x}.
#' @rdname as.response
#' @export
as.response.character <- function(x, directory = 'views', collapse = '\n', ...) {
  if (length(x) != 1) {
    stop('expecting argument `x` to be a single character string',
         call. = FALSE)
  }

  path <- file.path(directory, x)

  if (!file.exists(path)) {
    stop('file "', path, '" does not exist', call. = FALSE)
  }

  if (!is_readable(path)) {
    stop('do not have read permissions for "', path, '"', call. = FALSE)
  }

  res <- response()
  res[['Content-Type']] <- mime::guess_type(path)

  contents <- paste(readLines(path, warn = FALSE), collapse = collapse)
  body(res) <- contents

  res
}

#' @rdname as.response
#' @export
as.response.data.frame <- function(x, format="json", ...) {
  switch(format,
         json = response(content_type = "application/json",
                         body = as.json(x, ...) ),
         html = response(content_type = "text/html",
                         body = as.character(htmlTable::htmlTable(x)) ),
         csv = {
          theText = NULL
          con = textConnection(theText, open="w", local=TRUE)
          write.csv(x, file=con, row.names=FALSE)
          body = paste(textConnectionValue(con), collapse="\n")
          response(content_type = "text/csv",
                   body = body )
         },
         text = response(content_type = "text/plain",
                         body = capture.output(print(x)) ),
         stop("prairie: Invalid response format: ", format) )
}

#' @rdname as.response
#' @export
as.response.matrix <- function(x, ...) {
  as.response.data.frame(as.data.frame(x), ...)
}

#' @rdname as.response
#' @export
#' @examples
#' is.response(logical(1))
#' is.response(response())
#' is.response(3030)
is.response <- function(x) {
  inherits(x, 'response')
}


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
print.response <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

#' @keywords internal
#' @export
#' @rdname print.response
format.response <- function(x, ...) {

  str_sc <- paste(x[['status_code']], reason_phrase(x[['status_code']]))
  str_h <- paste0(names(x[['headers']]), ': ', ifelse(is.date(x[['headers']]),
                                                      http_date(x[['headers']]),
                                                      x[['headers']]))
  str_b <- if (x[['body']] == '') '""' else x[['body']]

  width <- max(nchar(str_sc), nchar(str_h))
  frmt <- paste0('%', width, 's')

  formatted <- c(
    'A response:',
    paste(sprintf(frmt, str_sc), '<status>'),
    paste(sprintf(frmt, str_h), '<header>'),
    paste(sprintf(frmt, str_b), '<body>')
  )

  paste('#', formatted, collapse = '\n')
}
