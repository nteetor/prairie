#' Response class
#'
#' An R6 class representing an HTTP response. Additionally, the response object
#' can be converted into a Rook response for use with the httpuv and Rook
#' packages.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{add_headers(headers)} Add a list of HTTP headers and values to the response
#'  object
#'  \item \code{set_body(expr)} Sets the body of the response, the value of \code{expr} is treated as html
#'  \item \code{set_status(n)} Sets the HTTP status of the response object
#'  \item \code{as_HTTP_response()} Formats and returns the response object as an HTTP response
#'  \item \code{as_Rook_response()} Formats and returns the response object as a Rook reponse
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @importFrom magrittr %>% %<>%
#' @export
#' @name response
response <- R6::R6Class(
  'response',
  public = list(
    status = NULL,
    body = NULL,
    headers = NULL,

    initialize = function() {
      self$status <- 200
      self$headers <- list()
      self$body <- ''
      invisible(self)
    },

    add_headers = function(headers) {
      self$headers %<>% append(headers)
      invisible(self)
    },
    set_body = function(expr) {
      #self$add_headers(list('Content-Type' = 'text/html'))
      self$body <- expr
      invisible(self)
    },
    set_content_type = function(type) {
      self$headers[['Content-Type']] <- type  
      invisible(self)
    },
    set_content_length = function(size) {
      self$headers[['Content-Length']] <- size
      invisible(self)
    },
    set_status = function(n) {
      stopifnot(n %>% is.numeric)
      self$status <- n
      invisible(self)
    },
    end = function(call = sys.call(-1)) {
      end_condition <- structure(
        class = c('end_response', 'message', 'condition'),
        list(
          message = 'response has ended',
          call = call
        )
      )
      stop(end_condition)
    },

    as_HTTP_response = function() {
      cat(
        paste0('HTTP/1.1 ', self$status),
        '\r\n',
        paste0(names(self$headers), ': ', self$headers, collapse = '\r\n'),
        '\r\n\r\n',
        paste0(self$body),
        '\r\n',
        sep = ''
      )
    },
    as_Rook_response = function() {
      list(
        status = self$status,
        headers = self$headers,
        body = self$body
      )
    }
  )
)
