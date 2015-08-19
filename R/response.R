#'
#'
#'
#'
response <- R6::R6Class(
  'response',
  public = list(
    initialize = function() {
      private$status <- 200
      private$headers <- list()
      private$body <- list()
      invisible(self)
    },
    
    add_headers = function(headers) {
      private$headers %<>% append(headers)
      invisible(self)
    },
    set_body = function(expr) {
      self$add_headers(list('Content-Type' = 'text/html'))
      private$body <- expr
      invisible(self)
    },
    set_status = function(n) {
      stopifnot(n %>% is.numeric)
      private$status <- n
      invisible(self)
    },
    
    as_HTTP_response = function() {
      cat(
        paste0('HTTP/1.1 ', private$status),
        '\r\n',
        paste0(names(private$headers), ': ', private$headers, collapse = '\r\n'),
        '\r\n\r\n',
        paste0(private$body),
        '\r\n',
        sep = ''
      )
    },
    as_Rook_response = function() {
      list(
        status = private$status,
        headers = private$headers,
        body = private$body
      )
    }
  ),
  private = list(
    status = NULL,
    body = NULL,
    headers = NULL
  )
)