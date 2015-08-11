#'
#'
#'
#'
dull_response <- R6::R6Class(
  'dull_response',
  public = list(
    initialize = function() {
      private$status <- 500
      private$headers <- list()
      private$body <- list()
      invisible(self)
    },
    
    add_headers = function(...) {
      new_headers <- list(...)
      if (new_headers %>% names %>% is.null) 
        stop('headers must be named')
      if (new_headers %>% names %>% length %>% is_less_than(length(new_headers)))
        stop('all headers must be named')
      
      private$headers %<>% append(new_headers)
    },
    set_body = function(expr) {
      self$add_headers('Content-Type' = 'text/html')
      private$body <- expr
      invisible(self)
    },
    set_status = function(n) {
      stopifnot(n %>% is.numeric)
      private$status <- n
      invisible(self)
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