#'
#'
#'
#'
dull_response <- R6::R6Class(
  'dull_response',
  public = list(
    initialize = function() {
      private$status <- 500
      private$header <- list()
      private$body <- list()
      invisible(self)
    },
    
    body_ = function(expr) {
      private$header %<>% append(list('Content-Type' = 'text/html'))
      private$body <- expr
      invisible(self)
    },
    status_ = function(n) {
      stopifnot(n %>% is.numeric)
      private$status <- n
      invisible(self)
    },
    as_Rook_response = function() {
      list(
        status = private$status,
        header = private$header,
        body = lazyeval::lazy_eval(private$body)
      )
    }
  ),
  private = list(
    status = NULL,
    body = NULL,
    header = NULL
  )
)