#' @docType class
#' @keywords internal
#' @importFrom assertthat assert_that
#' @name request-class
request__ <- R6::R6Class(
  'request__',
  public = list(
    args = NULL,
    body = NULL,
    ip = NULL,
    method = NULL,
    url = NULL,
    protocol = NULL,
    subdomains = NULL,

    initialize = function(http_request) {
      self$body <- http_request$rook.input$read_lines()
      self$ip <- http_request$SERVER_NAME
      self$url <- http_request$PATH_INFO

      if (grepl('\\?<[a-zA-Z]+>', http_request$ROUTE_PATH)) {
        args <- stringr::str_match_all(self$original_url, http_request$ROUTE_PATH)[[1]][1,][-1]
        args_names <- stringr::str_match_all(http_request$ROUTE_PATH, '\\?<([a-zA-Z]+)>')[[1]][,2]

        self$args <- as.list(setNames(args, args_names))
      } else {
        self$args <- list()
      }

      self$protocol <- http_request$rook.url_scheme # definitely check this
      self$subdomains <- strsplit(sub('(\\.\\w+){2}/.*$', '', self$original_url), '\\.')[[1]] # breaks on 'example.com'

      self$method <- http_request$REQUEST_METHOD
      private$port <- http_request$SERVER_PORT
      private$host_name <- http_request$HTTP_HOST

      headers <- http_request[grep('^HTTP_', names(http_request), value = TRUE)]
      names(headers) <- gsub('_', '-', gsub('^http_', '', tolower(names(headers))))

      if (length(headers) == 0) {
        private$header_fields <- list()
      } else {
        private$header_fields <- headers
      }

      invisible(self)
    },

    accepts = function(types) {
      assert_that(is.character(types))

      if (is.null(self$get('accept'))) return(NULL)

      accepted_types <- strsplit(self$get('accept'), ',\\s*')[[1]]
      types_to_question <- types

      all_combinations <- expand.grid(accepted_types, types_to_question, stringsAsFactors = FALSE)

      for (row in seq_len(NROW(all_combinations))) {
        accepted_type <- all_combinations[row, 1]
        type_in_question <- all_combinations[row, 2]

        if (grepl('/', type_in_question)) {
          if (accepted_type == type_in_question) {
            return(type_in_question)
          } else {
            accepted_regex <- gsub('\\*', '.*', accepted_type)

            if (grepl(accepted_regex, type_in_question)){
              return(type_in_question)
            }
          }
        } else {
          accepted_regex <- gsub('\\*', '.*', accepted_type)
          mime_in_question <- mime::guess_type(paste0('.', type_in_question))

          if (grepl(accepted_regex, mime_in_question)) {
            return(type_in_question)
          }
        }
      }

      NULL
    },
    get = function(field) {
      assert_that(is.character(field))

      if (tolower(field) %in% c('referer', 'referrer')) {
        private$header_fields$referer %||% private$header_fields$referrer
      } else {
        private$header_fields[[tolower(field)]]
      }
    },
    type_is = function(type) {
      assert_that(is.character(type))

      accepted_type <- sub('\\s*;.*$', '', self$get('content-type'))
      accepted_regex <- gsub('\\*', '.*', accepted_type)

      if (grepl('/', type)) {
        type_regex <- gsub('\\*', '.*', type)
      } else {
        type_regex <- mime::guess_type(paste0('.', type))
      }

      # this will need -plenty- of testing
      grepl(accepted_regex, type) || grepl(type_regex, accepted_type)
    }
  ),
  private = list(
    port = NULL,
    host_name = NULL,
    header_fields = NULL
  )
)
