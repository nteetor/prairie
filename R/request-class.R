is.request <- function(obj) inherits(obj, 'request')

request__ <- R6::R6Class(
  'request',
  active = list(
    args = function() private$args_, # TODO: take this out?
    body = function() private$body_,
    ip = function() private$ip_,
    method = function() private$method_,
    url = function() private$url_,
    protocol = function() private$protocol_,
    host_name = function() private$host_name_,
    port = function() private$port_
  ),
  public = list(
    initialize = function(http_request) {
      assert_that(is.environment(http_request) || is.list(http_request))

      http_request <- as.list(http_request)

      private$body_ <- tryCatch(http_request$rook.input$read_lines(), error = function(e) NULL)
      private$ip_ <- http_request$SERVER_NAME
      private$url_ <- http_request$PATH_INFO

      if (!is.null(http_request$ROUTE_PATH) && grepl('\\?<[a-zA-Z]+>', http_request$ROUTE_PATH)) {
        args <- stringr::str_match_all(private$url_, http_request$ROUTE_PATH)[[1]][1,][-1]
        args_names <- stringr::str_match_all(http_request$ROUTE_PATH, '\\?<([a-zA-Z]+)>')[[1]][,2]

        private$args_ <- as.list(setNames(args, args_names))
      } else {
        private$args_ <- list()
      }

      private$protocol_ <- http_request$rook.url_scheme # definitely check this

      private$method_ <- http_request$REQUEST_METHOD
      private$port_ <- http_request$SERVER_PORT
      private$host_name_ <- http_request$HTTP_HOST

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

      if (field %in% c('Referer', 'Referrer')) {
        private$header_fields$referer %||% private$header_fields$referrer
      } else {
        private$header_fields[[tolower(field)]]
      }
    },
    set = function(field, value) {
      assert_that(is.character(field))

      private$header_fields[[tolower(field)]] <- value

      invisible(self)
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
    },

    headers = function() private$header_fields
  ),
  private = list(
    header_fields = NULL,
    args_ = NULL,
    body_ = NULL,
    ip_ = NULL,
    method_ = NULL,
    url_ = NULL,
    protocol_ = NULL,
    host_name_ = NULL,
    port_ = NULL
  )
)
