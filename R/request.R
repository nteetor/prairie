#' Request class
#'
#' The request class holds meta information about a request received for a
#' particular application resource (URI). This information is pulled from an
#' underlying Rook environment. A request object is passed to the corresponding
#' callback function when a URI is requested.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{get_header_field(field)}:
#'    Retrieve a HTTP header field value from the underlying Rook environment
#'  \item \code{has_content_type(type)}:
#'    TRUE if the request content type is \code{type}
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom assertthat assert_that
#' @export
#' @name request
request <- R6::R6Class(
  'request',
  public = list(
    base_url = NULL, # in need of route implementation
    body = NULL,
    cookies = NULL, # need to implement cookie parser
    fresh = NULL, # may or may not be implemented,
    ip = NULL,
    ips = NULL, # may not be implemented
    original_url = NULL, # for now, very unecessary
    url = NULL,
    params = NULL,
    protocol = NULL,
    route = NULL,
    signed_cookies = NULL, # need to implement cookie parser
    stale = NULL, # depends on $fresh
    subdomains = NULL,
    xhr = NULL, # may not be implemented

    initialize = function(route, http_request) {
      self$base_url <- NULL
      self$body <- http_request$rook.input$read_lines()
      self$cookies <- list()
      self$ip <- http_request$SERVER_NAME
      self$ips <- NULL
      self$original_url <- http_request$PATH_INFO
      self$url <- self$original_url
      self$route <- route

      if (grepl('\\?<[a-zA-Z]+>', self$route$uri)) {
        param_values <- stringr::str_match_all(self$original_url, self$route$uri)[[1]][1,][-1]
        param_names <- stringr::str_match_all(self$route$uri, '\\?<([a-zA-Z]+)>')[[1]][,2]

        self$params <- as.list(setNames(param_values, param_names))
      } else {
        self$params <- list()
      }

      self$protocol <- http_request$rook.url_scheme # definitely check this
      self$signed_cookies <- NULL
      self$stale <- NULL
      self$subdomains <- strsplit(sub('(\\.\\w+){2}/.*$', '', self$original_url), '\\.')[[1]] # breaks on 'example.com'

      private$method <- http_request[['REQUEST_METHOD']]
      private$port <- http_request[['SERVER_PORT']]
      private$host_name <- http_request[['HTTP_HOST']]

      headers <- http_request[grep('^HTTP_', names(http_request), value = TRUE)]
      names(headers) <- tolower(names(headers))

      if (length(headers) == 0) {
        private$header_fields <- list()
      } else {
        private$header_fields <- headers
      }

      invisible(self)
    },

    accepts = function(type, ...) {
      assert_that(is.character(c(type, ...)))

      if (is.null(self$get('accept'))) return(NULL)

      accepted_types <- strsplit(self$get('accept'), ',\\s*')
      types_to_question <- c(type, ...)

      all_combinations <- expand.grid(accepted_types, types_to_question, stringsAsFactors = FALSE)

      for (row in seq_len(NROW(all_combinations))) {
        accepted_type <- all_combinations[row, 1]
        type_in_question <- all_combinations[row, 2]

        if (grepl('/', type_in_question)) {
          if (accepted_type == type_in_question) {
            return(type_in_question)
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
    header = function(field) {
      self$get(field)
    },
    is = function(type) {
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
    method = NULL,
    port = NULL,
    host_name = NULL,
    header_fields = NULL
  )
)
