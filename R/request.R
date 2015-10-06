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
    params = NULL,
    protocol = NULL,
    route = NULL,
    signed_cookies = NULL, # need to implement cookie parser
    stale = NULL, # depends on $fresh
    subdomains = NULL,
    xhr = NULL, # may not be implemented
    
    method = NULL,
    url = NULL,
    port = NULL,
    host_name = NULL,
    header_fields = NULL,

    initialize = function(route, http_request) {
      self$base_url <- NULL
      self$body <- http_request$rook.input$read_lines()
      self$cookies <- list()
      self$ip <- http_request$SERVER_NAME
      self$ips <- NULL
      self$original_url <- http_request$PATH_INFO
      self$url <- self$original_url
      
      self$params <- setNames(
        list(stringr::str_match_all(self$route$uri, self$original_url)[[1]][-1]),
        stringr::str_match_all(strsplit(self$original_url, '/'), '\\?<\\w+>')[, 2]
      )
      
      self$protocol <- http_request$rook.url_scheme # definitely check this
      self$route <- route
      self$signed_cookies <- NULL
      self$stale <- NULL
      self$subdomains <- strsplit(sub('(\\.\\w+){2}/.*$', '', self$original_url), '\\.')[[1]] # breaks on 'example.com'
      
      # move to private or remove      
      self$method <- http_request[['REQUEST_METHOD']]
      self$url <- http_request[['PATH_INFO']]
      self$port <- http_request[['SERVER_PORT']]
      self$host_name <- http_request[['HTTP_HOST']]
      
      headers <- http_request[grep('^HTTP_', names(http_request), value = TRUE)]
      
      if (length(headers) == 0) {
        self$header_fields <- list()
      } else {
        self$header_fields <- headers
      }
      
      invisible(self)
    },

    get = function(field) {
      assert_that(is.character(field))
      self$header_fields[[tolower(field)]]
    },
    header = function(field) {
      self$get(field)
    },
    
    # this will become the method "is"
    has_content_type = function(type) {
      type == self$header_fields[['HTTP_CONTENT_TYPE']]
    }
  )
)
