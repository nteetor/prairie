#' Route class
#' 
#' The route class holds meta information about a request received for a 
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
#'  \item \code{get_method()}:
#'    Return request HTTP method
#'  \item \code{get_body()}:
#'    Return the request body
#'  \item \code{get_ip()}:
#'    Return the remote ip address of the request
#'  \item \code{get_port()}:
#'    Return the remove host address of the request
#'  \item \code{get_host_name()}:
#'    Return the HTTP remote host
#'  \item \code{get_params()}:
#'    Return captured URI parameters as a list, list names are capture group name
#' }
#' 
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @importFrom stringr str_detect str_to_lower
#' @export
request <- R6::R6Class(
  'request',
  public = list(
    initialize = function(route, rook_envir) {
      private$method <- rook_envir[['REQUEST_METHOD']]
      private$url <- rook_envir[['PATH_INFO']]
      private$ip <- rook_envir[['SERVER_NAME']]
      private$port <- rook_envir[['SERVER_PORT']]
      private$host_name <- rook_envir[['HTTP_HOST']]
      private$body <- rook_envir[['rook.input']]$read_lines()
      
      http_headers <- Filter(function(nm) nm %>% str_detect('^HTTP_'), names(rook_envir))
      
      private$header_fields <- mget(http_headers, rook_envir) %>% as.list
      
      if (route$get_params() %>% is.null) {
        private$params <- c()
      } else {
        param_names <- route$get_params()
        param_values <- private$url %>% 
          str_match_all(route$get_uri()) %>% 
          .[[1]] %>% 
          .[-1]
        private$params <- setNames(param_values, param_names)
      }  
      
      invisible(self) 
    },
    
    get_header_field = function(field) {
      field_formatted <- field %>%
        str_to_upper %>% 
        str_c('HTTP_', .)
      
      if (!(field_formatted %in% private$header_fields)) {
        warning(paste('request for', private$url, 'does not contain header', field))
        return(NULL)
      }
      
      private$header_fields[[field_formatted]]
    },
    has_content_type = function(type) {
      type == private$header_fields[['HTTP_CONTENT_TYPE']]
    },
    
    get_method = function() private$method,
    get_body = function() private$body,
    get_ip = function() private$ip,
    get_port = function() private$port,
    get_host_name = function() private$host_name,
    get_params = function() private$params
  ),
  private = list(
    method = NULL,
    body = NULL,
    url = NULL,
    ip = NULL,
    port = NULL,
    host_name = NULL,
    params = NULL,
    header_fields = NULL
  )
)