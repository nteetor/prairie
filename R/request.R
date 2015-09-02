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
#' @importFrom R6 R6Class
#' @importFrom stringr str_match_all str_detect str_to_lower str_to_upper
#' @importFrom magrittr %>%
#' @export
#' @name request
request <- R6::R6Class(
  'request',
  public = list(
    method = NULL,
    body = NULL,
    url = NULL,
    ip = NULL,
    port = NULL,
    host_name = NULL,
    params = NULL,
    header_fields = NULL,
    
    initialize = function(route, rook_envir) {
      self$method <- rook_envir[['REQUEST_METHOD']]
      self$url <- rook_envir[['PATH_INFO']]
      self$ip <- rook_envir[['SERVER_NAME']]
      self$port <- rook_envir[['SERVER_PORT']]
      self$host_name <- rook_envir[['HTTP_HOST']]
      self$body <- rook_envir[['rook.input']]$read_lines()
      
      http_headers <- Filter(function(nm) nm %>% str_detect('^HTTP_'), names(rook_envir))
      
      self$header_fields <- mget(http_headers, rook_envir) %>% as.list
      
      if (route$params %>% is.null) {
        self$params <- c()
      } else {
        param_names <- route$params
        param_values <- self$url %>% 
          str_match_all(route$uri) %>% 
          .[[1]] %>% 
          .[-1]
        self$params <- setNames(param_values, param_names)
      }  
      
      invisible(self) 
    },
    
    get_header_field = function(field) {
      field_formatted <- field %>%
        str_to_upper %>% 
        paste0('HTTP_', .)
      
      if (!(field_formatted %in% self$header_fields)) {
        warning(paste('request for', self$url, 'does not contain header', field))
        return(NULL)
      }
      
      self$header_fields[[field_formatted]]
    },
    has_content_type = function(type) {
      type == self$header_fields[['HTTP_CONTENT_TYPE']]
    }
  )
)