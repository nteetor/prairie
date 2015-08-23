#' Simple route class
#' 
#' This class is a glorified container for a url and the corresponding callback
#' function. Some handy utility functions are included as private methods. As
#' dull middleware begins to take shape this class will evolve more.
#' 
#' @section Methods: 
#' \itemize{ 
#'  \item \code{uri_equals(string)}: TRUE if the
#'   route uri equals string 
#'  \item \code{uri_matches(path)}: TRUE if the route
#'   uri, as a regular expression, matches path 
#'  \item \code{assign_callback(method, callback)}: set the callback function for a
#'   specific HTTP method 
#'  \item \code{callback_for(method)}: return the callback
#'   function for a specific HTTP method 
#' }
#'   
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @importFrom stringr str_match_all str_sub
#' @importFrom magrittr %>% %<>% equals
#' @export
#' @name route-class
route <- R6::R6Class(
  # This naming convention will be applied to response.R once this branch is merged into master
  'route',
  public = list(
    uri = NULL,
    params = NULL,
    callbacks = NULL,
    
    initialize = function(method, uri, callback) {
      stopifnot(
        method %>% is.character,
        uri %>% is.character, 
        callback %>% is.function,
        callback %>% formals %>% length %>% equals(2)
      )
      
      self$callbacks <- list()
      self$callbacks[[method]] <- callback
      
      if (uri %>% str_sub(-1) %>% equals('/') & uri != '/') uri %<>% str_sub(end = -2)
      self$uri <- uri
      
      self$params <- self$capture_group_names(uri)
      
      invisible(self)
    },
    
    uri_equals = function(string) {
      self$uri == string
    },
    uri_matches = function(path) {
      stopifnot(path %>% is.character)
      
      if (path != '/' & path %>% str_sub(-1) %>% equals('/')) path %<>% str_sub(end = -2)
      
      str_detect(path, paste0('^',self$uri,'$'))
    },
    get_uri = function() {
      self$uri
    },
    get_params = function() {
      self$params
    },
    assign_callback = function(method, callback) {
      self$callbacks[[method]] <- callback
      
      invisible(self)
    },
    get_callback = function(method) {
      self$callbacks[[method]]
    },
    capture_group_names = function(uri) {
      group_names <- uri %>% 
        str_match_all('\\(\\?<(\\w*)>') %>% 
        .[[1]]
      
      if (group_names %>% NCOL %>% equals(0)) return(NULL)

      group_names %<>% .[, 2]
      
      if (any(group_names %>% equals(''))) {
        empty_names <- which(group_names %>% equals(''))
        stop(paste0(
          'route URI contains empty capture group name',
          ifelse(empty_names %>% length %>% equals(1), '', 's')
        ))
      }
      
      group_names
    }
    
  )
)