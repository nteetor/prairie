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
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @export
#' @name route
route <- R6::R6Class(
  # This naming convention will be applied to response.R once this branch is merged into master
  'route',
  public = list(
    initialize = function(method, uri, callback) {
      stopifnot(
        method %>% is.character,
        uri %>% is.character, 
        callback %>% is.function,
        callback %>% formals %>% length %>% equals(2)
      )
      
      private$callbacks <- list()
      private$callbacks[[method]] <- callback
      
      if (uri %>% str_sub(-1) %>% equals('/') & uri != '/') uri %<>% str_sub(end = -2)
      private$uri <- uri
      
      private$params <- private$capture_group_names(uri)
      
      invisible(self)
    },
    
    uri_equals = function(string) {
      private$uri == string
    },
    uri_matches = function(path) {
      stopifnot(path %>% is.character)
      
      if (path != '/' & path %>% str_sub(-1) %>% equals('/')) path %<>% str_sub(end = -2)
      
      str_detect(path, paste0('^',private$uri,'$'))
    },
    assign_callback = function(method, callback) {
      private$callbacks[[method]] <- callback
      
      invisible(self)
    },
    callback_for = function(method) {
      private$callbacks[[method]]
    }
  ),
  private = list(
    uri = NULL,
    params = NULL,
    callbacks = NULL,
    
    capture_group_names = function(uri) {
      group_names <- uri %>% 
        str_match_all('\\(\\?<(\\w*)>') %>% 
        extract2(1)
      
      if (group_names %>% NCOL %>% equals(0)) return(NULL)

      group_names %<>% extract( , 2)
      
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