#' Simple route class
#' 
#' This class is a glorified container for a url and the corresponding callback
#' function. Some handy utility functions are included as private methods. As
#' dull middleware begins to take shape this class will evolve more.
#' 
#' @section Methods: 
#' \itemize{ 
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
        length(formals(callback)) == 2
      )
      
      self$callbacks <- list()
      self$callbacks[[method]] <- callback
      
      # if (uri %>% str_sub(-1) %>% equals('/') & uri != '/') uri %<>% str_sub(end = -2)
      self$uri <- uri
      
      self$params <- self$uri_parameters(uri)
      
      invisible(self)
    },
    
    uri_matches = function(path) {
      stopifnot(path %>% is.character)
      
      if (str_sub(path, 1, 1) == '/') path %<>% str_sub(2)
      
      path == '' | str_detect(path, self$uri)
    },
    assign_callback = function(method, callback) {
      self$callbacks[[method]] <- callback
      
      invisible(self)
    },
    get_callback = function(method) {
      self$callbacks[[method]]
    },
    uri_parameters = function(uri) {
      group_names <- uri %>% 
        str_match_all('\\(\\?<(\\w*)>') %>% 
        .[[1]]
      
      if (NCOL(group_names) == 0) return(NULL)

      group_names %<>% .[, 2]
      
      if (any(group_names == '')) {
        empty_names <- which(group_names == '')
        stop(paste0(
          'route URI contains empty capture group name',
          ifelse(length(empty_names) == 1, '', 's')
        ))
      }
      
      group_names
    }
  )
)