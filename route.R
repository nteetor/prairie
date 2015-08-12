#' Simple route class
#'
#' This class is a glorified container for a url and the corresponding
#' callback function. Some handy utility functions are included as 
#' private methods. As dull middleware begins to take shape this class 
#' will evolve more.
#' 
#' @section Methods:
#' \itemize{
#'  \item \code{uri_equals(string)}: TRUE if the route uri equals string
#'  \item \code{uri_matches(path)}: TRUE if the route uri, as a regular expression, matches path
#'  \item \code{assign_callback(method, callback)}: set the callback function for a specific HTTP method
#'  \item \code{callback_for(method)}: return the callback function for a specific HTTP method
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
        uri %>% is.character, 
        callback %>% is.function,
        callback %>% formals %>% length %>% equals(2)
      )
      
      private$callbacks[[method]] <- callback
      
      private$uri <- uri
    },
    
    uri_equals = function(string) {
      private$uri == string
    },
    uri_matches = function(path) {
      stopifnot(path %>% is.character)
      
      str_detect(path, paste0('^',private$uri,'$'))
    },
    assign_callback = function(method, callback) {
      private$callbacks[[method]] <- callback
    },
    callback_for = function(method) {
      private$callbacks[[method]]
    }
  ),
  private = list(
    uri = NULL,
    callbacks = list(
      GET = NULL,
      POST = NULL,
      PUT = NULL
    )
    
  )
)