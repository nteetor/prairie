library(R6)
library(magrittr)
library(httr)

request <- R6Class(
  "request", 
  public = list(
    base_url = NA, 
    hostname = NA,
    ip = NA,
    original_url = NA,
    params = NA,
    path = NA, 
    protocol = NA, 
    query = NA,
    route = NA,
    secure = NA, 
    raw_string = NA,
    
    initialize = function(request_msg) {
      self$raw_string <- request_msg
      
    },
    accepts = function(type) {
      if( type %in% headers(self)$Accept) {
        return (TRUE)
      } else{
        return(FALSE)
      } 
    }, 
    get = function(field) {
      #returns specified HTTP requet header field 
    },
    is = function() {
      
    },
    param = function() {
      
    }
  )
)
