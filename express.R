library(R6)
library(magrittr)
library(httr)

Request <- R6Class("Request", 
                      public = list(
                  app = NA,
                  baseURL = NA, 
                  fresh = TRUE, 
                  hostname = NA,
                  ip = NA,
                  orginalUrl = NA,
                  params = NA,
                  path = NA, 
                  protocol = NA, 
                  query = NA,
                  route = NA,
                  secure = NA, 
                  stale = FALSE, 
                  
                  accepts = function(type){
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


# 
# accepts =  structure(list(), class = "accepts")
# types <- function() Usemethod("types")
# types.default <- function() { 
#   headers(self)$Accept 
#   }
# charsets <- function()  Usemethod("")