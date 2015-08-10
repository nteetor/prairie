
library(magrittr)
library(stringr)

#
# The comments throughout this file indicate future plans
# for this project
#

dull_class <- R6::R6Class(
  'dull_class',
  public = list(
    initialize = function() {
      private$routes <- list()
      invisible(self)
    },
    
    add_route = function(method, url, on_request) {
      if (private$routes[[url]] %>% is.null) {
        private$routes[[url]] <- list()
      }
      private$routes[[url]][[method]] <- on_request
      invisible(self)
    },
    
    print_routes = function() {
      print(private$routes)
    },
    
    call = function(req) {
      private$route_for(req[['PATH_INFO']], req[['REQUEST_METHOD']])
    },
    
    run = function(host = '0.0.0.0', port = '3000') {
      httpuv::runServer(host, port, self)
    }
  ),
  private = list(
    routes = NULL,
    
    route_for = function(url, method) {
      route <- private$routes[[url]][[method]]
      
      if (route %>% is.null) {
        return(
          list(
            status = 404,
            headers = list(
              'Content-Type' = 'text/html'
            ),
            body = paste('Could not find', url, 'for method', method)
          )
        )
      }
      
      # req <- dull_requst$new(...)
      # res <- dull_response$new()
      # route(req, res)
      res <- route()
      if (res$status %>% is.null) res$status <- 200
      
      # necessary formatting for httpuv::runServer() -> app::call() return value
      # res %>% as_Rook_response
      res
    }
  )
)

dull <- function() {
  dull_class$new()
}

get <- function(.app, url, f) {
  .app$add_route('GET', url, f)
}

# To be implemented
# post <- function(.app, url, f) {
#   .app$add_route('POST', url, f)
# }

listen <- function(.app, host, port) {
  .app$run(host, port)
}

