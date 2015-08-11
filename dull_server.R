
library(magrittr)
library(stringr)
library(lazyeval)

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
      res <- dull_response$new()
      
      lazyeval::lazy_eval(route(NULL, res), list(
        # route = route, # I think this thoroughly defeats the purpose
        body = function(.res, expr) .res$body_(expr),
        status = function(.res, status) .res$status_(status)
      ))
      
      # necessary formatting for httpuv::runServer() -> app::call() return value
      res$as_Rook_response()
    }
  )
)

dull <- function() {
  dull_class$new()
}

method <- function(.app, method, url, callback) {
  method_(.app, method, url, callback)
}

method_ <- function(.app, method, url, callback) {
  .app$add_route(method, url, callback)
}

get <- function(.app, url, callback) {
  method(.app, 'GET', url, callback)
}

post <- function(.app, url, callback) {
  method(.app, 'POST', url, callback)
}

put <- function(.app, url, callback) {
  method(.app, 'PUT', url, callback)
}

listen <- function(.app, host, port) {
  .app$run(host, port)
}

