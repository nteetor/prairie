
library(magrittr)
library(stringr)
library(lazyeval)

source('route.R')
source('response.R')

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
    
    add_route = function(method, uri, callback) {
      if (uri %in% names(private$routes))
        private$routes[[uri]]$assign_callback(method, callback)
      else
        private$routes[[uri]] <- route$new(method, uri, callback)
      
      invisible(self)
    },
    call = function(req) {
      response <- private$route_for(req[['PATH_INFO']], req[['REQUEST_METHOD']])
      response
    },
    run = function(host = '0.0.0.0', port = '3000') {
      httpuv::runServer(host, port, self)
    }
  ),
  private = list(
    routes = NULL,
    server_utils = list(
      body = function(.r, expr) {
        if (.r %>% inherits('dull_response')) {
          .r$set_body(expr)
        }
      },
      status = function(.r, status) {
        if (.r %>% inherits('dull_response')) {
          .r$set_status(status)
        }
      },
      headers = function(.r, ...) {
        if (.r %>% inherits('dull_response')) {
          args <- list(...)
          
          if (length(names(args)) != length(args)) {
            stop('Not all arguments are named')
          }
          
          sapply(names(args), function(field_name) {
            field_value <- args[[field_name]]
            .r$add_headers(setNames(field_value, rep(field_name, times = length(field_value))))
          })
          
          invisible(.r)
        }
      }
    ),
    
    assign_utils = function(route_env) {
      new_env <- new.env(parent = route_env)
      
      Map(function(util) assign(util, private$server_utils[[util]], envir = new_env), names(private$server_utils))
      
      new_env
    },    
    route_for = function(path, method) {
      uri <- Find(function(uri) private$routes[[uri]]$uri_matches(path), names(private$routes))
      
      callback <- private$routes[[uri]]$callback_for(method)
      
      if (callback %>% is.null) {
        return(
          list(
            status = 404,
            headers = list(
              'Content-Type' = 'text/html'
            ),
            body = paste('Could not find URI', uri, 'for method', method)
          )
        )
      }
      
      # req <- dull_requst$new(...)
      res <- dull_response$new()
      
      environment(callback) %<>% private$assign_utils(.)
      
      callback(NULL, res)
      
      # necessary formatting for httpuv::runServer() -> app::call() return value
      res$as_Rook_response()
    }
  )
)

dull <- function() {
  dull_class$new()
}

method <- function(.app, method, uri, callback) {
  .app$add_route(method, uri, callback)
}

get <- function(.app, uri, callback) {
  method(.app, 'GET', uri, callback)
}

post <- function(.app, uri, callback) {
  method(.app, 'POST', uri, callback)
}

put <- function(.app, uri, callback) {
  method(.app, 'PUT', uri, callback)
}

listen <- function(.app, host, port) {
  .app$run(host, port)
}

