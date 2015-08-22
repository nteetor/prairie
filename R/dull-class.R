#' Dull application class
#' 
#' An R6 class which is the backbone of the dull package. An instance of the
#' Dull class includes
#' 
#' @name dull-class
#' dull_app
#' dull_class
#' Dull
dull_app <- R6::R6Class(
  'dull_app',
  public = list(
    initialize = function() {
      private$routes <- list()
      private$default_404 <- list(
        status = 404,
        headers = list(
          'Content-Type' = 'text/html'
        ),
        body = paste('Sorry, page not found')
      )
      
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
      private$handle_request(req)
    },
    run = function(host = '0.0.0.0', port = '3000') {
      httpuv::runServer(host, port, self)
    }
  ),
  private = list(
    routes = NULL,
    default_404 = NULL,

    handle_request = function(rook_envir) {
      route <- private$find_route(rook_envir[['PATH_INFO']])
      
      if (route %>% is.null) return(private$default_404)
      
      callback <- route$get_callback(rook_envir[['REQUEST_METHOD']])
      
      if (callback %>% is.null) return(private$default_404)
      
      req <- request$new(route, rook_envir)
      res <- response$new()
      
      load_helpers(callback)(req, res)
      
      res$as_Rook_response()
    },
    find_route = function(uri) {
      route_name = Find(function(nm) private$routes[[nm]]$uri_matches(uri), names(private$routes), nomatch = NULL)
      
      if (route_name %>% is.null) return(NULL)
      
      private$routes[[route_name]]
    }
  )
)
