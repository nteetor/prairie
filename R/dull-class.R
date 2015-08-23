#' Dull application class object
#' 
#' An R6 class which is the backbone of the dull package.
#' 
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @export
#' @name dull_app
dull_app <- R6::R6Class(
  'dull_app',
  public = list(
    routes = NULL,
    default_404 = NULL,
    
    initialize = function() {
      self$routes <- list()
      self$default_404 <- list(
        status = 404,
        headers = list(
          'Content-Type' = 'text/html'
        ),
        body = paste('Sorry, page not found')
      )
      
      invisible(self)
    },
    
    call = function(req) {
      self$handle_request(req)
    },
    run = function(host, port) {
      httpuv::runServer(host, port, self)
    },
    
    add_route = function(method, uri, callback) {
      if (uri %in% names(self$routes))
        self$routes[[uri]]$assign_callback(method, callback)
      else
        self$routes[[uri]] <- route$new(method, uri, callback)
      
      invisible(self)
    },
    handle_request = function(rook_envir) {
    route <- self$find_route(rook_envir[['PATH_INFO']])
      
      if (route %>% is.null) return(self$default_404)
      
      callback <- route$get_callback(rook_envir[['REQUEST_METHOD']])
      
      if (callback %>% is.null) return(self$default_404)
      
      req <- request$new(route, rook_envir)
      res <- response$new()
      
      load_helpers(callback)(req, res)
      
      res$as_Rook_response()
    },
    find_route = function(uri) {
      route_name = Find(function(nm) self$routes[[nm]]$uri_matches(uri), names(self$routes), nomatch = NULL)
      
      if (route_name %>% is.null) return(NULL)
      
      self$routes[[route_name]]
    }
  )
)
