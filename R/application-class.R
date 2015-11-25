is.application <- function(obj) inherits(obj, 'application')

application__ <- R6Class(
  'application',
  public = list(
    initialize = function(routes) {
      assert_that(all(vapply(routes, is.route, logical(1))))
      
      self$routes <- routes
      self$default_404 <- list(
        status = 404,
        headers = list(
          'Content-Type' = 'text/html'
        ),
        body = paste('Sorry, page not found')
      )

      invisible(self)
    },

    call = function(http_request) {
      self$handle_request(http_request)
    },
    listen = function(host, port) {
      httpuv::runServer(host, port, self)
    },

    add_route = function(route) {
      self$routes <- append(self$routes, route)
      invisible(self)
    },
    handle_request = function(http_request) {
      rte <- Find(
        function(r) r$matches(http_request$REQUEST_METHOD, http_request$PATH_INFO), 
        self$routes 
      )

      if (length(rte) == 0) return(self$default_404)

      http_request$ROUTE_PATH <- rte$path
      rte$dispath(http_request)$as_Rook_response()
    }
  ),
  private = list(
    routes = NULL,
    default_404 = NULL
  )
)
