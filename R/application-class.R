#' @export
is.application <- function(obj) inherits(obj, 'application')

#' @docType class
#' @keywords internal
#' @name application-class
application__ <- R6::R6Class(
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
      routes <- Filter(
        function(r) r$matches(http_request$REQUEST_METHOD, http_request$PATH_INFO), 
        self$routes 
      )

      if (length(routes) == 0) return(self$default_404)

      http_request$ROUTE_PATH <- routes[[1]]$path
      req <- request__$new(http_request)
      res <- response__$new(req)
      
      withCallingHandlers(
        for (r in routes) {
          http_request$ROUTE_PATH <- r$path
          r$dispatch(request__$new(http_request), res)
        },
        end = function(sig) {
          res$as_Rook_response()
        },
        error = function(err) {
          list(
            status = 500,
            headers = list('Content-Type' = 'text/plain'),
            body = paste('Server error:', err$message, sep = '\n')
          )
        }
      )
    }
  ),
  private = list(
    routes = NULL,
    default_404 = NULL
  )
)
