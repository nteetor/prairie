#' Application class object
#'
#' An R6 class which is the backbone of the dull package.
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @export
#' @name application
application <- R6::R6Class(
  'application',
  public = list(
    routes = NULL,
    default_404 = NULL,
    host = NULL,
    port = NULL,

    initialize = function() {
      self$routes <- list()
      self$default_404 <- list(
        status = 404,
        headers = list(
          'Content-Type' = 'text/html'
        ),
        body = paste('Sorry, page not found')
      )
      self$host <- '127.0.0.1'
      self$port <- 3030

      invisible(self)
    },

    call = function(http_request) {
      self$handle_request(http_request)
    },
    run = function(host, port) {
      if (missing(host)) {
        message(paste('Host defaulting to', self$host))
        host <- self$host
      }
      if (missing(port)) {
        message(paste('Port defaulting to', self$port))
        port <- self$port
      }

      httpuv::runServer(host, port, self)
    },

    add_route = function(method, uri, callback) {
      route_to_be <- route$new(method, uri, callback)

      if (route_to_be$uri %in% names(self$routes))
        self$routes[[route_to_be$uri]]$assign_callback(method, callback)
      else
        self$routes[[route_to_be$uri]] <- route_to_be

      invisible(self)
    },
    handle_request = function(http_request) {
      route <- self$find_route(http_request[['PATH_INFO']])

      if (is.null(route)) return(self$default_404)

      callback <- route$get_callback(http_request[['REQUEST_METHOD']])

      if (is.null(callback)) return(self$default_404)

      req <- request$new(route, http_request)
      res <- response$new()

      tryCatch({
        load_callback_envir(callback)(req, res)

        list(
          status = 500,
          headers = list('Content-Type' = 'text/plain'),
          body = 'send() is never called for response object'
        )
      },
      end_response = function(c) {
        res$as_Rook_response()
      },
      error = function(e) {
        list(
          status = 500,
          headers = list('Content-Type' = 'text/plain'),
          body = paste('Server error:', e, sep = '\n')
        )
      })
    },
    find_route = function(uri) {
      route_name = Find(function(nm) self$routes[[nm]]$uri_matches(uri), names(self$routes), nomatch = NULL)

      if (is.null(route_name)) return(NULL)

      self$routes[[route_name]]
    }
  )
)
