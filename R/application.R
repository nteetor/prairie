#' Create an Application
#' 
#' A prairie application is quite simply a list of route objects. When creating 
#' an application, \ldots may be any combination of \R objects, so long as a 
#' corresponding \code{as.route} method is implemented for each object's class. 
#' See the Details section for more information.
#' 
#' @param \ldots \R objects, will be coerced to class route.
#'   
#' @details
#' 
#' In order for prairie to best help \R programmers grow web applications from 
#' their existing code many coercion generics have been created and are 
#' available for implementation. The, arguably, most important of these generics
#' is \code{as.route}. CRAN is ripe with packages which include custom S3 
#' classes. prairie aims to provide a streamlined process for serving these 
#' classes across the web. An implementation of \code{as.route} might choose a
#' resouce path based on a class attribute or choose the HTTP method based on
#' object permissions (GET for a read-only database connection or GET and POST
#' for a read/write connection).
#' 
#' Furthmore, prairie is meant to work on top of, around, and with existing \R
#' code. While writing a good implementation of \code{as.route} may take time,
#' if this package is succesful, one will not need to modify existing code. You
#' have worked hard on your code and prairie is along to grow it onwards and 
#' upwards.
#' 
#' @seealso \code{\link{as.route}}, \code{\link{start}}
#'   
#' @export
#' @name application
app <- function(...) {
  routes <- lapply(list(...), as.route)
  
  structure(
    list(
      routes = routes
    ),
    class = 'application'
  )
}

is.application <- function(x) inherits(x, 'application')
  
#' Start Up a Prairie Application
#'
#' Run an application at \code{host} on \code{port}. 
#' 
#' @param app An object with class \code{application}.
#' @param host A character string specifying the host name.
#' @param port An integer specifying the port number.
#'
#' @export
#' @name run
run <- function(app, host, port) {
  assert_that(
    is.application(app), 
    is.character(host), 
    is.numeric(port)
  )
  
  httpuv::runServer(
    host, 
    port,
    list(
      call = function(req) {
        matching_rt <- Find(function(rt) is_match(rt, req), app$routes, nomatch = NULL)
        
        if (is.null(matching_rt)) {
          return(
            list(
              status = 404,
              headers = list(`Content-Type` = 'text/plain'),
              body = paste('Sorry, page not found')
            )
          )
        }
        
        req$ROUTE_PATH <- matching_rt$path
        rt$handler(req)
      }
    )
  )
}
