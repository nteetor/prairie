#' Mockup a Route
#' 
#' Creates a mockup of a route object. A mockup simulates a route's response to
#' a particular method and resource combination.
#' 
#' @param r A \code{route} object.
#' 
#' @export
#' @name mockup
#' @examples
#' logger <- route(
#'   'GET',
#'   '^',
#'   function(req) {
#'     print(req)
#'     
#'     response()
#'   }
#' )
#' 
#' logger_m <- mockup(logger)
#' logger_m('GET', '/yellow/brick/path')
#' logger_m('GET', '/phonday', headers = list(Accepts = 'text/html'))
mockup <- function(r) {
  if (!is.route(r)) stop('Cannot create mockup of class ', class(r)[1], call. = FALSE)
  
  m <- structure(
    function(method, uri, headers = list()) {
      e <- new.env(parent = baseenv())
      e$REQUEST_METHOD <- method
      split_on_query <- strsplit(uri, '?', fixed = TRUE)[[1]]
      e$PATH_INFO <- split_on_query[1]
      e$QUERY_STRING <- if (length(split_on_query) > 1) split_on_query[2] else ''
      lapply(headers, function(h) assign(paste0('HTTP_', h), headers[[h]], envir = e))
      req <- as.request(e)

      if (is_match(r, req)) {
        res <- r$handler(req)
        
        if (!is.response(res)) {
          warning('handler returned object of class ', class(res), call. = FALSE)
        }
      } else {
        res <- response()
        status(res) <- 404
        body(res) <- paste0(
          'The method, uri combination could not be handled by the route.\n\n',
          '  Route method  ', paste(r$method, collapse = ', '), ' (case-insensitive)\n',
          '    Route path  "', r$path, '"\n\n',
          'Request method  ', paste(req$method, collapse = ', '), '\n',
          '   Request uri  "', r$uri, '"'
        )
      }
      
      invisible(res)
    },
    class = c('mockup', class(r))
  )
  
  attr(m, 'source') <- r
  
  m
}

#' @export
#' @rdname print.route
print.mockup <- function(x, ...) {
  print.route(attr(x, 'source', exact = TRUE))
}
