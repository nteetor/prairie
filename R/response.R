#' Create an HTTP response
#' 
#' A response object represents the HTTP response returned by a route handler.
#' 
#' @seealso \code{\link{route}}, \code{\link{request}}
#'   
#' @export
#' @name response
#' @examples
#' # simple 301 response
#' res_301 <- response()
#'     
#' res_301[['Connection']] <- 'close'
#' status(res_301) <- 301
#' body(res_301) <- 'Sorry, this page has moved'
#' 
#' # simple 404 response
#' res_404 <- response()
#' 
#' res_404[['Connection']] <- 'close'
#' status(res_404) <- 404
#' body(res_404) <- 'Sorry, page not found'
response <- function() {
  response__$new()
}

response__ <- R6::R6Class(
  'response',
  active = list(
    body = function(value) {
      if (missing(value)) {
        private$body_
      } else {
        if (is.json(value)) {
          self$set('Content-Type', 'application/json')
        }
        private$body_ <- value
        invisible(self)
      }
    },
    status = function(code) {
      if (missing(code)) {
        private$status_code
      } else {
        assert_that(is.numeric(code))
        
        private$status_code <- code
        invisible(self)
      }
    }
  ),
  public = list(
    initialize = function() {
      private$status_code <- 200
      private$headers <- list('Content-Type' = 'text/plain')
      private$body_ <- ''
      
      invisible(self)
    },
    
    append = function(field, value) {
      assert_that(is.character(field), is.character(value))
      
      if (!is.null(self$get(field))) {
        value <- paste(self$get(field), paste(value, collapse = ''), sep = '')
      }
      
      self$set(field, value)
      
      invisible(self)
    },
    attachment = function(path = NULL) {
      if (is.null(path)) {
        self$set('Content-Disposition', 'attachment')
      } else {
        assert_that(is.character(path), file.exists(path))
        
        split_path <- strsplit(path, '/|\\\\', perl = TRUE)[[1]]
        f_name <- split_path[length(split_path)]
        
        self$set('Content-Disposition', paste0('attachment; filename=\"', f_name, '\"'))
        self$set('Content-Type', mime::guess_type(f_name))
      }
      
      invisible(self)
    },
    cookie = function(name, value, expires = NULL) {
      assert_that(is.character(name), is.character(value), is.list(options))
      
      cookie_string <- paste0(name, '=', value)
      
      if (!is.null(expires)) {
        assert_that(is.time(expires) || is.date(expires))
        cookie_string <- paste0(cookie_string, '; ', 'Expires=', http_date(expires))
      }
      
      private$headers <- append(private$headers, list('Set-Cookie' = cookie_string))
      
      invisible(self)
    },
    clear_cookie = function(name) {
      assert_that(is.character(name))
      
      self$cookie(name, '', expires = Sys.time())
      
      invisible(self)
    },
    download = function(path, filename = path) {
      assert_that(is.character(path), file.exists(path), is.character(filename))
      
      # self$set('Content-Disposition', paste0('attachment; filename=\"', filename, '\"'))
      
      self$send_file(path, list('Content-Disposition' = paste0('attachment; filename=\"', filename, '\"')))
      
      invisible(self)
    },

    get = function(field) {
      assert_that(is.character(field))
      
      private$headers[[field]]
    },
    set = function(field, value) {
      assert_that(is.character(field))
      private$headers[[field]] <- value
      
      invisible(self)
    },
    type = function(type) {
      assert_that(is.character(type))
      self$set('Content-Type', mime::guess_type(type, empty = type, mime_extra = mimeextra))
      
      invisible(self)
    },
    vary = function(field) {
      assert_that(is.character(field))
      if (is.null(self$get('Vary'))) {
        self$set('Vary', field)
      } else {
        self$append('Vary', paste0(',', field))
      }
      
      invisible(self)
    },
    
    as_HTTP_response = function() {
      headers <- lapply(private$headers, as.character)
      
      cat('HTTP/1.1', private$status_code, get_status_description(private$status_code, FALSE), '\r\n')

      if (!is.null(headers)) {
        cat(paste0(names(headers), ': ', headers, collapse = '\r\n'), sep = '')
      }
      
      if (nchar(private$body_) > 0) {
        cat('\r\n\r\n', as.character(private$body_), '\r\n', sep = '')
      }
    },
    as_Rook_response = function() {
      list(
        status = private$status_code,
        headers = lapply(
          private$headers, 
          function(hdr) if (is.time(hdr) || is.date(hdr)) http_date(hdr) else as.character(hdr)
        ),
        body = private$body_
      )
    },
    
    print = function() {
      self$as_HTTP_response()
    }
  ),
  private = list(
    status_code = NULL,
    body_ = NULL,
    headers = NULL
  )
)

