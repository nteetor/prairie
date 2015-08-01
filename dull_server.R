
library(magrittr)
library(stringr)
library(R6)

dull_server <- R6::R6Class(
  'dull_server',
  public = list(
    initialize = function(host, port) {
      private$host <- host
      private$port <- port
    },
    
    add_controller = function(f) {
      return()
    },
    
    start = function(timeout = getOption('timeout')) {
      while (T) {
        conn <- socketConnection(host = private$host,
                                 port = private$port,
                                 timeout = timeout,
                                 server = TRUE,
                                 blocking = TRUE,
                                 open = 'r+'
        )
        req <- readLines(conn, 1)
        cat(req,'\n')
        req_method <- req %>% str_extract('^\\w+')
        cat('HTTP method: ', req_method, '\n')
        req_uri <- req %>% str_extract('/\\w+')
        cat('URI: ', req_uri, '\n')
        
        res <- paste(
          private$generate_header('200'),
          'Hello, world!',
          sep = '',
          collapse = '\n'
        )
        writeLines(res, conn)
        close(conn)
      }
    }
  ),
  private = list(
    host = NULL,
    port = NULL,
    urls = NULL,
    
    view = NULL,
    controllers = NULL,
    
    generate_header = function(status, content_type = 'text/plain', char_set = 'utf-8') {
      full_status <- switch(status,
                            '200' = '200 OK',
                            '404' = '404 Not Found',
                            stop(paste('Unknown status "', status, '"'))
      )
      
      return(
        paste(
          paste('HTTP/1.1', full_status),
          paste('Content-Type:', content_type, ';', 'charset=', char_set),
          '\n\n',
          sep = '',
          collapse = '\n'
        )
      )
    }
    
  )
)
