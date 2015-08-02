
library(magrittr)
library(stringr)
library(R6)

dull_class <- R6::R6Class(
  'dull_class',
  public = list(
    initialize = function() {
      private$routes <- list()
      invisible(self)
    },
    
    add_route = function(url, on_request) {
      private$routes[[url]] = on_request
      invisible(self)
    },
    
    print_routes = function() {
      print(private$routes)
    },
    
    run = function(host, port, timeout = getOption('timeout')) {
      on.exit({
        close(conn)
      })
      
      while (T) {
        conn <- socketConnection(host = host,
                                 port = port,
                                 timeout = timeout,
                                 server = TRUE,
                                 blocking = TRUE,
                                 open = 'r+'
        )
        req <- readLines(conn, 1)
        cat(req,'\n')
        
        req_parsed <- req %>% str_split('\\s+') %>% extract2(1)
        # cat(req_parsed, '\n')
        
        req_method <- req_parsed[1]
        req_url <- req_parsed[2]
        req_version <- req_parsed[3]
        
        # cat('HTTP method: ', req_method, '\n')
        # cat('URI: ', req_url, '\n')
        # cat('Version:', req_version, '\n')
        
        handler <- private$get_handler(req_url)
        # cat(paste('Handler:', handler, '\n'))
        
        if (handler %>% is.null) {
          res_header <- private$http_header('404 Not Found',
                                            version = req_version)
          res_body <- NULL
        } else {
          res_header <- private$http_header('200 OK',
                                            version = req_version)
          res_body <- handler()
        }
        
        res <- paste(
          res_header,
          res_body,
          '',
          sep = '\n',
          collapse = ''
        )
        
        cat(res)
        
        writeLines(res, conn)
        close(conn)
      }
    }
  ),
  private = list(
    routes = NULL,
    
    get_handler = function(url) {
      private$routes[[url]]
    },
    
    http_header = function(status, version = 'HTTP/1.1', content_type = 'text/plain', char_set = 'utf-8') {
      return(
        paste(
          paste(version, status),
          paste('Content-Type:', content_type, ';', 'charset=', char_set),
          '',
          sep = '\n',
          collapse = ''
        )
      )
    }
    
  )
)
