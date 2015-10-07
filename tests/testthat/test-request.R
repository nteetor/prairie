library(dull)
context('request object')

test_that('fields set on initialize', {
  HTTP_REQ <- list(
    REQUEST_METHOD = 'GET',
    SCRIPT_NAME = '',
    PATH_INFO = '/foo/bar',
    QUERY_STRING = '',
    SERVER_NAME = '127.0.0.1',
    SERVER_PORT = '3030',
    HTTP_HOST = '127.0.0.1:3030',
    rook.version = 'nope',
    rook.url_scheme = 'https',
    rook.input = list(
      read_lines = function() {
        '<p>Hello, world!</p>'
      }
    ),
    rook.errors = 'Should I care?'
  )
  
  req <- request$new(NULL, HTTP_REQ)
})
