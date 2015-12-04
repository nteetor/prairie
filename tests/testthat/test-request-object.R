context('request object')

template_request <- list2env(
  list(
    HTTP_ACCEPT = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    HTTP_ACCEPT_ENCODING = "gzip, deflate, sdch",
    HTTP_ACCEPT_LANGUAGE = "en-US,en;q=0.8",
    HTTP_CACHE_CONTROL = "max-age=0",
    HTTP_CONTENT_TYPE = 'text/html; charset=utf-8',
    HTTP_CONNECTION = "keep-alive",
    HTTP_HOST = "localhost:3030",
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
)

test_that('request initialize with defaults', {
  req <- request(template_request)
  
  expect_equal(req$url, '/foo/bar')
  expect_equal(req$body, '<p>Hello, world!</p>')
})

test_that('$type_is returns TRUE/FALSE for correct/incorrect content-type', {
  req <- request(template_request)
  
  expect_true(req$type_is('html'))
  expect_true(req$type_is('text/html'))
  expect_true(req$type_is('text/*'))
})
