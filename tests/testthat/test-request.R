library(dull)
context('request object')

template_route <- list(
  uri = '/foo/bar'
)

template_request <- list(
  HTTP_ACCEPT = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
  HTTP_ACCEPT_ENCODING = "gzip, deflate, sdch",
  HTTP_ACCEPT_LANGUAGE = "en-US,en;q=0.8",
  HTTP_CACHE_CONTROL = "max-age=0",
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

test_that('fields set on initialize', {
  req <- request$new(template_route, template_request)

  expect_equal(req$original_url, '/foo/bar')
  expect_equal(req$route$uri, '/foo/bar')
  expect_equal(req$body, '<p>Hello, world!</p>')

  template_route$uri <- '/get/(?<filename>\\w+)/(?<page>\\d+)'
  template_request$PATH_INFO <- '/get/party_jams/2'

  req2 <- request$new(template_route, template_request)

  expect_equal(list(filename = 'party_jams', page = '2'), req2$params)
  expect_equal(req$get('cache-control'), 'max-age=0')
  expect_equal(req$get('connection'), 'keep-alive')
})

test_that('$accepts accepts correct types', {
  template_request$HTTP_ACCEPT <- 'text/html'
  req <- request$new(template_route, template_request)
  expect_equal(req$accepts('html'), 'html')


})
