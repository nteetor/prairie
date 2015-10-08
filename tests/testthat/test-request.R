library(dull)
context('request object')

template_route <- list(
  uri = '/foo/bar'
)

template_request <- list(
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
})

test_that('$accepts accepts correct types', {
  skip('PICK UP HERE')
})
