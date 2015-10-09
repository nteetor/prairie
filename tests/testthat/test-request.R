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
  template_request$HTTP_ACCEPT <- 'text/html'
  req <- request$new(template_route, template_request)
  expect_equal(req$accepts('html'), 'html')

  template_request$HTTP_ACCEPT <- 'text/*, application/json'
  req2 <- request$new(template_route, template_request)
  expect_equal(req2$get('accept'), 'text/*, application/json')
  expect_equal(req2$accepts('html'), 'html')
  expect_equal(req2$accepts('text/html'), 'text/html')
  expect_equal(req2$accepts('json'), 'json')
  expect_equal(req2$accepts(c('json', 'text')), 'json')
  expect_equal(req2$accepts('application/json'), 'application/json')
  expect_equal(req2$accepts('image/png'), NULL)
  expect_equal(req2$accepts('png'), NULL)

  template_request$HTTP_ACCEPT <- 'text/*;q=.5, application/json'
  req3 <- request$new(template_route, template_request)
  expect_equal(req3$accepts(c('html', 'json')), 'json')
})

test_that('$get retrieves correct header values', {
  req <- request$new(template_route, template_request)

  expect_equal(req$get('cache-control'), 'max-age=0')
  expect_equal(req$get('Cache-Control'), req$get('cache-control'))
  expect_equal(req$get('connection'), 'keep-alive')
  expect_equal(req$get('Connection'), req$get('connection'))
  expect_equal(req$get('accept-encoding'), 'gzip, deflate, sdch')
  expect_equal(req$get('accept-encoding'), req$get('accept-Encoding'))
  expect_equal(req$get('foo'), NULL)
})

test_that('$is returns TRUE/FALSE for correct/incorrect content-type', {
  template_request$HTTP_CONTENT_TYPE <- 'text/html; charset=utf-8'
  req <- request$new(template_route, template_request)
  expect_true(req$is('html'))
  expect_true(req$is('text/html'))
  expect_true(req$is('text/*'))

  template_request$HTTP_CONTENT_TYPE <- 'application/json'
  req2 <- request$new(template_route, template_request)
  expect_true(req2$is('json'))
  expect_true(req2$is('application/json'))
  expect_true(req2$is('application/*'))
  expect_false(req2$is('html'))
})
