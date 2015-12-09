context('request functions')

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

test_that('`[[` and `[` properly gets header fields', {
  req <- request(template_request)

  expect_equal(req[['Cache-Control']], 'max-age=0')
  expect_equal(req[['Connection']], 'keep-alive')
  expect_equal(req[['Accept-Encoding']], 'gzip, deflate, sdch')
  expect_null(req[['foo']])
  expect_equal(req[c('Cache-Control', 'Connection')], list(`Cache-Control` = 'max-age=0', Connection = 'keep-alive'))
})

test_that('`[[<-` properly sets header fields', {
  req <- request()

  req[['Cache-Control']] <- 'out of control'
  req[['Accept']] <- 'text/*'

  expect_equal(req[['Cache-Control']], 'out of control')
  expect_equal(req[['Accept']], 'text/*')
})

test_that('url() helper function', {
  expect_error(url('request'))
  expect_error(url(3030))
  req <- request(template_request)
  expect_equal(url(req), '/foo/bar')
})

