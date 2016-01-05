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
    HTTP_REFERER = 'http://www.references.com',
    REQUEST_METHOD = 'GET',
    SCRIPT_NAME = '',
    PATH_INFO = '/foo/bar',
    QUERY_STRING = 'key=value',
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
  req <- as.request(template_request)

  expect_equal(req[['Cache-Control']], 'max-age=0')
  expect_equal(req[['Connection']], 'keep-alive')
  expect_equal(req[['Accept-Encoding']], 'gzip, deflate, sdch')
  expect_null(req[['foo']])
  expect_equal(req[c('Cache-Control', 'Connection')], list(`Cache-Control` = 'max-age=0', Connection = 'keep-alive'))
})

test_that('[[\'Referer\']] and [[\'Referrer\']] are equivalent', {
  req <- as.request(template_request)
  
  expect_equal(req[['Referer']], req[['Referrer']])
  expect_equal(req[['Referer']], 'http://www.references.com')
})

test_that('url function', {
  expect_error(uri('request'))
  expect_error(uri(3030))
  req <- as.request(template_request)
  expect_equal(uri(req), '/foo/bar')
})

test_that('body function', {
  req <- as.request(template_request)
  expect_equal(body(req), '<p>Hello, world!</p>')
})

test_that('method function', {
  req <- as.request(template_request)
  expect_equal(method(req), 'get')
})

test_that('query function', {
  req <- as.request(template_request)
  expect_equal(query(req), 'key=value')
})

test_that('print function', {
  req <- as.request(template_request)
  req_output <- paste(capture.output(print(req)), collapse = '')
  expect_equal(req_output, "get /foo/bar HTTP/1.1 \rReferer: http://www.references.com\rHost: 127.0.0.1:3030\rConnection: keep-alive\rContent-Type: text/html; charset=utf-8\rCache-Control: max-age=0\rAccept-Language: en-US,en;q=0.8\rAccept-Encoding: gzip, deflate, sdch\rAccept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8\r\r<p>Hello, world!</p>\r")
})
