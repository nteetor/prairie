context('route object')

test_that('route stops on incorrect arguments', {
  expect_error(route(3030, '^test/$', function(req) response()))
  expect_error(route('get', 3030, function(req) response()))
  expect_error(route('get', '^test/$', 'WRONG'))
})

test_that('route matching with different paths', {
  route_hello <- route('get', '^test$', function(req) response())
  route_hello_m <- mockup(route_hello)

  expect_status <- function(s1, s2) {
    route_status <- status(s1)
    eval(bquote(expect_equal(.(route_status), .(s2))))
  }

  expect_status(route_hello_m('get', 'hello/world/'), 404)
  expect_status(route_hello_m('get', 'unit/test'), 404)
  expect_status(route_hello_m('get', 'testing'), 404)

  expect_status(route_hello_m('get', 'test'), 200)
})

test_that('route matching with different methods', {
  route_get <- route('get', '^.*', function(req) response())
  route_get_m <- mockup(route_get)

  expect_status <- function(s1, s2) {
    route_status <- status(s1)
    eval(bquote(expect_equal(.(route_status), .(s2))))
  }

  expect_status(route_get_m('put', 'path'), 404)
  expect_status(route_get_m('head', 'path'), 404)
  expect_status(route_get_m('post', 'path'), 404)

  expect_status(route_get_m('get', 'path'), 200)
  expect_status(route_get_m('get', 'another/path'), 200)
  expect_status(route_get_m('get', 'else'), 200)
})

test_that('index route matches correct paths', {
  route_index <- route('get', '^$', function(req) response())
  route_index_m <- mockup(route_index)

  expect_status <- function(s1, s2) {
    route_status <- status(s1)
    eval(bquote(expect_equal(.(route_status), .(s2))))
  }

  expect_status(route_index_m('get', 'oops'), 404)

  expect_status(route_index_m('get', ''), 404)
})

test_that('path may only be vector of length 1', {
  expect_error(route('get', c('red', 'fish', 'blue', 'fish'), function(req) respsonse()))
  expect_error(route('get', c('one', 'two'), function(req) response()))
})

test_that('a single route catches matching requests', {
  skip('todo in the future')
  route_many <- route('get', c('^number|numeric|numeral$'), function(req) response())

#  expect_true(route_many$matches('get', 'numeric'))
#  expect_true(route_many$matches('get', 'numeral'))
})

test_that('uri named parameters are extracted', {
  skip('TODO')

  route_groups <- route('get', '^/(?<name>\\w+)/(?<age>\\d+)$', function(req) NULL)

  expect_equal(route_groups$params, c('name', 'age'))

  expect_error(route('get', '^/(?<>\\w+)', function(req) NULL), 'route URI contains empty')

  route_no_groups <- route('get', '^/foo/bar', function(req) NULL)

  expect_equal(route_no_groups$params, c())
})
