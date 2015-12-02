library(prairie)
context('route object')

test_that('route stops on incorrect arguments', {
  expect_error(route(3030, '^test/$', function() response()))
  expect_error(route('get', 3030, function() response()))
  expect_error(route('get', '^test/$', 'WRONG'))
})

test_that('route matching with different paths', {
  route_hello <- route('get', '^test$', function() response())

  expect_false(route_hello$matches('get', 'hello/world/'))
  expect_false(route_hello$matches('get', 'unit/test'))
  expect_false(route_hello$matches('get', 'testing'))

  expect_true(route_hello$matches('get', 'test'))
})

test_that('route matching with different methods', {
  route_get <- route('get', '^.*', function() response())
  
  expect_false(route_get$matches('put', 'path'))
  expect_false(route_get$matches('head', 'path'))
  expect_false(route_get$matches('post', 'path'))
  
  expect_true(route_get$matches('get', 'path'))
  expect_true(route_get$matches('get', 'another/path'))
  expect_true(route_get$matches('get', 'else'))
})

test_that('index route matches correct paths', {
  route_index <- route('get', '^$', function() response())

  expect_false(route_index$matches('get', 'oops'))
  
  expect_true(route_index$matches('get', ''))
})

test_that('path may only be vector of length 1', {
  expect_error(route('get', c('red', 'fish', 'blue', 'fish'), function() respsonse()))
  expect_error(route('get', c('one', 'two'), function() response()))
})

test_that('a single route catches matching requests', {
  route_many <- route('get', c('^number|numeric|numeral$'), function() response())

  expect_true(route_many$matches('get', 'number'))
  expect_true(route_many$matches('get', 'numeric'))
  expect_true(route_many$matches('get', 'numeral'))
})

test_that('uri named parameters are extracted', {
  skip('TODO')
  
  route_groups <- route('get', '^/(?<name>\\w+)/(?<age>\\d+)$', function() NULL)

  expect_equal(route_groups$params, c('name', 'age'))

  expect_error(route('get', '^/(?<>\\w+)', function() NULL), 'route URI contains empty')

  route_no_groups <- route('get', '^/foo/bar', function() NULL)

  expect_equal(route_no_groups$params, c())
})
