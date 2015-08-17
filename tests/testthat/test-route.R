library(dull)
context('route-class')

test_that('route stops on missing arguments', {
  expect_error(route$new())
  expect_error(route$new('GET'))
  expect_error(route$new('GET', '/test'))
})

test_that('route stops on incorrect arguments', {
  expect_error(route$new(404, '/test', function(req, res) NULL))
  expect_error(route$new('GET', 404, function(req, res) NULL))
  expect_error(route$new('GET', '/test', 404))
})

# the label of this test needs improvement
test_that('callback function is correct', {
  expect_error(route$new('GET', '/test', function() NULL))
  expect_error(route$new('GET', '/test', function(req) NULL))
  expect_error(route$new('GET', '/test', function(req, res, extra_arg) NULL))
})

test_that('uri matching works for simple strings', {
  route_hello <- route$new('GET', '/hello', function(req, res) NULL)
  
  expect_true(route_hello$uri_matches('/hello'))
  expect_false(route_hello$uri_matches('hello'))
  expect_false(route_hello$uri_matches('/helloo'))
  expect_true(route_hello$uri_matches('/hello/'))
})
