library(dull)
context('route-class')

test_that('route stops on missing arguments', {
  expect_error(route$new())
  expect_error(route$new('GET'))
  expect_error(route$new('GET', '^test/$'))
})

test_that('route stops on incorrect arguments', {
  expect_error(route$new(404, '^test/$', function(req, res) NULL))
  expect_error(route$new('GET', 404, function(req, res) NULL))
  expect_error(route$new('GET', '^test/$', 404))
})

# the label of this test needs improvement
test_that('callback function formals are checked', {
  expect_error(route$new('GET', '^test/$', function() NULL))
  expect_error(route$new('GET', '^test/$', function(req) NULL))
  expect_error(route$new('GET', '^test/$', function(req, res, extra_arg) NULL))
})

test_that('route matching works for simple strings', {
  route_hello <- route$new('GET', '^hello/$', function(req, res) NULL)
  
  expect_false(route_hello$uri_matches('say/hello/'))
  expect_false(route_hello$uri_matches('hello/world/'))
  expect_false(route_hello$uri_matches('helloo/'))
  
  expect_true(route_hello$uri_matches('hello/'))
})

test_that('index request is correctly matched', {
  route_index <- route$new('GET', '^$', function(req, res) NULL)
  
  expect_true(route_index$uri_matches(''))
  expect_true(route_index$uri_matches('/'))
})

test_that('vector of allowed uris is correctly concatenated', {
  route_vector_4 <- route$new('GET', c('red', 'fish', 'blue', 'fish'), function(req, res) NULL)
  
  expect_equal(route_vector_4$uri, 'red|fish|blue|fish')
  
  route_vector_2 <- route$new('GET', c('raj', 'that'), function(req, res) NULL)
  
  expect_equal(route_vector_2$uri, 'raj|that')
})

test_that('a single route will catch all matching requests', {
  route_many <- route$new('GET', c('^number$', '^numeric$', '^numeral$'), function(req, res) NULL)
  
  expect_true(route_many$uri_matches('number'))
  expect_true(route_many$uri_matches('numeric'))
  expect_true(route_many$uri_matches('numeral'))
})
