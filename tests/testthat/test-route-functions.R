context('route functions')

test_that('is.route returns TRUE/FALSE appropriately', {
  expect_true(is.route(route('', '', function(req) NULL)))
  expect_false(is.route('route'))
  expect_false(is.route(c('route', '31')))
  expect_false(is.route(3030))
  expect_false(is.route(data.frame()))
})

test_that('as.route.route coerces correctly', {
  route31 <- route('', '', function(req) NULL)
  expect_true(is.route(route31))
  expect_true(is.route(as.route(route31)))
})

test_that('as.route.character coerces correctly', {
  expect_true(is.route(as.route('sample-route.R', directory = '.')))
  expect_error(as.route('does-not-exist-route.R', directory = '.'))
  expect_error(as.route('whoops-route.R', directory = '.'), '^Error : Could not parse')
})

test_that('as.route.list coerces correctly', {
  expect_true(is.route(as.route(list(method = '', path = '', handler = function(req) NULL))))
  expect_error(as.route(list(path = '', handler = function(req) NULL)))
  expect_error(as.route(list()))
})

test_that('print function', {
  sesame_route <- route('PUT', 'down/the/ducky', function(req) response())
  route_output <- paste(capture.output(print(sesame_route)), collapse = '')
  expect_equal(route_output, 'route  put   down/the/ducky')
})
