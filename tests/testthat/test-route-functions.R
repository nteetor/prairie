context('route functions')

test_that('is.route returns TRUE/FALSE appropriately', {
  expect_true(is.route(route('', '', function() NULL)))
  expect_false(is.route('route'))
  expect_false(is.route(c('route', '31')))
  expect_false(is.route(3030))
  expect_false(is.route(data.frame()))
})

test_that('as.route.route coerces correctly', {
  route31 <- route('', '', function() NULL)
  expect_true(is.route(route31))
  expect_true(is.route(as.route(route31)))
})

test_that('as.route.character coerces correctly', {
  expect_true(is.route(as.route('sample-route.R', path = '.')))
  expect_error(as.route('does-not-exist-route.R', path = '.'))
  expect_error(as.route('whoops-route.R', path = '.'), '^Error : Could not parse')
})

test_that('as.route.list coerces correctly', {
  expect_true(is.route(as.route(list(method = '', path = '', handler = function() NULL))))
  expect_error(as.route(list(path = '', handler = function() NULL)))
  expect_error(as.route(list()))
})
