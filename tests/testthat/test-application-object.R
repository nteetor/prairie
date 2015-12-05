context('test application')

test_that('is.application and app() no arguments', {
  expect_true(is.application(app()))
  expect_false(is.application(character(1)))
  expect_false(is.application(integer(1)))
})

test_that('application constructor constructs properly', {
  expect_true(
    is.application(
      app(
        route('GET', '^$', function() NULL),
        route('PUT', '^/putt/putt', function() NULL)
      )
    )
  )
})

test_that('application coerces arguments to routes', {
  expect_true(
    is.application(
      app(
        as.route('sample-route.R', path = '.'),
        list(
          method = 'GET',
          path = '^',
          handler = function() NULL
        )
      )
    )
  )
  expect_error(app(3030))
})

test_that('starting application fails for incorrect args', {
  expect_error(start(route(), 'deltron', 3030))
  expect_error(start(app(), 30, 30))
  expect_error(start(app(), 'del', 'tron'))
})

# deltron 3030, handy because the name is a string and a number

