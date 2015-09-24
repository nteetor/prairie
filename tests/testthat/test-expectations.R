library(dull)
context('sharpen tests')

# borrowed from testthat package, file test/testthat/test-xxx.R
# Using to test expected failures of expect_* functions found in sharpen.R
test_test_that <- function(desc, failure_expected = TRUE, expr) {
  reporter <- SilentReporter$new()
  old_reporter <- set_reporter(reporter)
  test_that(desc, expr)
  set_reporter(old_reporter)
  test_that(desc, {
    if (failure_expected) {
      info <- 'Test succeeded when failure expected'
      expect_equal(length(reporter$failures), 1, info = info)
    } else {
      info <- sprintf(
        'Test failed unexpectedly: %s',
        as.character(reporter$failures[[desc]]))
      expect_equal(length(reporter$failures), 0, info = info)
    }
  })
}

dummy_app <- function(status_code, body) {
  return(list(
    call = function(req) {
      list(
        status = status_code,
        headers = list(Connection = 'close'),
        body = body
      )
    }
  ))
}

test_test_that(
  'expect_status fails correctly',
  failure_expected = TRUE,
  {
    test_app <- list(
      call = function(req) {
          list(
            status = 303L,
            headers = list(Connection = 'close'),
            body = '... and nobody would stop to save her ...'
          )
      }
    )

    expect_status(test_app, 'GET', '/404', 200)
    expect_status(test_app, 'HEAD', '/toons.php', 444)
  }
)

test_that('expect_status succeeds correctly', {
  expect_status(dummy_app(200, ''), 'GET', '/error/please', 200)
  expect_status(dummy_app(200, ''), 'PUT', '/maybe/404', 200)

  expect_status(dummy_app(404, ''), 'POST', '/', 404, body = 'post content')
  expect_status(dummy_app(404, ''), 'GET', '/hooga/chaka', 404)

  expect_status(dummy_app(303, ''), 'PUT', '/your/hands/up', 303)
  expect_status(dummy_app(303, ''), 'PUT', '/down/the/ducky', 303)
})

test_test_that(
  'expect_body fails correctly',
  failure_expected = TRUE,
  {
    test_app <- list(
      call = function(req) {
        list(
          status = 303L,
          headers = list(Connection = 'close'),
          body = 'head, shoulders, knees, and toes'
        )
      }
    )

    expect_body(test_app, 'GET', '/hokey/pokey', 'eyes and ears and mouth and nose')
  }
)

test_that('expect_body succeeds correctly', {
  expect_body(dummy_app(200, 'wax on, wax off'), 'GET', '/again', 'wax on, wax off')
  expect_body(dummy_app(300, 'do or do not'), 'PUT', '/and', 'do or do not')
})
