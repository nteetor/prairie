library(prairie)
context('response object')

test_that('get and set values with `[[` and `[[<-`', {
  res <- response()
  
  res[['Content-Type']] <- 'text/html'
  
  expect_equal(res[['Content-Type']], 'text/html')

  res[['Warning']] <- 'challenger approaching'
  expect_equal(res[['Warning']], 'challenger approaching')

  expect_null(res[['missing']])
  expect_null(res[['whoops']])
})

test_that('responses intialize with default values', {
  res <- response()

  expect_equal(res[['Content-Type']], 'text/plain')
  expect_equal(status(res), 200)
})

test_that('header field values set properly with `[[`', {
  res <- response()

  res[['Connection']] <- 'close'
  res[['Content-Length']] <- 9001

  expect_equal(res[['Connection']], 'close')
  expect_equal(res[['Content-Length']], '9001')
})
