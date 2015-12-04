context('response class')

test_that('responses created properly', {
  expect_silent(res <- response())
  expect_error(res <- response('whoops!'))
})

test_that('is.response succeeds / fails correctly', {
  res <- response()
  expect_true(is.response(res))
  expect_false(is.response(FALSE))
  expect_false(is.response(3030))
  expect_false(is.response('bombos ether quake'))
})

test_that('response defaults set when created', {
  res <- response()
  expect_equal(res[['Content-Type']], 'text/plain')
  expect_equal(status(res), 200)
  expect_equal(body(res), '')
})

test_that('get values with `[[`', {
  res <- response()
  
  expect_equal(res[['Content-Type']], 'text/plain')
  expect_null(res[['missing']])
  expect_null(res[['whoops']])
})

test_that('set field values `[[<-`', {
  res <- response()

  res[['Connection']] <- 'close'
  res[['Content-Length']] <- 3030
  res[['Warning']] <- 'challenger approaching'

  expect_equal(res[['Connection']], 'close')
  expect_equal(res[['Content-Length']], 3030)
  expect_equal(res[['Warning']], 'challenger approaching')
})
