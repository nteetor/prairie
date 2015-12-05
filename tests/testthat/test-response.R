context('response class')

test_that('responses created properly', {
  expect_silent(res <- response())
  expect_true(is.response(res))
  expect_false(is.response(data.frame()))
  expect_false(is.response(list()))
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

test_that('get values with `[` and `[[`', {
  res <- response()

  expect_equal(res[['Content-Type']], 'text/plain')
  expect_null(res[['missing']])
  expect_null(res[['whoops']])
  expect_equal(res[c('Content-Type')], list(`Content-Type` = 'text/plain'))
})

test_that('set field values `[[<-`', {
  res <- response()

  res[['Connection']] <- 'close'
  res[['Content-Length']] <- 3030
  res[['Warning']] <- 'challenger approaching'

  expect_equal(res[['Connection']], 'close')
  expect_equal(res[['Content-Length']], 3030)
  expect_equal(res[['Warning']], 'challenger approaching')
  expect_equal(res[c('Connection', 'Content-Length')], list(Connection = 'close', `Content-Length` = 3030))
})

test_that('get status of response with status.response', {
  res <- response()

  expect_equal(status(res), 200)
})

test_that('set status of response with status<-.response', {
  res <- response()

  status(res) <- 301
  expect_equal(status(res), 301)
  status(res) <- 502
  expect_equal(status(res), 502)
})

test_that('get body of response with body.response', {
  res <- response()

  expect_equal(body(res), '')
})

test_that('set body of response with body<-.response', {
  res <- response()

  body(res) <- 'head shoulders knees and toes'
  expect_equal(body(res), 'head shoulders knees and toes')
  body(res) <- 'eyes and ears and mouth and nose'
  expect_equal(body(res), 'eyes and ears and mouth and nose')
})
