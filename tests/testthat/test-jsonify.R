context('jsonify objects')

test_that('arguments passed to `as.json` pass to `toJSON`', {
  skip_if_not_installed('jsonlite')
  father_time <- Sys.time()
  frame_data <- data.frame(top = c(1:5), bottom = c(10:6), time = rep(father_time, 5))
  expect_equal(jsonlite::toJSON(frame_data), as.json(frame_data))
  expect_equal(jsonlite::toJSON(frame_data, dataframe = 'rows'), as.json(frame_data, dataframe = 'rows'))
  expect_equal(jsonlite::toJSON(frame_data, dataframe = 'columns', Date = 'epoch'), as.json(frame_data, dataframe = 'columns', Date = 'epoch'))
})

test_that('setting body as data.frame automatically calls as.json', {
  skip_if_not_installed('jsonlite')
  res <- response()
  body(res) <- data.frame(one = 'fish', two = 'fish')
  expect_true(is.json(body(res)))
  expect_true(is.character(body(res)))
  expect_false(is.data.frame(body(res)))
  expect_true(is.response(res))
  expect_equal(res[['Content-Type']], 'application/json')
})

test_that('setting body as list automatically calls as.json', {
  skip_if_not_installed('jsonlite')
  res <- response()
  body(res) <- list(red = 'fish', blue = 'fish')
  expect_true(is.json(body(res)))
  expect_true(is.character(body(res)))
  expect_false(is.list(body(res)))
  expect_true(is.response(res))
  expect_equal(res[['Content-Type']], 'application/json')
})
