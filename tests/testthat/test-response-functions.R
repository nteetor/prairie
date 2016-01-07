context('response functions / utils')

test_that('response coerces to list', {
  res <- response()
  status(res) <- 405
  body(res) <- 'hello, world'
  res[['Content-Length']] <- 3030
  
  reslive <- as.list(res)
  
  expect_true(reslive %has_name% 'status')
  expect_true(reslive %has_name% 'headers')
  expect_true(reslive %has_name% 'body')
  
  expect_equal(reslive$status, 405)
  expect_equal(reslive$headers, list(`Content-Type` = 'text/plain', `Content-Length` = 3030))
  expect_equal(reslive$body, 'hello, world')
})
