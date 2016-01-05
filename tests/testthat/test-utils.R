context('util functions')

test_that('`%||%` function', {
  expect_true(is.function(`%||%`))
  expect_equal(3030, NULL %||% 3030)
})

test_that('http_date function', {
  date1 <- strptime('11/06/1994 08:49:37', '%m/%d/%Y %H:%M:%S', tz = 'UTC')
  expect_equal('Sun, 06 Nov 1994 08:49:37 UTC', http_date(date1))

  date2 <- strptime('12/04/2015', '%m/%d/%Y', tz = 'UTC')
  expect_equal('Fri, 04 Dec 2015 00:00:00 UTC', http_date(date2))

  expect_error(http_date('01/01/3030'))
  expect_error(http_date(3030))
})

test_that('is_named function', {
  expect_false(is_named('voldemort'))
  expect_false(is_named(9.75))
  dark_lord <- 'voldemort'
  names(dark_lord) <- 'he who must not be named'
  expect_true(is_named(dark_lord))

  expect_true(is_named(list(one = 'fish', two = 'fish')))
  expect_false(is_named(list()))
  expect_false(is_named(list('tombs', of = 'atuan')))
  expect_false(is_named(list('deltron', 3030)))

  expect_true(is_named(c(little = 'bunny', foo = 'foo')))
  expect_false(is_named(1:5))
})

test_that('is_absolute function', {
  expect_false(is_absolute('edward'))
  expect_error(is_absolute(36))

  expect_true(is_absolute('/yellow/brick/road'))
  expect_false(is_absolute('../we/re/in/kansas'))

  expect_true(is_absolute('C:\\is\\for\\cookie'))
  expect_true(is_absolute('\\\\tires\\tires\\not\\really'))
})

test_that('zfill and bfill functions', {
  expect_equal(zfill(5), '00000')
  expect_equal(zfill(3, '|'), '|||')
  expect_equal(bfill(2), '  ')
  expect_equal(bfill(6), '      ')
})

test_that('status reason_phrase function', {
  expect_error(reason_phrase(TRUE), 'is.numeric(status_code) || is.character(status_code)')
  expect_equal(reason_phrase(100), 'Continue')
  expect_equal(reason_phrase(404), 'Not Found')
  expect_equal(reason_phrase(515), '')
})
