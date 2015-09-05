library(dull)
context('callback utility functions')

test_that('all response utilities loaded', {
  loaded_callback <- load_helpers(function(req, res) NULL)

  expect_exists <- function(name) {
    eval(bquote(
      expect_true(exists(.(name), where = environment(loaded_callback), inherits = FALSE))
    ))
  }

  expect_exists('headers')
  expect_exists('status')
  expect_exists('body')
  expect_exists('body.response')
})

test_that('all request utilities loaded', {
  loaded_callback <- load_helpers(function(req, res) NULL)

  expect_exists <- function(name) {
    eval(bquote(
      expect_true(exists(.(name), where = environment(loaded_callback), inherits = FALSE))
    ))
  }

  expect_exists('body')
  expect_exists('body.request')
  expect_exists('body.response')
  expect_exists('method')
  expect_exists('ip')
  expect_exists('port')
  expect_exists('host_name')
  expect_exists('params')
  expect_exists('field')
  expect_exists('is')
  expect_exists('send')
  expect_exists('redirect')
})
