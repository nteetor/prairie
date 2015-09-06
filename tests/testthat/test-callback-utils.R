library(dull)
context('callback utility functions')

test_that('all callback utilities loaded', {
  callback_envir <- environment(load_callback_envir(function(req, res) NULL))

  expect_exists <- function(name) {
    eval(bquote(
      expect_true(exists(.(name), where = callback_envir, inherits = FALSE))
    ))
  }

  for (nm in names(dull:::callback_utils)) {
    expect_exists(nm)
  }
})
