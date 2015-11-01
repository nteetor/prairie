#' Render an HTML page
#'
#'
#' @importFrom assertthat assert_that
#' @keywords internal
#' @export
render <- function(path, locals = NULL) {
  assert_that(file.exists(path))
  if (!is.null(locals)) {
    assert_that(is.list(locals), is_named(locals))

    with(locals, eval(parse(path)))
  } else {
  }
}

