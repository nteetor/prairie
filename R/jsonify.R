#' Convert Objects to JSON
#'
#' When building a web server or API it is often useful to send more complex
#' data obects. Within prairie one can do this by converting the object to JSON.
#' Additionally, prairie exposes the generic \code{as.json} function allowing
#' users to specify how their custom classes need to be converted to JSON.
#'
#' @details
#'
#' For the included \code{as.json} functions, the excellent package
#' \code{jsonlite} does all the heavy lifting behind the scenes. This package
#' is straightforward to use and is recommended for those who wish to create
#' further \code{as.json} functions.
#'
#' @name json
#' @examples
#' as.json(list(one = 'fish', two = 'fish'))
#' as.json(data.frame(red = 'fish', blue = 'fish'))
#'
#' # setting a response object body as a data.frame or list
#' # will automatically convert the object to JSON and set
#' # Content-Type of the response as "application/json"
#'
#' res <- response()
#' body(res) <- list(
#'   list(name = 'ged', occupation = 'wizard'),
#'   list(name = 'sparrowhawk', occupation = 'wizard')
#' )
#' is.json(body(res))  # TRUE
#' res[['Content-Type']] == 'application/json' # TRUE
NULL

#' @param x Any \R object.
#' @export
#' @rdname json
is.json <- function(x) inherits(x, 'json')

#' @param \ldots Additional arguments to pass on to \code{jsonlite::toJSON}.
#' @export
#' @rdname json
as.json <- function(x, ...) UseMethod('as.json')

#' @export
#' @rdname json
as.json.list <- function(x, ...) {
  if (!requireNamespace('jsonlite', quietly = TRUE)) {
    stop('package "jsonlite" must be installed', call. = FALSE)
  }
  jsonlite::toJSON(x, ...)
}

#' @export
#' @rdname json
as.json.data.frame <- function(x, ...) {
  if (!requireNamespace('jsonlite', quietly = TRUE)) {
    stop('package "jsonlite" must be installed', call. = FALSE)
  }
  jsonlite::toJSON(x, ...)
}
