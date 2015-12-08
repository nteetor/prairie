#' Test if response
#'
#' \code{TRUE} if object is a response.
#'
#' @param obj An \R object
#'
#' @export
#' @examples
#' # FALSE
#' is.response(logical(1))
#'
#' # TRUE
#' is.response(response())
#'
#' # FALSE
#' is.response(3030)
is.response <- function(obj) inherits(obj, 'response')

