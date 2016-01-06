#' Coerce Response to List
#' 
#' Used to format a response object in the acceptable httpuv format.
#' 
#' @param x A response object.
#' @param \dots Ignored.
#' 
#' @keywords internal
#' @export
#' @name as.list.response
as.list.response <- function(x, ...) {
  list(
    status = x$status_code,
    headers = x$headers,
    body = x$body
  )
}
