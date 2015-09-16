body <- function(x, ...) UseMethod('body', x)

#' Retrieve the body of a request object
#'
#' @param .req \code{response} object
#'
#' @name request-body
#' @examples
#' library(magrittr)
#'
#' dull() %>%
#'   post('^upload/csv/$', function(req, res) {
#'     file_name <- tempfile(pattern = 'upload_', fileext = '.csv', tmpdir = 'public')
#'
#'     # extract body from request and write to temp file
#'     writeBin(body(req), file_name)
#'
#'     res %>%
#'       send('Upload successful!')
#'   })
body.request <- function(.req) .req$body

#' Add body to response object
#'
#' @name response-body
body.response <- function(.res, expr) {
  .res$set_body(expr)

  invisible(res)
}

callback_utils <- list(
  body = body,
  body.request = body.request,
  body.response = body.response,
  method = function(.req) .req$method,
  ip = function(.req) .req$ip,
  port = function(.req) .req$port,
  host_name = function(.req) .req$host_name,
  params = function(.req) .req$params,
  field = function(.req, field) .req$get_header_field(field),
  is = function(.req, type) .req$has_content_type(type),
  original_url = function(.req, type) .req$url,
  status = function(.res, status_code) {
    .res$set_status(status_code)

    invisible(.res)
  },
  headers = function(.res, ...) {
    args <- list(...)

    if (length(names(args)) != length(args)) {
      stop('All arguments must be named')
    }

    sapply(names(args), function(field_name) {
      field_value <- args[[field_name]]
      .res$add_headers(setNames(field_value, rep(field_name, times = length(field_value))))
    })

    invisible(.res)
  },
  render = function(.res, path) {
    stopifnot(is.character(path), file.exists(path))

    .res$render_body(path)
    .res$set_content_type('text/html')
    .res$end()
  },
  send = function(.res, body = NULL) {
    if (is.null(body)) {
      .res$end()
    } else {
      .res$set_body(body)
      .res$end()
    }
  },
  send_file = function(.res, path) {
    stopifnot(is.character(path), file.exists(path))

    path_info <- file.info(path, extra_cols = FALSE)
    .res$set_body(readBin(path, 'raw', path_info$size))
    .res$set_content_type(mime::guess_type(path))
    .res$set_content_length(path_info$size)
    .res$end()
  },
  redirect = function(.res, url, status_code = NULL) {
    .res$set_status(ifelse(is.null(status_code), 302, status_code))
    .res$add_headers(list(Location = url))
    .res$end()
  }
)

load_callback_envir <- function(callback) {
  environment(callback) <- list2env(
    x = callback_utils,
    parent = environment(callback)
  )
  callback
}
