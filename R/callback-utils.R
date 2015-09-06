body <- function(x, ...) UseMethod('body', x)

#
# request helpers
#
body.request <- function(.req) .req$body

method <- function(.req) .req$method

ip <- function(.req) .req$ip

port <- function(.req) .req$port

host_name <- function(.req) .req$host_name

params <- function(.req) .req$params

field <- function(.req, field) .req$get_header_field(field)

is <- function(.req, type) .req$has_content_type(type)

original_url <- function(.req, type) .req$url

#
# response helpers
#
status <- function(.res, status_code) {
  .res$set_status(status_code)

  invisible(.res)
}

headers <- function(.res, ...) {
  args <- list(...)

  if (length(names(args)) != length(args)) {
    stop('All arguments must be named')
  }

  sapply(names(args), function(field_name) {
    field_value <- args[[field_name]]
    .res$add_headers(setNames(field_value, rep(field_name, times = length(field_value))))
  })

  invisible(.res)
}

body.response <- function(.res, expr) {
  .res$set_body(expr)

  invisible(.res)
}

send <- function(.res, body = NULL) {
  if (is.null(body)) {
    .res$end()
  } else {
    .res$set_body(body)
    .res$end()
  }
}

redirect <- function(.res, url, status_code = NULL) {
  .res$set_status(ifelse(is.null(status_code), 302, status_code))
  .res$add_headers(list(Location = url))
  .res$end()
}

load_helpers <- function(callback) {
  environment(callback) <- list2env(
    x = list(
      body = body,
      body.request = body.request,
      body.response = body.response,
      method = method,
      ip = ip,
      port = port,
      host_name = host_name,
      params = params,
      field = field,
      is = is,
      original_url = original_url,
      status = status,
      headers = headers,
      send = send,
      redirect = redirect
    ),
    parent = environment(callback)
  )
  callback
}
