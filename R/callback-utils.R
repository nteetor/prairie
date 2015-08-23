body <- function(x, ...) UseMethod('body', x)

body.request <- function(.req) .req$body

body.response <- function(.res, expr) {
  .res$set_body(expr)
  
  invisible(.res)
}

method <- function(.req) .req$method

ip <- function(.req) .req$ip

port <- function(.req) .req$port

host_name <- function(.req) .req$host_name

params <- function(.req) .req$params

field <- function(.req, field) .req$get_header_field(field)

is <- function(.req, type) .req$has_content_type(type)

status <- function(.res, status) {
  .res$set_status(status)
  
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
      status = status,
      headers = headers
    ),
    parent = environment(callback)
  )
  callback
}
