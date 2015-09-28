# borrowing from javascript, will return
"%||%" <- function(a, b) if (is.null(a)) b else a

http_date <- function(date_time) {
  strftime(date_time, format = '%a, %d %b %Y %H:%M:%S', usetz = TRUE)
}

is_named <- function(lst) {
  if (length(lst) == 0) FALSE
  else if (any(names(lst) == '')) FALSE
  else if (any(is.null(names(lst)))) FALSE
  else TRUE
}

# borrowed from expressjs
is_absolute <- function(path) {
  if (substr(path, 1, 1) == '/') TRUE
  else if (substr(path, 2, 2) == ':' & substr(path, 3, 3) == '\\') TRUE
  else if (substr(path, 1, 2) == '\\\\') TRUE
  else FALSE
}

get_status_description <- function(status, default_to_status = TRUE) {
  stopifnot(is.integer(status) | is.character(status))
  switch (
    as.character(status),
    '100' = "Continue",
    '101' = "Switching Protocols",
    '200' = "OK",
    '201' = "Created",
    '202' = "Accepted",
    '203' = "Non-Authoritative Information",
    '204' = "No Content",
    '205' = "Reset Content",
    '206' = "Partial Content",
    '300' = "Multiple Choices",
    '301' = "Moved Permanently",
    '302' = "Found",
    '303' = "See Other",
    '304' = "Not Modified",
    '305' = "Use Proxy",
    '307' = "Temporary Redirect",
    '400' = "Bad Request",
    '401' = "Unauthorized",
    '402' = "Payment Required",
    '403' = "Forbidden",
    '404' = "Not Found",
    '405' = "Method Not Allowed",
    '406' = "Not Acceptable",
    '407' = "Proxy Authentication Required",
    '408' = "Request Timeout",
    '409' = "Conflict",
    '410' = "Gone",
    '411' = "Length Required",
    '412' = "Precondition Failed",
    '413' = "Request Entity Too Large",
    '414' = "Request-URI Too Long",
    '415' = "Unsupported Media Type",
    '416' = "Requested Range Not Satisifable",
    '417' = "Expectation Failed",
    '500' = "Internal Server Error",
    '501' = "Not Implemented",
    '502' = "Bad Gateway",
    '503' = "Service Unavailable",
    '504' = "Gateway Timeout",
    '505' = "HTTP Version Not Supported",
    if (default_to_status) as.character(status) else NULL
  )
}
