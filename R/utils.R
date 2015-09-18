# borrowing from javascript, will return
"%||%" <- function(a, b) if (is.null(a)) b else a

http_date <- function(date_time) {
  strftime(date_time, format = '%a, %d %b %Y %H:%M:%S', usetz = TRUE)
}

# borrowed from expressjs
is_absolute <- function(path) {
  if (substr(path, 1, 1) == '/') TRUE
  else if (substr(path, 2, 2) == ':' & substr(path, 3, 3) == '\\') TRUE
  else if (substr(path, 1, 2) == '\\\\') TRUE
  else FALSE
}
