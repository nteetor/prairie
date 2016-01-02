# borrowing from javascript, will return
`%||%` <- function(a, b) if (is.null(a)) b else a

mimeextra <- c(
  md = "text/markdown",
  markdown = "text/markdown",
  r = "text/plain",
  rd = "text/plain",
  rmd = "text/markdown",
  geojson = "application/vnd.geo+json",
  NULL
)

http_date <- function(date_time) {
  assert_that(is.date(date_time) || is.time(date_time))
  strftime(date_time, format = '%a, %d %b %Y %H:%M:%S', usetz = TRUE)
}

zfill <- function(n, z = '0') {
  paste(rep(z, times = n), collapse = '')
}

bfill <- function(n) zfill(n, ' ')

capitalize_header <- function(s) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = '')
  paste(vapply(strsplit(s, split = '-')[[1]], cap, character(1), USE.NAMES = FALSE), collapse = '-')
}

is_match <- function(rte, req) {
  grepl(rte$path, req$uri) && (rte$method == req$method || rte$method == 'all')
}

is_named <- function(lst) {
  if (length(lst) == 0) FALSE
  else if (any(names(lst) == '')) FALSE
  else if (any(is.null(names(lst)))) FALSE
  else TRUE
}

# borrowed from expressjs
is_absolute <- function(path) {
  assert_that(is.character(path))
  if (substr(path, 1, 1) == '/') TRUE
  else if (substr(path, 2, 2) == ':' & substr(path, 3, 3) == '\\') TRUE
  else if (substr(path, 1, 2) == '\\\\') TRUE
  else FALSE
}
