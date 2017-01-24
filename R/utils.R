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

is.date <- function(x) {
  inherits(x, 'Date') | inherits(x, 'POSIXt')
}

http_date <- function(x) {
  if (!(inherits(x, 'Date') || inherits(x, 'POSIXt'))) {
    stop('argument `x` must be of class Date or POSIXt', call. = FALSE)
  }
  strftime(x, format = '%a, %d %b %Y %H:%M:%S', usetz = TRUE)
}

collapse <- function(..., sep = ', ') {
  paste(..., sep = sep, collapse = sep)
}

frmt_header <- function(s) {
  if (grepl('^HTTP_', s)) {
    s <- sub('^HTTP_', '', s)
  }

  cap <- function(.s) {
    paste0(toupper(substring(.s, 1, 1)), tolower(substring(.s, 2)))
  }

  collapse(lapply(strsplit(s, '_'), cap)[[1]], sep = '-')
}

is_named <- function(l) {
  length(l) != 0 && all(names(l) != '') && all(!is.null(names(l)))
}

set_names <- function(x, names) {
  names(x) <- names
  x
}

# borrowed from expressjs
is_absolute <- function(path) {
  if (!is.character(path)) {
    stop('argument `path` must be of class character', call. = FALSE)
  }
  if (substr(path, 1, 1) == '/') TRUE
  else if (substr(path, 2, 2) == ':' & substr(path, 3, 3) == '\\') TRUE
  else if (substr(path, 1, 2) == '\\\\') TRUE
  else FALSE
}

is_readable <- function(path) {
  (file.access(path, mode = 4) == 0)[[1]]
}

conjunction <- function(x, coordinator = 'and') {
  if (length(x) == 1) {
    x
  } else if (length(x) == 2) {
    paste(x[[1]], 'and', x[[2]])
  } else {
    paste0(paste(x[1:(length(x) - 1)], collapse = ', '), ', and ',
           x[[length(x)]])
  }
}
