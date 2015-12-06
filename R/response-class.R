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

response__ <- R6::R6Class(
  'response',
  active = list(
    body = function(value) {
      if (missing(value)) {
        private$body_
      } else {
        if (is.list(value) || is.data.frame(value)) {
          private$body_ <- as.json(value)
          self$set('Content-Type', 'application/json')
        } else {
          private$body_ <- value
        }
        invisible(self)
      }
    },
    status = function(code) {
      if (missing(code)) {
        private$status_code
      } else {
        assert_that(is.numeric(code))

        private$status_code <- code
        invisible(self)
      }
    }
  ),
  public = list(
    initialize = function() {
      private$status_code <- 200
      private$headers <- list('Content-Type' = 'text/plain')
      private$body_ <- ''

      invisible(self)
    },

    append = function(field, value) {
      assert_that(is.character(field), is.character(value))

      if (!is.null(self$get(field))) {
        value <- paste(self$get(field), paste(value, collapse = ''), sep = '')
      }

      self$set(field, value)

      invisible(self)
    },
    attachment = function(path = NULL) {
      if (is.null(path)) {
        self$set('Content-Disposition', 'attachment')
      } else {
        assert_that(is.character(path), file.exists(path))

        split_path <- strsplit(path, '/|\\\\', perl = TRUE)[[1]]
        f_name <- split_path[length(split_path)]

        self$set('Content-Disposition', paste0('attachment; filename=\"', f_name, '\"'))
        self$set('Content-Type', mime::guess_type(f_name))
      }

      invisible(self)
    },
    cookie = function(name, value, expires = NULL) {
      assert_that(is.character(name), is.character(value), is.list(options))

      cookie_string <- paste0(name, '=', value)

      if (!is.null(expires)) {
        assert_that(is.time(expires) || is.date(expires))
        cookie_string <- paste0(cookie_string, '; ', 'Expires=', http_date(expires))
      }

      private$headers <- append(private$headers, list('Set-Cookie' = cookie_string))

      invisible(self)
    },
    clear_cookie = function(name) {
      assert_that(is.character(name))

      self$cookie(name, '', expires = Sys.time())

      invisible(self)
    },
    download = function(path, filename = path) {
      assert_that(is.character(path), file.exists(path), is.character(filename))

      # self$set('Content-Disposition', paste0('attachment; filename=\"', filename, '\"'))

      self$send_file(path, list('Content-Disposition' = paste0('attachment; filename=\"', filename, '\"')))

      invisible(self)
    },
#     format = function(callbacks) {
#       assert_that(is.list(callbacks), is_named(callbacks))
#
#       default_callback <- callbacks$default
#       callbacks$default <- NULL
#
#       accepted_type <- Find(private$req$accepts, names(callbacks), nomatch = NULL)
#
#       if (!is.null(accepted_type)) {
#         self$set('Content-Type', mime::guess_type(accepted_type))
#         callbacks[[accepted_type]]()
#         self$end()
#       } else if (!is.null(default_callback)) {
#         default_callback()
#         self$end()
#       } else {
#         self$send_status(406)
#       }
#     },
    get = function(field) {
      assert_that(is.character(field))

      private$headers[[field]]
    },
    json = function(body = NULL) {
      if (!requireNamespace('jsonlite')) {
        stop('package jsonlite must be installed in order to use method `json`')
      }

      if (!is.null(body)) {
        assert_that(is.character(body) | is.list(body) | is.data.frame(body))

        private$body_ <- jsonlite::toJSON(body)
      }

      self$set('Content-Type', 'application/json')

      invisible(self)
    },
    links = function(links) {
      assert_that(is.list(links), length(links) != 0)

      existing_links <- self$get('Link')
      new_links <- paste(
        vapply(names(links), function(rel) paste0('<', links[[rel]], '>; rel=', rel), character(1)),
        collapse = ', '
      )

      if (is.null(existing_links)) {
        self$set('Link', new_links)
      } else {
        self$set('Link', paste0(existing_links, ', ', new_links))
      }

      invisible(self)
    },
    location = function(path) {
      assert_that(is.character(path))

      if (path == 'back') {
        path <- private$req$get('Referrer') %||% '/'
      }

      self$set('Location', path)

      invisible(self)
    },
    redirect = function(path, status = 302) {
      assert_that(is.character(path), is.numeric(status))

      self$status(status)
      self$location(path)

      invisible(self)
    },
    render = function(view, locals) {
      stop('$render not implemented')
    },
    send = function(body = NULL) {
      if (!is.null(body)) {
        assert_that(is.character(body) || is.list(body) || is.data.frame(body))

        if (is.list(body) || is.data.frame(body)) {
          self$json(body)
        } else {
          self$set('Content-Type', 'text/html')
          private$body_ <- body
        }
      }

      invisible(self)
    },
    send_file = function(path, options = list(), ...) {
      assert_that(is.character(path), is.list(options))

      all_options <- append(options, list(...))
      if (length(all_options) && !is_named(all_options)) {
        stop('all options must be named')
      }

      if (!all(names(all_options) %in% c('max_age', 'root', 'last_modified', 'headers', 'dotfiles'))) {
        stop('unknown options passed to $send_file')
      }

      root <- all_options$root
      if (is.null(root) && !is_absolute(path)) {
        stop('option `root` must be specified or `path` must be absolute')
      }

      full_path <- encodeURI(file.path(root, path))

      if (!is.null(all_options$headers)) {
        if (!is_named(all_options$headers)) {
          stop('values of option `headers` must be named')
        }

        for (nm in names(all_options$headers)) {
          self$set(nm, all_options$headers[[nm]])
        }
      }

      if (is.null(all_options$max_age)) {
        self$set('Cache-Control', paste0('max-age=', 0))
      } else {
        self$set('Cache-Control', paste0('max-age=', all_options$max_age))
      }

      if (is.null(all_options$last_modified) || all_options$last_modified) {
        self$set('Last-Modified', http_date(file.mtime(full_path)))
      } else {
        self$set('Last-Modified', NULL)
      }

      if (!is.null(all_options$dot_files)) {
        if (!(all_options$dot_files %in% c('allow', 'deny', 'ignore'))) {
          stop('unknown value for option `dot_files`, must be one of "allow", "deny", or "ignore"')
        }
        warning('option `dot_files` is not implemented')
      }

      private$body_ <- setNames(full_path, 'file')

      invisible(self)
    },
    send_status = function(status) {
      assert_that(is.numeric(status))
      self$status(status)
      private$body_ <- get_status_description(status)

      invisible(self)
    },
    set = function(field, value) {
      assert_that(is.character(field))
      private$headers[[field]] <- value

      invisible(self)
    },
    type = function(type) {
      assert_that(is.character(type))
      self$set('Content-Type', mime::guess_type(type, empty = type, mime_extra = mimeextra))

      invisible(self)
    },
    vary = function(field) {
      assert_that(is.character(field))
      if (is.null(self$get('Vary'))) {
        self$set('Vary', field)
      } else {
        self$append('Vary', paste0(',', field))
      }

      invisible(self)
    },

    as_HTTP_response = function() {
      headers <- lapply(private$headers, as.character)
      cat(
        paste('HTTP/1.1', private$status_code, get_status_description(private$status_code, FALSE))
      )
      if (!is.null(headers)) {
        cat(
          '\r\n',
          paste0(names(headers), ': ', headers, collapse = '\r\n'),
          sep = ''
        )
      }
      if (nchar(private$body_) > 0) {
        cat(
          '\r\n\r\n',
          paste0(private$body_),
          '\r\n',
          sep = ''
        )
      }
    },
    as_Rook_response = function() {
      list(
        status = private$status_code,
        headers = lapply(private$headers, as.character),
        body = private$body_
      )
    },

    print = function() {
      self$as_HTTP_response()
    }
  ),
  private = list(
    status_code = NULL,
    body_ = NULL,
    headers = NULL
  )
)
