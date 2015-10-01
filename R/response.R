#' Response objects
#'
#' An R6Class respresentation of an HTTP response. The response object is used
#' inside route callback functions where it can be
#'
#' @format
#'
#' An R6 class object.
#'
#' @section Fields:
#'
#'   None.
#'
#' @section Methods:
#'
#'   Response objects understand the following methods. Many of these methods
#'   invisibly return the response object, which allows multiple methods to be
#'   chained together. The methods which cannot be chained return a value, such
#'   as \code{get}.
#'
#'   \subsection{\code{res$append(field, value)}}
#'
#'   Append \code{value} to the existing value for header field \code{field}.
#'
#'   \subsection{\code{res$attachment(path = NULL)}}
#'
#'   Add \code{path} as the response attachment. Calling \code{attachment} will
#'   set the "Content-Disposition" and "Content-Type" HTTP header fields of the
#'   response object.
#'
#'   If \code{path} is specified the path is split, attchment file name parsed,
#'   and the file name of "Content-Disposition" set accordingly.
#'
#'   \subsection{\code{res$download(path, filename = path)}}
#'
#'   Prompts the client to a file specified by \code{path}. An alternate name
#'   may be specified by \code{filename}.
#'
#'   Ends the callback function.
#'
#'   \subsection{\code{res$end(body = NULL)}}
#'
#'   Ends the callback function. Typically used to halt callback execution when
#'   a request is incomplete. If \code{body} is not NULL then the response body
#'   will be overwritten when \code{end} is called.
#'
#'   Ends the callback function.
#'
#'   \code{as_HTTP_response()} Formats and returns the response object as an
#'   HTTP response
#'
#'   \code{as_Rook_response()} Formats and returns the response object as a Rook
#'   reponse
#'
#' @examples
#' sample <- dull$route('^.+', function(req, res) {
#'   res$set('Connection', 'close')
#'
#'   res$status(301)$send('Page is gone!')
#' })
#'
#' @docType class
#' @keywords internal
#' @importFrom assertthat assert_that
#' @export
#' @name response
response <- R6::R6Class(
  'response',
  public = list(
    initialize = function(req) {
      private$status_code <- 200L
      private$headers <- list('Content-Type' = 'text/plain')
      private$body <- ''
      private$req <- req
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
    cookie = function(name, value, options = NULL) {
      stop('$cookie not implemented')
    },
    clear_cookie = function(name, options = NULL) {
      stop('$clear_cookie not implemented')
    },
    download = function(path, filename = path) {
      assert_that(is.character(path), file.exists(path), is.character(filename))

      # self$set('Content-Disposition', paste0('attachment; filename=\"', filename, '\"'))

      self$send_file(path, list('Content-Disposition' = paste0('attachment; filename=\"', filename, '\"')))
    },
    end = function(body = NULL) {
      if (!is.null(body)) {
        assert_that(is.character(body))
        private$body <- body
      }

      stop(
        structure(
          class = c('end_signal', 'error', 'condition'),
          list(
            message = '(not really an error) $end called from response'
          )
        )
      )
    },
    format = function(lst) {
      assert_that(is.list(lst))

      default_callback <- lst[['default']]
      lst['default'] <- NULL

      accepted_type <- Find(private$req$accepts, names(lst), nomatch = NULL)

      if (!is.null(accepted_type)) {
        res$set('Content-Type', mime::guess_type(accepted_type))
        lst[[accepted_type]]()
      } else if (!is.null(default_callback)) {
        default_callback()
      } else {
        self$send_status(406)
      }

      invisible(self)
    },
    get = function(field) {
      assert_that(is.character(field))
      field <- Find(function(nm) tolower(nm) == tolower(field), names(private$headers), nomatch = NA)
      private$headers[[field]]
    },
    json = function(body = NULL) {
      if (!requireNamespace('jsonlite')) {
        stop('package jsonlite must be installed in order to use method `json`')
      }

      if (!is.null(body)) {
        assert_that(is.character(body) | is.list(body) | is.data.frame(body))

        private$body <- jsonlite::toJSON(body)
      }

      self$set('Content-Type', 'application/json')

      self$end()
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
    redirect = function(path, status = 302L) {
      assert_that(is.character(path), is.integer(status))

      self$status(status)
      self$location(path)

      self$end()
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
          private$body <- body
        }
      }

      self$end()
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

      full_path <- httpuv::encodeURI(file.path(root, path))

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

      private$body <- setNames(full_path, 'file')
      self$end()
    },
    send_status = function(status) {
      assert_that(is.numeric(status))
      self$status(status)
      private$body <- get_status_description(status)
      self$end()
    },
    set = function(field, value) {
      assert_that(is.character(field), is.null(value) || is.character(value))
      private$headers[[field]] <- value
      invisible(self)
    },
    status = function(code) {
      assert_that(is.numeric(code))
      private$status_code <- code
      invisible(self)
    },
    type = function(type) {
      assert_that(is.character(type))
      self$set('Content-Type', mime::guess_type(type, empty = type))
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
      cat(
        paste('HTTP/1.1', private$status_code, get_status_description(private$status_code, FALSE)),
        '\r\n',
        paste0(names(private$headers), ': ', private$headers, collapse = '\r\n'),
        '\r\n\r\n',
        paste0(private$body),
        '\r\n',
        sep = ''
      )
    },
    as_Rook_response = function() {
      list(
        status = private$status_code,
        headers = private$headers,
        body = private$body
      )
    }
  ),
  private = list(
    status_code = NULL,
    body = NULL,
    headers = NULL,
    req = NULL
  )
)
