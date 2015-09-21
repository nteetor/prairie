#' Response class
#'
#' An R6 class representing an HTTP response. Additionally, the response object
#' can be converted into a Rook response for use with the httpuv and Rook
#' packages.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{add_headers(headers)} Add a list of HTTP headers and values to the response
#'  object
#'  \item \code{set_body(expr)} Sets the body of the response, the value of \code{expr} is treated as html
#'  \item \code{set_status(n)} Sets the HTTP status of the response object
#'  \item \code{as_HTTP_response()} Formats and returns the response object as an HTTP response
#'  \item \code{as_Rook_response()} Formats and returns the response object as a Rook reponse
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom assertthat assert_that
#' @export
#' @name response
response <- R6::R6Class(
  'response',
  public = list(
    initialize = function(req) {
      private$status <- 200
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
        private$headers[['Content-Disposition']] <- 'attachment'
      } else {
        assert_that(is.character(path), file.exists(path))

        split_path <- strsplit(path, '/|\\', perl = TRUE)[[1]]
        f_name <- split_path[length(split_path)]

        private$headers[['Content-Disposition']] <- paste0('attachment; filename=\"', f_name, '\"')
        private$headers[['Content-Type']] <- mime::guess_type(f_name)
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
      assert_that(is.character(path), file.exists(path))
      if (!is.null(filename)) assert_that(filename)

      self$attachment(filename)

      stop('$send_file not implemented')
    },
    end = function(body = NULL) {
      if (!is.null(body)) {
        assert_that(is.character(body))

        private$body <- body
      }

      stop(
        structure(
          class = c('end_response', 'message', 'condition'),
          list(
            message = 'response has ended',
            call = sys.call(-1)
          )
        )
      )
    },
    format = function(obj) {
      assert_that(is.list(obj))

      default_callback <- obj[['default']]
      obj['default'] <- NULL

      accepted_type <- Find(private$req$accepts, names(obj), nomatch = NULL)

      if (!is.null(accepted_type)) {
        res$set('Content-Type', mime::guess_type(accepted_type))
        obj[[accepted_type]]()
      } else if (!is.null(default_callback)) {
        default_callback()
      } else {
        self$status(406)
      }

      invisible(self)
    },
    get = function(field) {
      self$headers[[stringr::str_to_lower(field)]]
    },
    json = function(body = NULL) {
      if (!requireNamespace('jsonlite')) {
        stop('package jsonlite is not installed')
      }

      assert_that(is.character(body) | is.list(body) | is.data.frame(body))

      self$set('Content-Type', 'application/json')
      private$body <- jsonlite::toJSON(body)

      invisible(self)
    },
    links = function(links) {
      assert_that(is.list(links), length(links) != 0)

      existing_links <- self$get('Link')

      self$set(
        'Link',
        paste(
          existing_links,
          paste(
            vapply(names(links), function(rel) paste0('<', links[[rel]], '>; rel=', rel), character(1)),
            collapse = ', '
          ),
          sep = ', '
        )
      )

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
      assert_that(is.character(body) | is.list(body) | is.data.frame(body))

      if (is.list(body) | is.data.frame(body)) {
        self$json(body)
      }

      self$set('Content-Type', 'text/html')
      private$body <- body

      invisible(self)
    },
    send_file = function(path, options = list(), ...) {
      assert_that(is.character(path), is.list(options))

      all_options <- append(options, list(...))
      if (!all(names(all_options) %in% c('max_age', 'root', 'last_modified', 'headers', 'dotfiles'))) {
        stop('unknown options passed to $send_file')
      }
      
      root <- all_options$root
      if (is.null(root) & !is_absolute(path)) {
        stop('option `root` must be specified or`path` must be absolute')
      }
      
      full_path <- httpuv::encode(file.path(root, path))
      
      if (!is.null(all_options$headers)) {
        if (length(names(all_options$headers)) != length(all_options$headers)) {
          stop('values of option `headers` must be named')
        }
        
        for (nm in names(all_options)) {
          self$set(nm, all_options$headers[[nm]])
        }
      }
      
      if (is.null(all_options$max_age)) {
        self$set('Cache-Control', paste0('max-age=', 0))
      } else {
        self$set('Cache-Control', paste0('max-age=', all_options$max_age))
      }
      
      if (is.null(all_options$last_modified) | all_options$last_modified) {
        self$set('Last-Modified', http_date(file.mtime(full_path)))
      }
      
      if (is.null(all_options$dot_files)) {
        if (!(all_options$dot_files %in% c('allow', 'deny', 'ignore'))) {
          stop('unknown value for option `dot_files`, must be one of "allow", "deny", or "ignore"')
        }
        
        warning('option `dot_files` is not implemented')
      }
      
      private$body <- setNames(full_path, 'file')
      self$end()
    }

    status = function(code) {
      assert_that(is.integer)

      private$status_code <- code

      invisible(self)
    }
  ),
  private = list(
    status_code = NULL,
    body = NULL,
    headers = NULL,
    req = NULL,

    add_headers = function(headers) {
      private$headers <- append(private$headers, headers)
      invisible(self)
    },
    render_body = function(path) {
      stopifnot(is.character(path), file.exists(path))

      private$body <- markdown::markdownToHTML(path)
      invisible(self)
    },
    set_body = function(expr) {
      private$body <- expr
      invisible(self)
    },
    set_content_type = function(type) {
      private$headers[['Content-Type']] <- type
      invisible(self)
    },
    set_content_length = function(size) {
      private$headers[['Content-Length']] <- size
      invisible(self)
    },
    set_status = function(n) {
      stopifnot(n %>% is.numeric)
      private$status <- n
      invisible(self)
    },


    as_HTTP_response = function() {
      cat(
        paste0('HTTP/1.1 ', private$status),
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
        status = private$status,
        headers = private$headers,
        body = private$body
      )
    }
  )
)
