#' Simple route class
#'
#' This class is a glorified container for a url and the corresponding callback
#' function. Some handy utility functions are included as private methods. As
#' dull middleware begins to take shape this class will evolve more.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{uri_matches(path)}: TRUE if the route
#'   uri, as a regular expression, matches path
#'  \item \code{assign_callback(method, callback)}: set the callback function for a
#'   specific HTTP method
#'  \item \code{callback_for(method)}: return the callback
#'   function for a specific HTTP method
#' }
#'
#' @docType class
#' @keywords internal
#' @format An R6 class object.
#' @importFrom stringr str_match_all
#' @importFrom magrittr %>%
#' @export
#' @name route-class
route <- R6::R6Class(
  # This naming convention will be applied to response.R once this branch is merged into master
  'route',
  public = list(
    uri = NULL,
    params = NULL,
    callbacks = NULL,

    initialize = function(method, uri, callback) {
      stopifnot(
        is.character(method),
        is.character(uri),
        is.function(callback),
        length(formals(callback)) == 2
      )

      self$callbacks <- list()
      self$callbacks[[method]] <- callback

      # I typically would not modify the user's specification, however
      # the fact that ^$ does not match the empty string is bonkers
      self$uri <- if (uri == '^$') '^\\B$' else paste(uri, collapse = '|')

      self$params <- self$uri_parameters()

      invisible(self)
    },

    uri_matches = function(path) {
      stopifnot(path %>% is.character)

      if (substr(path, 1, 1) == '/') path <- substr(path, 2, nchar(path))

      grepl(path, self$uri, perl = TRUE)
    },
    assign_callback = function(method, callback) {
      self$callbacks[[method]] <- callback

      invisible(self)
    },
    get_callback = function(method) {
      self$callbacks[[method]]
    },
    uri_parameters = function() {
      if (grepl('<>', self$uri)) {
        stop('route URI contains empty capture group name(s)')
      }

      split_uri <- Filter(function(w) w != '', strsplit(self$uri, '/')[[1]])
      grp_name_pattern <- '<\\w+>'
      name_match <- regexpr(grp_name_pattern, split_uri, perl = TRUE)
      group_names <- gsub('<|>', '', regmatches(split_uri, name_match))

      if (length(group_names) == 0) return(NULL)

      group_names
    }
  )
)
