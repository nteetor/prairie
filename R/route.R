#' Create routes
#' 
#' Within prairie, a route is defined as \emph{a mapping between any number of 
#' \link[=method]{methods} and a single \link{path}.} A route may never have 
#' more than one path, however a path may be defined as a regular expression. In
#' this case a single route would match multiple URIs.
#' 
#' @param method A character vector specifying an HTTP method(s), such as 
#'   \code{"get"}, \code{"post"}, or \code{"put"}
#' @param path A character vector specifying which URIs the route will handle
#' @param handler A function which returns a \link{repsonse} object, see below
#'   for more information
#'   
#' @details
#' 
#' \strong{\code{method}}
#' 
#' \code{method} is a character vector with at least one element. Multiple 
#' methods may be specified or if \code{"all"} is specified then all HTTP 
#' methods are accepted. Custom methods are possible, but not advised.
#' 
#' \strong{\code{path}}
#' 
#' \code{path} may be a regular expression. \code{path} never needs to include a
#' beginning \code{/}. The root of a web application is specified by \code{^$}. 
#' If \code{path} is a regular expression capture groups may be used to extract 
#' substrings.
#' 
#' For example, if \code{path} is 
#' \tabular{c}{\code{"^wizard/(?<alias>[a-z]+)/([a-z]+)$"}} and the application 
#' receives a request for \tabular{c}{\code{/wizard/sparrowhawk/ged}} then 
#' \code{handler} will have access to the character vector \code{c("alias" = 
#' "sparrowhawk", "" = "ged")} as \code{args} during evaluation. See next 
#' section for details about \code{args} and \code{handler}.
#' 
#' \strong{\code{handler}}
#' 
#' \code{handler} must be a function of at least two arguments, \code{req} and 
#' \code{res}. An optional third argument, \code{args}, may be included. If 
#' \code{path} is a regular expression with capture groups the captured values 
#' are passed as \code{args} to \code{handler}. \code{args} will always be a 
#' character vector. Values in \code{args} are ordered according to the order of
#' the capture groups in \code{path} from left to right. If \code{path} does not
#' contain a capture group and \code{handler} includes \code{args} as a 
#' parameter, then the value of \code{args} will be \code{character(0)}.
#' 
#' @return
#' 
#' A route object.
#' 
#' @export
#' @name route
#' @examples
#' ## typically, route() is called inside of 
#' ## app(), but creation of standalone route 
#' ## objects is possible
#' 
#' ## accepts only GET requests
#' ## path has capture groups, include args
#' route(
#'   'get',
#'   '^transformers/(?<series>[a-z_]+)$',
#'   function(req, res, args) {
#'     if (args['series'] == 'beast_wars') {
#'       res$body('Right on!')$send()
#'     } else {
#'       res$body('I can dig that.')$send()
#'     }
#'   }
#' )
#' 
#' ## accepts both GET and POST requests
#' ## no capture groups, no need for args
#' route(
#'   c('get', 'post'),
#'   '^blog/comments$',
#'   function(req, res) {
#'     if (req$method == 'get') {
#'       res$body('The latest comments...')$send()
#'     } else if (req$method == 'post') {
#'       res$body('Uploading comment')$send()
#'     }
#'   }
#' )
route <- function(method, path, handler) {
  route$new(method, path, handler)
}
