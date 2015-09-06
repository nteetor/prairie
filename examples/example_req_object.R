#
# example file demonstrating extracting data from the request object
# make sure the package meta files are up-to-date before loading
#

library(dull)
library(magrittr)

arg = commandArgs(TRUE)
port = ifelse(length(arg) == 1, as.integer(arg), 3030)
cat("Listening on port", port, "\n")

dull() %>%
  get('^$', function(req, res) {
    res %>%
      status(200) %>%
      headers(Connection = 'close') %>%
      body('<div align="center"><h1>Hello, world!</h1><p>(and all who inhabit it)</p></div') %>%
      send

  }) %>%
  get('^echo/(?<phrase>\\w+)$', function(req, res) {
    res %>%
      status(200) %>%
      body(paste0('<h3>', params(req)['phrase'], '</h3><p>Same to you pal!</p>')) %>%
      send

  }) %>%
  get('^greet$', function(req, res) {
    res %>%
      body(paste0('<p>Hello, ', ip(req), ' or should I say ', host_name(req), '!<p>')) %>%
      send

  }) %>%
  listen('0.0.0.0', port)
