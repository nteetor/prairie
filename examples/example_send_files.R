#
# example file demonstrating sending a file through the response object
#

library(dull)
library(magrittr)

arg = commandArgs(TRUE)
port = ifelse(length(arg) == 1, as.integer(arg), 3030)
cat("Listening on port", port, "\n")

old_wd <- getwd()
setwd('examples/')

dull() %>% 
  get('^pull/foo_data$', function(req, res) {
    res %>% 
      send_file('./public/foo_data.csv')
  }) %>% 
  get('^$', function(req, res) {
    res %>% 
      send_file('./public/index.html')
  }) %>% 
  get('^about/$', function(req, res) {
    res %>% 
      render('./public/about.md')
  }) %>% 
  listen('127.0.0.1', port)

setwd(old_wd)
