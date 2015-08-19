#
# example file demonstrating multiple HTTP methods for the same URI
#

source('R/dull-class.R')

arg = commandArgs(TRUE)
port = ifelse(length(arg) == 1, as.integer(arg), 3030)
cat("Listening on port", port, "\n")

dull() %>% 
  get('/', function(req, res) {
    res %>% 
      status(200) %>% 
      headers(Connection = 'close') %>% 
      body('<div align="center"><h1>Hello, world!</h1><p>(and all who inhabit it)</p></div')
    
  }) %>% 
  post('/', function(req, res) {
    # different method for the same uri
    
    res %>% 
      status(405) %>% 
      body('<h4>Sorry, this is just a test</h4><p>Washington or Huffington?</p>')
    
  }) %>% 
  get('/user/(?<id>[0-9]+)', function(req, res) {
    res %>% 
      body('<h4>User info<h4>')
  }) %>% 
  listen('0.0.0.0', port)
