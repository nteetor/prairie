#
# example file demonstrating multiple HTTP methods for the same URI
#

source('dull_server.R')

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
  listen('0.0.0.0', 3000)
