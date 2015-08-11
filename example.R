#
# example file
#

source('dull_server.R')
source('response.R')

# NOTE: the parameter structure for the "get" function
# is currently ignored within the dull_class object
dull() %>% 
  get('/', function(req, res) {lazyeval::lazy(
    res %>% 
      status(200) %>% 
      body('Hello, world!') # content type auto added
    
    # NOTE: It is not necessary to return the response object
  )}) %>% 
  get('/not_found', function(req, res) {lazyeval::lazy(
    # res %>% http_404_page
    
    # for now,
    res %>% 
      status(404) %>% 
      body('<h4>Whoops, page not found!</h4><p>Better luck next time</p>')
  )}) %>% 
  listen('0.0.0.0', 8080)
