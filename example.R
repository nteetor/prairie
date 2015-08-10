#
# example file
#

source('dull_server.R')

# NOTE: the parameter structure for the "get" function
# is currently ignored within the dull_class object
dull() %>% 
  get('/', function(req, res) {
    # to demonstrate future plans
    if (missing(req)) req <- NULL
    if (missing(res)) res <- list() # will be R6 object
    
    # res %>% 
    #   status(200) %>% 
    #   body('Hello, world!') # content type auto added
    
    # but, for now,
    res$status <- 200
    res$headers <- list('Content-Type' = 'text/html')
    res$body <- 'Hello, world!'
    res
  }) %>% 
  get('/not_found', function(req, res) {
    # see note above
    if (missing(req)) req <- NULL
    if (missing(res)) res <- list()
    
    # res %>% http_404_page
    # res
    
    # for now,
    res$status <- 404
    res$headers <- list('Content-Type' = 'text/html')
    res$body <- '<h5>Whoops, page not found!<h5>'
    res
  }) %>% 
  listen('0.0.0.0', 8080)
