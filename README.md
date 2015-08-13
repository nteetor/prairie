# dull
A light-weight web server package for R

### Implementation notes

#### dull_server
The server functionality and setup is primarily based on [expressjs](http://expressjs.com/). The syntactical styling is influenced by [wookie](http://wookie.lyonbros.com/)

#### dull_response
* status(res)
    + based on expressjs
  
* body(res)
    + based on expressjs
  
* headers(res)
    + based on expressjs
    + passes named arguments to header of the response object, lists of values are handle like wookie,
    ```{r}
    # the following are equivalent
    res %>% 
        headers(field = list('value1', 'value2'))
    res %>%
        headers(field = 'value1', field = 'value2')
    ```
    
#### dull_request
TBD

#### route
* allowing regular expressions in URIs seems pretty standard among the popular web frameworks and has been included in dull

#### General notes
This project, as stated above, is heavily influenced by expressjs and wookie. Additionally, the web frameworks [Sinatra](http://sinatrarb.com/), [Tornado](http://www.tornadoweb.org/en/stable/), and [Shiny](http://shiny.rstudio.com/) have also influenced the design of this project. They deserve this mention and more.

Hobey-ho, let's go!
  
