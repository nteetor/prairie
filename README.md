# dull
A light-weight web server package for R

### Implementation notes

#### dull_server
The server functionality and setup is primarily based on [expressjs](http://expressjs.com/). The syntactical styling is influenced by [wookie](http://wookie.lyonbros.com/)

#### dull_response
* status(res)
    + based on [expressjs](http://expressjs.com/)
  
* body(res)
    + based on [expressjs](http://expressjs.com/)
  
* headers(res)
    + based on [wookie](http://wookie.lyonbros.com/)
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

#### General notes
This project, as stated above, is heavily influenced by expressjs and wookie. Additionally, the web frameworks Sinatra, Tornado, and Shiny have also influenced the design of this project and deserve this mention and more.

Hobey-ho, let's go!
  
