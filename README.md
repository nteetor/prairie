<h1 align="center">
	<img width="400" src="./inst/dull_logo.png" alt="">
</h1>

# dull
A light-weight web framework for R that makes building web applications a little too easy.

```R
dull() %>%
  get('^$', function(req, res) {
    res %>%
      body("I'm not a no-body") %>%
      send
  }) %>%
  put('^create/file/(?<path>\\w+)$', function(req, res) {
    file_path <- params(req)[1]
    
    if (file.exists(file_path)) {
      res %>%
        status(404) %>%
        send("Sorry, file already exists")
    } else {
      file.create(file_path)
      res %>%
        send("File created")
    }
  }) %>%
  listen('127.0.0.1', 3030)
```

A dull application is as simple or complex as you need it to be.

The server functionality and setup are primarily based on [expressjs](http://expressjs.com/). The syntactical styling is influenced by [wookie](http://wookie.lyonbros.com/). Additionally, the web frameworks [Sinatra](http://sinatrarb.com/), [Tornado](http://www.tornadoweb.org/en/stable/), [django](https://www.djangoproject.com/) and [Shiny](http://shiny.rstudio.com/) have also influenced the design of this project. They deserve this mention and more.

Hobey-ho, let's go!
