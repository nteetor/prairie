# prairie
A framework to grow your existing R code into web applications.

[![Travis-CI Build Status](https://travis-ci.org/nteetor/prairie.svg?branch=master)](https://travis-ci.org/nteetor/prairie) [![codecov](https://img.shields.io/codecov/c/github/nteetor/prairie.svg)](https://codecov.io/github/nteetor/prairie)

Below is a simple prairie application,

```R
app(
  route(
    'get',
    '^$',
    function(req) {
      res <- response()
      
      status(res) <- 200
      
      res[['Content-Type']] <- 'text/html'
      body(res) <- '<h1>Welcome to prairie</h1>'
      
      res
    }
  ),
  # Make sure the list has values with names
  # method, path, and handler
  list(
    method = c('get', 'post'),
    path = '^data$',
    handler = function(req) {
      if (method(req) == 'get') {
        as.response(iris) # because who doesn't want iris data?
      } else {
        print(body(req)) # log to console
        
        res <- response()
        body(res) <- 'Thanks for all the data!'
        
        res
      }
    }
  )
)
```

Work on prairie, when the project was still titled dull, began prior to the release of Shiny version 0.13.0. Prior to version 0.13.0, modularization in Shiny was cumbursome, probably ill-advised, or impossible. Rook, another framework for R, was out of date and, to my knowledge, not well-maintained, if at all. I set out to create an Express-like web framework and during the first couple months of development another package, jug, appeared. Jug takes an Express-like approach to web application contruction using R6 classes. I recommend looking at both [Rook](https://github.com/jeffreyhorner/Rook) and [jug](https://github.com/Bart6114/jug). Rook had a significant impact on the development of the powerful httpuv package. Specifically the format of the request environment available to the `call` function in an httpuv application. If you like dectorators in Python and have any interest in APIs check out the [plumber](https://github.com/trestletech/plumber) package. Jumping back to prairie, mid November of 2015 I decided to move away from Express, changed the project name, and began work on prairie. Prairie is a style of web application construction I believe to be very R-like. Prairie draws inspiration from sinatra and wookie and appears more functional in use. The request and response object construction is reminiscent of django and express (I didn't completely let go). The master version of prairie, going forward, will always be stable. Prairie is not on CRAN yet, so look into `devtools::install_github` if you aren't already familiar with the function and would like to try praire out.

As the tagline says, prairie is about growing existing code. Even though httpuv uses the same libuv C library used by Node, R is inherently single-threaded and prairie does not have the same aynschronous capabilities of Node or express. Thus, prairie focuses more on utility. Prairie focuses on coercing your existing R classes into the application, route, and response classes central to the framework. The goal is to avoid requiring rewrites of their code base. By creating
your own `as.route` methods one can serve up methodologies, data, and anything else R across the web. I love working with R and I know personally how skeptical programmers are of R as anything more than a statiscal workhorse. With prairie I hope you, an R programmer and enthusiast, can push the boundaries of "acceptable" use cases for R.

Before I put away my soapbox I have a couple favors to ask,

1) Help me make prairie better. Let me know what functionality could be added, what isn't programmer friendly, what documentation could be better, and in general your experience using prairie. 

2) Help me keep prairie working. A great man once proved any program can be reduced to a single bug, so I bet there is at least one bug in prairie. You can, and I would greatly appreciate, if you create an [issue](https://github.com/nteetor/prairie/issues) for any bugs you discover.

With that I offer you prairie and hope you will give the package the package a try. We R all capable of making R <strike>harder</strike>, better, faster, and stronger, so let's do it.

Nate
