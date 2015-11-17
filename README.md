# prairie
A framework to grow web applications in R.

[![Travis-CI Build Status](https://travis-ci.org/nteetor/dull.svg?branch=master)](https://travis-ci.org/nteetor/dull) [![codecov](https://img.shields.io/codecov/c/github/nteetor/dull.svg)](https://codecov.io/github/nteetor/dull)

```R
app(
  route(
    'get',
    '^$',
    function() {
      res <- response()
      
      status(res) <- 200
      
      res[['Content-Type']] <- 'text/html'
      body(res) <- '<h1>Welcome to prairie</h1>'
      
      res
    }
  ),
  list(
    method = c('get', 'post'),
    path = '^data$',
    handler = function() {
      res <- response()
      
      status(res) <- 200
      
      res[['Content-Type']] <- 'application/json'
      body(res) <- iris
      
      res
    }
  )
)
```
