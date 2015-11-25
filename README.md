# prairie
A framework to grow web applications in R.

[![Travis-CI Build Status](https://travis-ci.org/nteetor/prairie.svg?branch=master)](https://travis-ci.org/nteetor/prairie) [![codecov](https://img.shields.io/codecov/c/github/nteetor/prairie.svg)](https://codecov.io/github/nteetor/prairie)

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
      res <- as.response(iris)
      
      # change status, headers, etc.
      
      res
    }
  )
)
```
