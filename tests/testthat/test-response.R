library(dull)
context('response object')

end_response_signal <- '.*end called from response\\n'

test_that('$get and $set correctly access values', {
  res <- response$new(NULL)

  res$set('Content-Type', 'text/html')
  expect_equal(res$get('Content-Type'), 'text/html')
  expect_equal(res$get('content-type'), 'text/html')
  expect_equal(res$get('content-Type'), 'text/html')

  res$set('Warning', 'challenger approaching')
  expect_equal(res$get('Warning'), 'challenger approaching')

  expect_null(res$get('missing'))
})

test_that('$initialize sets defaults', {
  res <- response$new(NULL)

  expect_equal(res$get('content-type'), 'text/plain')
})

test_that('$append sets new value', {
  res <- response$new(NULL)

  res$append('Connection', 'close')
  res$append('Content-Length', 'over 9000')

  expect_equal(res$get('Connection'), 'close')
  expect_equal(res$get('Content-Length'), 'over 9000')

  res$append('Content-Length', '?! That\'s impossible!')
  expect_equal(res$get('Content-Length'), 'over 9000?! That\'s impossible!')
})

test_that('$attachment uses correct type and file name', {
  res <- response$new(NULL)

  res$attachment()
  expect_equal(res$get('content-disposition'), 'attachment')
  expect_equal(res$get('content-type'), 'text/plain')
  expect_null(res$get('warning'))

  res$attachment('attachment.html')
  expect_equal(res$get('content-disposition'), 'attachment; filename="attachment.html"')
  expect_equal(res$get('content-type'), 'text/html')
})

test_that('$download sets correct disposition and file name when specified', {
  skip('testing $download depends on implementing a `resolve` method')
  res <- response$new(NULL)

  expect_error(res$download('/path/to/distant/lands.txt'))

  # TODO: research "resolve" function in node "path" module, once added
  # $download can be better tested, see
  # https://github.com/strongloop/express/blob/master/lib/response.js#L523
})

test_that('$end raises end signal, sets body if specified', {
  res <- response$new(NULL)

  expect_error(res$end(), end_response_signal)

  expect_error(res$end('and soul'))
  expect_equal(res$as_Rook_response()$body, 'and soul')
})

test_that('$format handles both explicit and general content types', {
  skip('implement once request object is overhauled')
  res <- response$new(NULL)

  # testing of $format will have to wait until the request object is overhauled
})

test_that('$json accepts correct types, ends response', {
  skip_if_not_installed('jsonlite')

  res <- response$new(NULL)

  df <- data.frame(a = 1:5, b = 5:9, c = 10:14)
  json_df <- jsonlite::toJSON(df)

  lst <- list(foo = list(bar = 'boo', baz = 'hoo'), howdy = 'doody')
  json_lst <- jsonlite::toJSON(lst)

  chr <- 'ol_salty'
  json_chr <- jsonlite::toJSON(chr)

  expect_error(res$json(), end_response_signal)
  expect_equal(res$get('content-type'), 'application/json')
  expect_equal(res$as_Rook_response()$body, '')

  expect_error(res$json(df), end_response_signal)
  expect_equal(res$get('Content-Type'), 'application/json')
  expect_equal(res$as_Rook_response()$body, json_df)

  expect_error(res$json(lst), end_response_signal)
  expect_equal(res$get('Content-Type'), 'application/json')
  expect_equal(res$as_Rook_response()$body, json_lst)

  expect_error(res$json(chr), end_response_signal)
  expect_equal(res$get('content-type'), 'application/json')
  expect_equal(res$as_Rook_response()$body, json_chr)
})

test_that('$links sets and updates Link header', {
  res <- response$new(NULL)

  res$links(list(Princess = 'Zelda'))
  expect_equal(res$get('Link'), '<Zelda>; rel=Princess')

  res$links(list(Hero = 'Link'))
  expect_equal(res$get('Link'), '<Zelda>; rel=Princess, <Link>; rel=Hero')
})

test_that('$location sets Location header, handles special value "back"', {
  skip('need to implement new request object')
})

test_that('$redirect sets Location, sets Status, raises end signal', {
  skip('requires that res$location is implemented')
})

test_that('$render', {
  skip('res$render is not implemented')
})

test_that('$send sets content-type and body, signals end of response', {
  skip_if_not_installed('jsonlite')

  res <- response$new(NULL)

  expect_error(res$send(), end_response_signal)

  html <- '<h1>Graphing Bad Puns</h1><p>Para-plot</p>'
  expect_error(res$send(html), end_response_signal)
  expect_equal(res$get('content-type'), 'text/html')
  expect_equal(res$as_Rook_response()$body, html)

  lsting <- list(tip = 'the boat')
  expect_error(res$send(lsting), end_response_signal)
  expect_equal(res$get('content-type'), 'application/json')
  expect_equal(res$as_Rook_response()$body, jsonlite::toJSON(lsting))

  df <- data.frame(d = 'me', and = 'revenge I swore')
  expect_error(res$send(df), end_response_signal)
  expect_equal(res$get('content-type'), 'application/json')
  expect_equal(res$as_Rook_response()$body, jsonlite::toJSON(df))
})

test_that('$send_file stops on incorrect options', {
  res <- response$new(NULL)

  expect_error(res$send_file('attachment.html', options = list(opt = 'ical'), 'illusion'),
               'all options must be named')
  expect_error(res$send_file('attachment.html', options = list('ing')),
               'all options must be named')

  expect_error(res$send_file('attachment.html', mag = 'ical'),
               'unknown options passed to \\$send_file')
  expect_error(res$send_file('attachment.html', max_age = 20, bad = 'oops'),
               'unknown options passed to \\$send_file')

  expect_error(res$send_file('../../README.md'),
               'option `root` must be specified or `path` must be absolute')

  expect_error(res$send_file('attachment.html', root = '.', headers = list('shoulders', 'knees')),
               'values of option `headers` must be named')
  expect_error(
    res$send_file('attachment.html',
                  options = list(root = '.', headers = list(Warning = 'small file', '#comment'))
    ),
    'values of option `headers` must be named'
  )

})

