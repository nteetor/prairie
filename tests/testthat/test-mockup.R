context('route mockups')

test_that('print.mockup calls underlying print.route', {
  simple_route <- route('get', '/qwerty', function(req) response())
  simple_route_m <- mockup(simple_route)
  
  route_output <- capture.output(print(simple_route))
  mockup_output <- capture.output(print(simple_route_m))
  
  expect_equal(route_output, mockup_output)
})

test_that('mockup throws error if route does not return response', {
  bad_route <- route('post', '^/on/sunday$', function(req) 'whoops!')
  bad_mockup <- mockup(bad_route)
  
  expect_warning(bad_mockup('post', '/on/sunday'), 'handler returned object of class character')
})