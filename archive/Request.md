
Request properties:
  
req.app  # instance of the express app using the middleware 
req.baseUrl # url path for router 
req.body # key value pairs of data submitted in the request body 
req.cookies # cookies sent by teh request 
req.fresh #
red.hostname
req.ip
req.ips 
req.originalUrl
req.params # object containing properties mapped to the named route "parameters"
req.path #path part of the request URL
req.protocol # 
req.query # object containing property for each query string parameter in teh route 
req.route # string of the currently matched route 
req.secure #boolean
req.signedCookies
req.stale # opposite of req.fresh
req.subdomains 
req.xhr # boolean 


Request methods: 
  
req.accepts(types) # checks if the specified types are acceptable, based on the requests Accept HTTP header field returns list/array of best match
req.acceptsCharsets
req.acceptsEncodings
req.acceptsLanguages 
req.get(field) # returns teh specified HTTP request header field 
req.is(type) #returns true is the requests content type matches the type specidied by 