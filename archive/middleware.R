middleware <- list(
  body_parser = function(req) {
    # pass
  },
  header_parser = function(req) {
    request_line <- req[1] %>% 
      str_split_fixed('\\s+', 3)
    
    method_token <- request_line[1]
    request_uri <- request_line[2]
    protocol_version <- request_line[3]
    
    header_end <- which(req == '')[1] - 1
    if (header_end %>% is.na) header_end <- length(req)
    
    headers <- req[2:header_end]
    
    parsed_headers <- vector('list', headers %>% length)
    parsed_values <- vector('list', headers %>% length)
    
    for (i in 1:length(headers)) {
      line <- headers[i] %>% 
        str_split(':', 2) %>% 
        .[[1]] %>% 
        str_trim('both')
      
      parsed_headers[[i]] <- line[1]
      parsed_values[[i]] <- line[2]
    }
    
    names(parsed_values) <- unlist(parsed_headers)
    
    parsed_values %<>%
      append(list(
        'method_token' = method_token,
        'request_uri' = request_uri,
        'protocol_version' = protocol_version
      ), 0)    
    
    parsed_values
  }
)