#' parser combinator written in R
#' requires R.utils

#' parsed :: String -> String -> ParsedResult
parsed <- function(r,l) {
  return(list(result = r, leftover=l))
}

#' parse :: Parser a -> String -> [(a, String)]
parse <- function(parser, ...) {
  return(parser(...))
}

#' returns :: a -> Parser a
returns <- function(string) {
  return(function(nextString) {
    return(parsed(string, nextString))
  })
}

#' failure :: a -> []
failure <- function(...) {
  return(function(string) {return(list())})
}

#' item :: Parser Char
item <- function(...){
  return(function(string){
    return (if(string=="") list() else parsed(substr(string, 1, 1), substring(string, 2)))
  })
}

#' then :: Parser a -> (a -> Parser b) -> Parser b
#' 
#' Usage: then(p, f) would be the same as p `then` f. Alternatively, 
#' in the piping world, it would look like:
#' ```
#' (parserp(...) %>=% parserf(...)) (string)
#' ```
then <- function(parserp, parserf) {
  return(function(string) {
    result <- parserp (string)
    if (length(result) == 0) {
      return (list())
    }
    else {
      result_ <- parserf (result$leftover)
      return(list(result=c(result$result, result_$result), leftover=result_$leftover))
    }
  })
}

`%>>=%` <- then

#' doList :: Parser a -> Parsec b -> ... -> Parser n -> (a -> b -> ... -> n -> Parser n) -> Parser n
#' Accepts a list of funtions to perform parsing.
#' 
#' do : this is is the list of parsers
#' f : this is the function to be applied, based on the variables in do
#' 
#' For example, `do` can be like:
#' do=list(x=item(), item(), y=item())
do <- function(do, f) {
  return(function(string){
    doResult <- list()
    result <- list(leftover = string)
    for(element in 1:length(do)) { # has to be a better way
      if (length(result) == 0){
        return (list()) # no more tokens
      }
      result_ <-  do[[element]] (result$leftover) # apply the current function
      if(is.null(result_$leftover)) {return(list())} # only succeeds if every parser in the sequence succeeds
      tryCatch({doResult[[names(do[element])]] <- result_$result}) # if the token is to be assigned...
      result$leftover <- result_$leftover
    }
    
    # can fail in the final call, we need to check the function call
    fcall <- R.utils::doCall(f, args=doResult)
    if (is.null(fcall)) {return(list())}
    else {return(list(result = fcall, leftover=result$leftover))}
  })
}

#' choice (or else)
#' (+++) :: Parser a -> Parser a -> Parser a
choice <- function(parserp, parserq) {
  return(function(string){
    result <- parserp (string)
    if(!is.null(result$leftover)) {return(result)}
    return(parserq (string))    
  })
}

`%+++%` <- choice

#' sat :: (Char -> Bool) -> Parser Char
sat <- function(p) {
  do(do=list(x=item()), f = function(x) {
    if(p (x)) {
      return(x)
    }
    else {
      return(c())
    }
  })
}

## define the generic functions
Digit <- function(...) sat(function(x) {return(!!length(grep("[0-9]", x)))})
Lower <- function(...) sat(function(x) {return(!!length(grep("[a-z]", x)))})
Upper <- function(...) sat(function(x) {return(!!length(grep("[A-Z]", x)))})
Alpha <- function(...) sat(function(x) {return(!!length(grep("[A-Za-z]", x)))})
AlphaNum <- function(...) sat(function(x) {return(!length(grep("[A-Za-z0-9]", x)))})
Char <- function(c) {sat(function(x) {return(c==x)})}

#' string :: String -> Parser String
String <- function(x) {
  if(x=="") {return(returns(""))}
  else {
    do(do=list(Char(substr(x,1,1)),
               String(substring(x,2))),
       f = function() {return(x)})
  }
}

String("a") ("asdfghjk")



