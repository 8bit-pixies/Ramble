#' parser combinator written in R
#' requires R.utils

#' parsed :: String -> String -> ParsedResult
parsed <- function(r,l) {
  return(list(result = r, leftover=l))
}

#' parse :: Parser a -> String -> [(a, String)]
parses <- function(parser, ...) {
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
#' 
#' This is also known as the bind function
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

#' Usage:
#' ```
#' ( item() %>>=% item() %>>=% item() %>>=% failure() ) ("abcdefghi")
#' ```
`%>>=%` <- then

#' doList :: Parser a -> Parsec b -> ... -> Parser n -> (a -> b -> ... -> n -> Parser n) -> Parser n
#' Accepts a list of funtions to perform parsing.
#' 
#' do : this is is the list of parsers
#'      the list will assume that the last element is the function to be applied, based on the variables in do.
#'     This function can also pickup "leftovers" via the argument "leftover_" which 
#'     has been reserved especially for `f`. sample usage: 
#'     `do(do=list(t=natural()), f=function(t,leftover_) {return(leftover_)}) ("123 123")`
#' 
#' For example, `do` can be like:
#' do=list(x=item(), item(), y=item())
do <- function(do) {
  return(function(string){
    doResult <- list()
    result <- list(leftover = string)
    for(element in 1:(length(do)-1)) { # What is the apply approach? it will probably be invisible(apply)
      if (length(result) == 0){
        return (list()) # no more tokens
      }
      result_ <-  do[[element]] (result$leftover) # apply the current function
      if(is.null(result_$leftover)) {return(list())} # only succeeds if every parser in the sequence succeeds
      tryCatch({doResult[[names(do[element])]] <- result_$result},
               error=function(x) {return(NA)},
               warning=function(x) {return(NULL)}) # try catch to ensure that the token is assigned (if it is meant to be assigned), otherwise it is discarded
      result$leftover <- result_$leftover
    }
    
    doResult$leftover_ <- result$leftover
    
    # can fail in the final call, we need to check the function call
    fcall <- R.utils::doCall(tail(do,1)[[1]], args=doResult,.ignoreUnusedArgs=TRUE)
    if (is.null(fcall)) {
      return(list())
    }
    else if ("leftover" %in% names(fcall)) {
      # if fcall returns a list with the element leftover, we need to take that one
      return(list(result = fcall$result, leftover=fcall$leftover))
    }
    else {
      return(list(result = fcall, leftover=result$leftover))
    }
  }
)}

#' choice (or else)
#' (+++) :: Parser a -> Parser a -> Parser a
choice <- function(parserp, parserq) {
  return(function(string){
    result <- parserp (string)
    if(!is.null(result$leftover)) {return(result)}
    return(parserq (string))    
  })
}

#' Usage:
#' ```
#' (item() %+++% returns("2")) ("abcdef")
#' ```
`%+++%` <- choice

#' sat :: (Char -> Bool) -> Parser Char
sat <- function(p) {
  do(list(x=item(), function(x) {
    if(p (x)) {
      return(x)
    }
    else {
      return(c())
    }
  })
)}

## define the generic functions
Digit <- function(...) sat(function(x) {return(!!length(grep("[0-9]", x)))})
Lower <- function(...) sat(function(x) {return(!!length(grep("[a-z]", x)))})
Upper <- function(...) sat(function(x) {return(!!length(grep("[A-Z]", x)))})
Alpha <- function(...) sat(function(x) {return(!!length(grep("[A-Za-z]", x)))})
AlphaNum <- function(...) sat(function(x) {return(!!length(grep("[A-Za-z0-9]", x)))})
Char <- function(c) {sat(function(x) {return(c==x)})}
Space <- function(...) sat(function(x) {return(!!length(grep("\\s", x)))})

#' string :: String -> Parser String
String <- function(x) {
  if(x=="") {return(returns(""))}
  else {
    do(list(Char(substr(x,1,1)),
               String(substring(x,2)),
            function() {return(x)})
  )}
}

#' many :: Parser a -> Parser [a]
#' many matches 0 or more of pattern p
many <- function(p) {
  many1 (p) %+++% returns(list())
}

#' many1 :: Parser a -> Parser[a]
#' many1 matches 1 or more of pattern p
many1 <- function(p) {
  do(list(v=p,
             vs=many(p),
     f = function(v,vs="") {unlist(c(v,vs))})
  )
}

#' ident :: Parser String
#' ident is identify, which is lowercase followed by zero or more alphanumeric
ident <- function() {
  do(list(x = Lower(),
               xs = many(AlphaNum()), 
     f = function(x,xs="") {paste0(x,paste(xs, collapse=''))})
  )
}

#' nat :: Parser Int
nat <- function() {
  do(list(xs = many1(Digit()),
     f = function(xs) {paste(xs, collapse='')})
  )
}

#' space :: Parser ()
space <- function() {
  do(list(xs = many(Space()),
     f = function(x) {return(list())})
  )
}

#' token :: Parser a -> Parser a
token <- function(p) {
  do(list(space(),
               v = p,
               space(), 
     f = function(v) {v})
  )
}

#' identifier :: Parser String
identifier <- function() {token(ident())}
natural <- function() {token(nat())}
symbol <- function(xs) {token(String(xs))}



