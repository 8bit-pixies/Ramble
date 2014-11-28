#' returns is a function which always succeeds
#' returns || a -> Parser a
#' 
#' @export
#' @examples
#' \dontrun{
#' returns("1") ("abc")
#' }
returns <- function(string) {
  return(function(nextString) {
    return(list(result = string, leftover=nextString))
  })
}

#' item matches a single string
#' item || Parser Char
#' 
#' @export
#' @examples
#' \dontrun{
#' item() ("abc")
#' item() ("")
#' }
item <- function(...){
  return(function(string){
    return (if(string=="") list() else list(result=substr(string, 1, 1), leftover=substring(string, 2)))
  })
}

#' then is also know as the bind function.
#' then || Parser a -> (a -> Parser b) -> Parser b
#' 
#' Usage: then(p, f) would be the same as p `then` f. Alternatively, 
#' in the piping world, it would look like:
#' ```
#' (parserp(...) %>=% parserf(...)) (string)
#' ```
#' 
#' This is also known as the bind function
#' \dontrun{
#' then(item(), returns("123")) ("abc")
#' }
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

#' Bind/then function. 
#' 
#' @export
#' @examples
#' \dontrun{
#' ( item() %>>=% returns("123") ) ("abc")
#' }
`%>>=%` <- then

#' Accepts a list of funtions to perform parsing. The last element is assumed to be the return(ing) function
#' doList || Parser a -> Parsec b -> ... -> Parser n -> (a -> b -> ... -> n -> Parser n) -> Parser n
#' 
#' do : this is is the list of parsers
#'      the list will assume that the last element is the function to be applied, based on the variables in do.
#'     This function can also pickup "leftovers" via the argument "leftover_" which 
#'     has been reserved especially for `f`. sample usage: 
#'     `do(do=list(t=natural()), f=function(t,leftover_) {return(leftover_)}) ("123 123")`
#' 
#' For example, `do` can be like:
#' do(list(x=item(), item(), y=item(), function(x,y){c(x,y)})) ("abcde")
#' @export
#' @examples
#' \dontrun{
#' do(list(x=item, item, y=item, f = function(x,y) {unlist(x,y)})) ("abcdef")
#' do(list(x=item, item, y=item, f = function(x,y) {c(x,y)})) ("ab")
#' }
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
#' (+++) || Parser a -> Parser a -> Parser a
#' choice(returns("1"), returns("2")) ("abcdef")
choice <- function(parserp, parserq) {
  return(function(string){
    result <- parserp (string)
    if(!is.null(result$leftover)) {return(result)}
    return(parserq (string))    
  })
}

#' binary operand for choice
#' Usage:
#' ```
#' (item() %+++% returns("2")) ("abcdef")
#' ```
#' @export
#' @examples
#' \dontrun{
#' (item %+++% returns("2")) ("abcdef")
#' }
`%+++%` <- choice

#' sat is
#' sat || (Char -> Bool) -> Parser Char
#' @export
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

#' Digit checks for single digit
#' 
#' @export
#' @examples
#' \dontrun{
#' Digit()("123")
#' }
Digit <- function(...) {sat(function(x) {return(!!length(grep("[0-9]", x)))})}

#' Lower checks for single lower case character
#' 
#' @export
#' @examples
#' \dontrun{
#' Lower() ("abc")
#' }
Lower <- function(...) {sat(function(x) {return(!!length(grep("[a-z]", x)))})}

#' Upper checks for a single upper case character
#' 
#' @export
#' @examples
#' \dontrun{
#' Upper()("Abc")
#' }
Upper <- function(...) sat(function(x) {return(!!length(grep("[A-Z]", x)))})

#' Alpha checks for single alphabet character
#' 
#' @export
#' @examples
#' \dontrun{
#' Alpha()("abc")
#' }
Alpha <- function(...) sat(function(x) {return(!!length(grep("[A-Za-z]", x)))})

#' AlphaNum checks for a single alphanumeric character
#' 
#' @export
#' @examples
#' \dontrun{
#' AlphaNum()("123")
#' AlphaNum()("abc123")
#' }
AlphaNum <- function(...) sat(function(x) {return(!!length(grep("[A-Za-z0-9]", x)))})

#' char checks for a single character of your choice
#' 
#' @export
#' @examples
#' \dontrun{
#' Char("a")("abc")
#' }
Char <- function(c) {sat(function(x) {return(c==x)})}

#' Space checks for a single space character
#' 
#' @export
#' @examples
#' \dontrun{
#' Space()(" 123")
#' }
Space <- function(...) sat(function(x) {return(!!length(grep("\\s", x)))})

#' String tries to match a whole string
#' string || String -> Parser String
#' 
#' @export
#' @examples
#' \dontrun{
#' String("test")("test123")
#' }
String <- function(x) {
  if(x=="") {return(returns(""))}
  else {
    do(list(Char(substr(x,1,1)),
            String(substring(x,2)),
            function() {return(x)})
    )}
}

#' many matches 0 or more of pattern p
#' many || Parser a -> Parser [a]
#' 
#' @export
#' @examples
#' \dontrun{
#' many(Digit()) ("123abc")
#' many(Digit()) ("abc")
#' }
many <- function(p) {
  many1 (p) %+++% returns(list())
}

#' many1 matches 1 or more of pattern p
#' many1 || Parser a -> Parser[a]
#'
#' @export
#' @examples
#' \dontrun{
#' many1(Digit()) ("123abc")
#' }
many1 <- function(p) {
  do(list(v=p,
          vs=many(p),
          f = function(v,vs="") {unlist(c(v,vs))})
  )
}

#' ident is identify, which is lowercase followed by zero or more alphanumeric
#' ident || Parser String
#' @export
#' @examples
#' \dontrun{
#' ident() ("variable1 = 123")
#' }
ident <- function() {
  do(list(x = Lower(),
          xs = many(AlphaNum()), 
          f = function(x,xs="") {paste0(x,paste(xs, collapse=''))})
  )
}

#' nat matches natural numbers
#' nat || Parser Int
#' @export
#' @examples
#' \dontrun{
#'   nat() ("123 + 456")
#' }
nat <- function() {
  do(list(xs = many1(Digit()),
          f = function(xs) {paste(xs, collapse='')})
  )
}

#' space matches spaces
#' space || Parser ()
#' @export
#' @examples
#' \dontrun{
#' space() ("  abc")
#' }
space <- function() {
  do(list(xs = many(Space()),
          f = function(x) {return(list())})
  )
}

#' token strips spaces as needed
#' token || Parser a -> Parser a
#' @export
#' @examples
#' \dontrun{
#' token(ident()) ("   variable1   ")
#' }
token <- function(p) {
  do(list(space(),
          v = p,
          space(), 
          f = function(v) {v})
  )
}

#' identifier creates an identifier
#' 
#' @export
identifier <- function(...) {token(ident())}

#' @export
natural <- function(...) {token(nat())}

#' Symbol creates a token for a symbol
#' @export
#' @examples
#' \dontrun{
#' symbol("[") ("  [123]")
#' }
symbol <- function(xs) {token(String(xs))}
