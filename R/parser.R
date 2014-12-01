#' @import R.utils
NULL

#' \code{succeed} is based on the empty string symbol in the BNF notation The 
#' \code{succeed} parser always succeeds, without actually consuming any input 
#' string. Since the outcome of succeed does not depend on its input, its result
#' value must be pre-detemined, so it is included as an extra parameter.
#' 
#' @param string the result value of succeed parser
#' @export
#' @examples 
#' succeed("1") ("abc")
succeed <- function(string) {
  return(function(nextString) {
    return(list(result = string, leftover=nextString))
  })
}

#' \code{item} is a parser that consumes the first character of the string and
#' returns the rest. If it cannot consume a single character from the string, it
#' will emit the empty list, indicating the parser has failed.
#' 
#' @export
#' @examples
#' item() ("abc")
#' item() ("")
item <- function(...){
  return(function(string){
    if(length(string)==0){return(NULL)}
    return (if(string=="") list() else list(result=substr(string, 1, 1), leftover=substring(string, 2)))
  })
}

#' \code{satisfy} is a function which allows us to make parsers that recognise single symbols. 
#' 
#' @param p is the predicate to determine if the arbitrary symbol is a member.
#' @export
satisfy <- function(p) {
  return(function(string) {
    if (length(string)==0) {
      return(list())
    }
    else if (string==""){
      return(list())
    }
    else {
      result_ = list(result=substr(string, 1, 1), leftover=substring(string, 2))
      if (p(result_$result)) {
        return(succeed(result_$result)(result_$leftover))
      }
      else{
        return(list())
      }
    }    
  })
}

#' \code{literal} is a parser for single symbols. It will attempt to match the
#' single symbol with the first character in the string.
#' 
#' @param char is the character to be matched
#' @export
#' @examples
#' literal("a") ("abc")
literal <- function(char) {
  satisfy(function(x){return(x==char)})
}

## Building Combinators ##

#' \code{then} combinator corresponds to sequencing in BNF. The parser 
#' \code{(then(p1, p2))} recognises anything that \code{p1} and \code{p2} would 
#' if placed in succession. In this manner, two results are produced for each
#' successful parse, one from each parser. They are combined (by pairing) to
#' form a single result for the compound parser.
then <- function(p1, p2) {
  return(function(string) {
    result <- p1 (string)
    if (length(result) == 0) {
      return (list())
    }
    else {
      result_ <- p2 (result$leftover)
      if (length(result_$leftover) == 0 || is.null(result_$leftover)) {return(list())}
      return(list(result=append(list(result$result), result_$result), leftover=result_$leftover))
    }
  })
}

#' \code{\%then\%} is the infix operator for the then combinator.
#' 
#' @export
#' @examples
#' (item() %then% succeed("123")) ("abc")
`%then%` <- then

#' \code{alt} combinator is similar to alternation in BNF. the parser 
#' \code{(then(p1, p2))} recognises anything that \code{p1} or \code{p2} would. 
#' The approach taken in this parser follows (Fairbairn86), in which either is 
#' interpretted in a sequential (or exclusive) manner, returning the result of
#' the first parser to succeed, and failure if neither does.
alt <- function(p1, p2) {
  return(function(string){
    result <- p1 (string)
    if(!is.null(result$leftover)) {return(result)}
    else{
      return(p2 (string))
    }
  })
}

#' \code{\%+++\%} is the infix notation for the \code{alt} function. 
#' 
#' @export
#' @examples
#' (item() %+++% succeed("2")) ("abcdef")
`%alt%` <- alt

#' \code{using} combinator allows us to manipulate results from a parser, for 
#' example building a parse tree. The parser \code{(p \%using\% f)} has the same 
#' behaviour as the parser \code{p}, except that the function \code{f} is
#' applied to each of its result values.
#' 
#' @param p is the parser to be applied
#' @param f is the function to be applied to each result of \code{p}.
using <- function(p, f) {
  return(function(string) {
    result <- p (string) 
    if(length(result) == 0) {return(list())}
    return(list(result=f(result$result),
                leftover=result$leftover))
  })
}

#' \code{\%using\%} is the infix operator for using
#' 
#' @export
#' @examples
#' (item() %using% as.numeric) ("1abc")
`%using%` <- using

#' \code{many} matches 0 or more of pattern \code{p}. In BNF notation, 
#' repetition occurs often enough to merit its own abbreviation. When zero or 
#' more repetitions of a phrase \code{p} are admissible, we simply write 
#' \code{p*}. The \code{many} combinator corresponds directly to this operator, 
#' and is defined in much the same way.
#' 
#' This implementation of \code{many} differs from (Hutton92) due to the nature 
#' of R's data structures. Since R does not support the concept of a list of
#' tuples, we must revert to using a list rather than a vector, since all values
#' in an R vector must be the same datatype.
#' 
#' @param p is the parser to matched 0 or more times.
#' @export
#' @examples
#' Digit <- function(...) {satisfy(function(x) {return(!!length(grep("[0-9]", x)))})}
#' many(Digit()) ("123abc")
#' many(Digit()) ("abc")
many <- function(p) {
  return(function(string) {
    ((p %then% many(p)) %alt% succeed(NULL)) (string)
  })
}

#' \code{some} matches 1 or more of pattern \code{p}. in BNF notation, repetition occurs often enough to merit its own abbreviation. When zero or 
#' more repetitions of a phrase \code{p} are admissible, we simply write 
#' \code{p+}. The \code{some} combinator corresponds directly to this operator,
#' and is defined in much the same way.
#' 
#' @param p is the parser to matched 1 or more times.
#' @export
#' @examples
#' Digit <- function(...) {satisfy(function(x) {return(!!length(grep("[0-9]", x)))})}
#' some(Digit()) ("123abc")
some <- function(p) {
  return(function(string){
    (p %then% many(p)) (string)
  })
}

#' 
#' \code{do} combinator
#' 
#' Accepts a list of funtions to perform parsing. The last element is assumed to
#' be the return(ing) function
#' 
#' do : this is is the list of parsers the list will assume that the last
#' element is the function to be applied, based on the variables in do. This
#' function can also pickup "leftovers" via the argument "leftover_" which has
#' been reserved especially for `f`. sample usage: `do(do=list(t=natural()),
#' f=function(t,leftover_) {return(leftover_)}) ("123 123")`
#' 
#' For example, `do` can be like: do(x=item(), item(), y=item(),
#' function(x,y){c(x,y)}) ("abcde")
#' @export
#' @examples
#' do(x=item(), item(), y=item(), f = function(x,y) {c(x,y)}) ("abcdef")
#' do(x=item(), item, y=item(), f = function(x,y) {c(x,y)}) ("ab")
do <- function(...) {
  .Deprecated("do function can be replicated with 'then' and 'using'")
  do <- list(...)
  return(function(string){
    doResult <- list()
    result <- list(leftover = string)
    
    # a try function would be helpful here!
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



## Define the Generic Parsers ##

#' Digit checks for single digit
#' 
#' @export
#' @examples
#' Digit()("123")
Digit <- function(...) {satisfy(function(x) {return(!!length(grep("[0-9]", x)))})}

#' Lower checks for single lower case character
#' 
#' @export
#' @examples
#' Lower() ("abc")
Lower <- function(...) {satisfy(function(x) {return(!!length(grep("[a-z]", x)))})}

#' Upper checks for a single upper case character
#' 
#' @export
#' @examples
#' Upper()("Abc")
Upper <- function(...) satisfy(function(x) {return(!!length(grep("[A-Z]", x)))})

#' Alpha checks for single alphabet character
#' 
#' @export
#' @examples
#' Alpha()("abc")
Alpha <- function(...) satisfy(function(x) {return(!!length(grep("[A-Za-z]", x)))})

#' AlphaNum checks for a single alphanumeric character
#' 
#' @export
#' @examples
#' AlphaNum()("123")
#' AlphaNum()("abc123")
AlphaNum <- function(...) satisfy(function(x) {return(!!length(grep("[A-Za-z0-9]", x)))})

#' char checks for a single character of your choice
#' 
#' @export
#' @examples
#' Char("a")("abc")
Char <- function(c) {satisfy(function(x) {return(c==x)})}

#' Space checks for a single space character
#' 
#' @export
#' @examples
#' Space()(" 123")
Space <- function(...) satisfy(function(x) {return(!!length(grep("\\s", x)))})

#' \code{string} is a combinator which allows us to build parsers whcih
#' recognise strings of symbols, rather than just single symbols
#' 
#' @param string is the string to be matched
#' @export
#' @examples
#' string("123")("123 abc")
String <- function(string) {
  if (string=="") {
    return (succeed(NULL))
  }
  else {
    result_=substr(string, 1, 1)
    leftover_=substring(string, 2)
    return((literal(result_) %then% 
            String(leftover_)) %using% 
             function(x) {paste(unlist(c(x)), collapse="")})
  }
}

#' ident is identify, which is lowercase followed by zero or more alphanumeric
#' ident || Parser String
#' @export
#' @examples
#' ident() ("variable1 = 123")
ident <- function() {(many(AlphaNum()) %using%
            function(x) paste0(unlist(c(x)), collapse=""))}

#' nat matches natural numbers
#' nat || Parser Int
#' @export
#' @examples
#' nat() ("123 + 456")
nat <- function() {
  some(Digit()) %using%
    function(x) {paste(unlist(c(x)), collapse="")}
}

#' space matches spaces
#' space || Parser ()
#' @export
#' @examples
#' space() ("  abc")
space <- function() {
  many(Space()) %using%
    function(x) {return("")}
}

#' token strips spaces as needed
#' token || Parser a -> Parser a
#' @export
#' @examples
#' token(ident()) ("   variable1   ")
token <- function(p) {
  space() %then%
    p %then%
    space() %using%
    function(x) {return(unlist(c(x))[2])}
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
