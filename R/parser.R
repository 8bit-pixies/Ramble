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
  function(nextString) {
    list(result=string,
         leftover=nextString)
  }
}

#' \code{item} is a parser that consumes the first character of the string and
#' returns the rest. If it cannot consume a single character from the string, it
#' will emit the empty list, indicating the parser has failed.
#' 
#' @param ... additional arguments for the parser
#' @export
#' @examples
#' item() ("abc")
#' item() ("")
item <- function(...){
  return(function(string){
    if(length(string)==0){
      return(NULL)
    }
    if(string=="") {
      list()
    } else {
      list(result=substr(string, 1, 1),
           leftover=substring(string, 2))
    }
  })
}

#' \code{satisfy} is a function which allows us to make parsers that recognise single symbols. 
#' 
#' @param p is the predicate to determine if the arbitrary symbol is a member.
#' @export
satisfy <- function(p) {
  return(function(string) {
    if (length(string) == 0) {
      return(list())
    }
    else if (string == "") {
      return(list())
    }
    else {
      result_ <- list(result=substr(string, 1, 1),
                      leftover=substring(string, 2))
      if (p(result_$result)) {
        return(succeed(result_$result)(result_$leftover))
      } else {
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
  satisfy(function(x){
    return(x==char)
  })
}

## Building Combinators ##

#' \code{alt} combinator is similar to alternation in BNF. the parser 
#' \code{(alt(p1, p2))} recognises anything that \code{p1} or \code{p2} would. 
#' The approach taken in this parser follows (Fairbairn86), in which either is 
#' interpretted in a sequential (or exclusive) manner, returning the result of
#' the first parser to succeed, and failure if neither does.
#' 
#' \code{\%alt\%} is the infix notation for the \code{alt} function, and it is the
#' preferred way to use the \code{alt} operator.
#' 
#' @param p1 the first parser
#' @param p2 the second parser
#' @return Returns the first parser if it suceeds otherwise the second parser
#' @examples
#' (item() %alt% succeed("2")) ("abcdef")
#' @seealso \code{\link{then}}
alt <- function(p1, p2) {
  function(string){
    result <- p1(string)
    if(!is.null(result$leftover)) {
      result
    } else {
      p2(string)
    }
  }
}

#' @export
`%alt%` <- alt

#' \code{then} combinator corresponds to sequencing in BNF. The parser 
#' \code{(then(p1, p2))} recognises anything that \code{p1} and \code{p2} would 
#' if placed in succession.
#' 
#' \code{\%then\%} is the infix operator for the then combinator, and it is the
#' preferred way to use the \code{then} operator.
#' 
#' @param p1 the first parser
#' @param p2 the second parser
#' @return recognises anything that \code{p1} and \code{p2} would if placed in 
#'   succession.
#' @examples
#' (item() %then% succeed("123")) ("abc")
#' @seealso \code{\link{alt}}, \code{\link{thentree}}
then <- function(p1, p2) {
  function(string) {
    result <- p1(string)
    if (length(result) == 0) {
      list()
    } else {
      result_ <- p2(result$leftover)
      if (length(result_$leftover) == 0 ||
          is.null(result_$leftover)) {
        list()
      } else {
        list(result=Unlist(append(list(result$result),
                                  list(result_$result))),
             leftover=result_$leftover)
      }
    }
  }
}

#' @export
`%then%` <- then

#' \code{thentree} keeps the full tree representation of the results of parsing.
#' Otherwise, it is identical to \code{then}.
#' 
#' \code{\%thentree\%} is the infix operator for the then combinator, and it is
#' the preferred way to use the \code{thentree} operator.
#' 
#' @param p1 the first parser
#' @param p2 the second parser
#' @return recognises anything that \code{p1} and \code{p2} would if placed in 
#'   succession.
#' @examples
#' (item() %thentree% succeed("123")) ("abc")
#' @seealso \code{\link{alt}}, \code{\link{thentree}}
thentree <- function(p1, p2) {
  function(string) {
    result <- p1(string)
    if (length(result) == 0) {
      list()
    } else {
      result_ <- p2(result$leftover)
      if (length(result_$leftover) == 0 ||
          is.null(result_$leftover)) {
        list()
      } else {
        list(result=list(result$result,
                         result_$result),
             leftover=result_$leftover)
      }
    }
  }
}

#' @export
`%thentree%` <- thentree

#' \code{using} combinator allows us to manipulate results from a parser, for 
#' example building a parse tree. The parser \code{(p \%using\% f)} has the same 
#' behaviour as the parser \code{p}, except that the function \code{f} is
#' applied to each of its result values.
#' 
#' \code{\%using\%} is the infix operator for \code{using}, and it is the
#' preferred way to use the \code{using} operator.
#' 
#' @param p is the parser to be applied
#' @param f is the function to be applied to each result of \code{p}.
#' @return The parser \code{(p \%using\% f)} has the same behaviour as the
#'   parser \code{p}, except that the function \code{f} is applied to each of
#'   its result values.
#' @examples
#' (item() %using% as.numeric) ("1abc")
using <- function(p, f) {
  return(function(string) {
    result <- p (string) 
    if(length(result) == 0) {
      return(list())
    }
    list(result=f(result$result),
         leftover=result$leftover)
  })
}

#' @export
`%using%` <- using

#' \code{maybe} matches 0 or 1 of pattern \code{p}.  In EBNF notation, this
#' corresponds to a question mark ('?').
#' 
#' @param p is the parser to be matched 0 or 1 times.
#' @export
#' @examples
#' maybe(Digit())("123abc")
#' maybe(Digit())("abc123")
#' @seealso \code{\link{many}}, \code{\link{some}}
maybe <- function(p) {
  function(string) {
    (p %alt% succeed(NULL))(string)
  }
}

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
#' @param p is the parser to match 0 or more times.
#' @export
#' @examples
#' Digit <- function(...) {satisfy(function(x) {return(!!length(grep("[0-9]", x)))})}
#' many(Digit()) ("123abc")
#' many(Digit()) ("abc")
#' @seealso \code{\link{maybe}}, \code{\link{some}}
many <- function(p) {
  function(string) {
    ((p %then% many(p)) %alt% succeed(NULL))(string)
  }
}

#' \code{some} matches 1 or more of pattern \code{p}. in BNF notation, repetition occurs often enough to merit its own abbreviation. When zero or 
#' more repetitions of a phrase \code{p} are admissible, we simply write 
#' \code{p+}. The \code{some} combinator corresponds directly to this operator,
#' and is defined in much the same way.
#' 
#' @param p is the parser to match 1 or more times.
#' @export
#' @examples
#' Digit <- function(...) {satisfy(function(x) {return(!!length(grep("[0-9]", x)))})}
#' some(Digit()) ("123abc")
#' @seealso \code{\link{maybe}}, \code{\link{many}}
some <- function(p) {
  function(string) {
    (p %then% many(p))(string)
  }
}

## Define the derived primitives ##

#' Digit checks for single digit
#' 
#' @param ... additional arguments for the primitives to be parsed
#' @export
#' @examples
#' Digit()("123")
#' @seealso \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
Digit <- function(...) {
  satisfy(function(x) {
    !!length(grep("[0-9]", x))
  })
}

#' Lower checks for single lower case character
#' 
#' @param ... additional arguments for the primitives to be parsed
#' @export
#' @examples
#' Lower() ("abc")
#' @seealso \code{\link{Digit}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
Lower <- function(...) {
  satisfy(function(x) {
    !!length(grep("[a-z]", x))
  })
}

#' Upper checks for a single upper case character
#' 
#' @param ... additional arguments for the primitives to be parsed
#' @export
#' @examples
#' Upper()("Abc")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
Upper <- function(...) {
  satisfy(function(x) {
    !!length(grep("[A-Z]", x))
  })
}

#' Alpha checks for single alphabet character
#' 
#' @param ... additional arguments for the primitives to be parsed
#' @export
#' @examples
#' Alpha()("abc")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
Alpha <- function(...) {
  satisfy(function(x) {
    !!length(grep("[A-Za-z]", x))
  })
}

#' AlphaNum checks for a single alphanumeric character
#' 
#' @param ... additional arguments for the primitives to be parsed
#' @export
#' @examples
#' AlphaNum()("123")
#' AlphaNum()("abc123")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
AlphaNum <- function(...) {
  satisfy(function(x) {
    !!length(grep("[A-Za-z0-9]", x))
  })
}

#' SpaceCheck checks for a single space character
#' 
#' @param ... additional arguments for the primitives to be parsed
#' @export
#' @examples
#' SpaceCheck()(" 123")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}},
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
SpaceCheck <- function(...) {
  satisfy(function(x) {
    !!length(grep("\\s", x))
  })
}

#' \code{String} is a combinator which allows us to build parsers which
#' recognise strings of symbols, rather than just single symbols
#' 
#' @param string is the string to be matched
#' @export
#' @examples
#' String("123")("123 abc")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
String <- function(string) {
  if (string=="") {
    succeed(NULL)
  } else {
    result_ <- substr(string, 1, 1)
    leftover_ <- substring(string, 2)
    (literal(result_) %then% 
      String(leftover_)) %using% 
      function(x) {
        paste(unlist(c(x)), collapse="")
      }
  }
}

#' \code{ident} is a parser which matches zero or more alphanumeric
#' characters. 
#' 
#' @export
#' @examples
#' ident() ("variable1 = 123")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
ident <- function() {
  (many(AlphaNum()) %using%
     function(x) paste0(unlist(c(x)), collapse=""))
}

#' \code{nat} is a parser which matches one or more numeric characters.
#' 
#' @export
#' @examples
#' nat() ("123 + 456")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}},
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
nat <- function() {
  some(Digit()) %using%
    function(x) {
      paste(unlist(c(x)), collapse="")
    }
}

#' \code{space} matches zero or more space characters.
#' 
#' @export
#' @examples
#' space() ("  abc")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
space <- function() {
  many(SpaceCheck()) %using%
    function(x) {
      ""
    }
}

#' \code{token} is a new primitive that ignores any space before and after
#' applying a parser to a token.
#' 
#' @param p is the parser to have spaces stripped.
#' @export
#' @examples
#' token(ident()) ("   variable1   ")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{identifier}},
#'   \code{\link{natural}}, \code{\link{symbol}}
token <- function(p) {
  space() %then%
    p %then%
    space() %using%
    function(x) {
      x <- x[-1]
      x <- x[-length(x)]
      x
    }
}

#' \code{identifier} creates an identifier
#' 
#' @param ... takes in token primitives
#' @export
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}},
#'   \code{\link{natural}}, \code{\link{symbol}}
identifier <- function(...) {
  token(ident())
}

#' \code{natural} creates a token parser for natural numbers
#' 
#' @param ... additional arguments for the parser
#' @export
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{symbol}}
natural <- function(...) {
  token(nat())
}

#' \code{symbol} creates a token for a symbol
#' 
#' @param xs takes in a string to create a token
#' @export
#' @examples
#' symbol("[") ("  [123]")
#' @seealso \code{\link{Digit}}, \code{\link{Lower}}, \code{\link{Upper}}, 
#'   \code{\link{Alpha}}, \code{\link{AlphaNum}}, \code{\link{SpaceCheck}}, 
#'   \code{\link{String}}, \code{\link{ident}}, \code{\link{nat}}, 
#'   \code{\link{space}}, \code{\link{token}}, \code{\link{identifier}},
#'   \code{\link{natural}}
symbol <- function(xs) {
  token(String(xs))
}
