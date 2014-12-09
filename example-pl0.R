#' PL/0 language using Ramble
#' 
#' [PL/0](http://en.wikipedia.org/wiki/PL/0) is a simple language which is
#' used for educational purposes. Here we will provide a sample implementation
#' based on it.
#' 
#' ## EBNF grammar
#' 
#' program = block "." .
#' 
#' block = [ "const" ident "=" number {"," ident "=" number} ";"]
#' [ "var" ident {"," ident} ";"]
#' { "procedure" ident ";" block ";" } statement .
#' 
#' statement = [ ident ":=" expression | "call" ident 
#'               | "?" ident | "!" expression 
#'               | "begin" statement {";" statement } "end" 
#'               | "if" condition "then" statement 
#'               | "while" condition "do" statement ].
#' 
#' condition = "odd" expression |
#'   expression ("="|"#"|"<"|"<="|">"|">=") expression .
#' 
#' expression = [ "+"|"-"] term { ("+"|"-") term}.
#' 
#' term = factor {("*"|"/") factor}.
#' 
#' factor = ident | number | "(" expression ")".

#' for the purposes of this example, all the assignments will be of the form
#' `assign(x, value, env=PL0)`, this will then assign the variable to 
#' PL0 environment, which we will define.
#' 
#' Currently this implementation is incomplete, as it is missing block and while grammar and logic

#' getVar is a helper function. If the value is numeric it will return it, 
#' otherwise it will try to call it from PL0 envir.
getVar <- function(x) {
  if (is.numeric(x)){
    return (x)
  }
  else {
    return(get(x, envir=PL0))
  }
}

#' evalCond evaluates condition which is currently a string.
evalCond <- function(x) {
  fst <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(fst)) { fst <- get(x[[1]], envir=PL0)}
  snd <- suppressWarnings(as.numeric(x[[3]]))
  if (is.na(snd)) { fst <- get(x[[3]], envir=PL0)}
  
  # now actually determine if it is true or false
  if (x[[2]] == "<") {
    try(bool <- fst < snd, silent=TRUE)
  }
  else if (x[[2]] == ">") {
    try(bool <- fst > snd, silent=TRUE)
  }
  else if (x[[2]] == "=") {
    try(bool <- fst == snd, silent=TRUE)
  }
  else if (x[[2]] == "<=") {
    try(bool <- fst <= snd, silent=TRUE)
  }
  else if (x[[2]] == ">=") {
    try(bool <- fst >= snd, silent=TRUE)
  }
  else {
    warning("boolean symbol should have matched, please check PL\\0 implementation")
    return(x)
  }
  return(if (is.na(bool)) x else bool)
}

#' evalPrint prints when called.

PL0 <- new.env()
PL0.preserve <- FALSE

# statement("if 1 > 2 then var1 := 3")
# statement("test := 1+2+3")
# statement("call test")
# invisible(statement("begin x := 1; x := x + 1; ! x end"))
# statement("begin x := 10; while x > 0 do begin x := x - 1; ! x end")
statement <- (((identifier() %then% token(String(":=")) %then% expr)
               %using% function(stateVar) {
                 if (stateVar[[2]] == ":=") {
                   assign(stateVar[[1]], stateVar[[3]], env=PL0)
                 }
                 return(stateVar)
               })
            %alt% (symbol("!") %then% identifier() 
                    %using% function(stateVar) {
                      # this calls a defined function (procedure)
                      print(get(stateVar[[2]], envir = PL0))
                      return(stateVar)
                    })
            %alt% (((token(String("if")) %then% condition %then% token(String("then"))) 
                    # this solution copies EVERYTHING in the environment to another environment
                    # this would clearly NOT be the wisest idea if it were a production implementation
                    %using% function(x) {
                      PL0.preserve <<- not(evalCond(x[[2]]))
                      if(not(PL0.preserve)) {   
                        return("")
                      }
                      else {
                        for(n in ls(PL0, all.names=TRUE)) assign(n, get(n, PL0), PL0.bak)
                        return("")
                      }
                    })
                    %then% statement %using% function(x) {
                      if(PL0.preserve) {
                        for(n in ls(PL0.bak, all.names=TRUE)) assign(n, get(n, PL0.bak), PL0)
                        PL0.preserve <<- FALSE
                        return("")
                      }
                      return(x)
                    })
             %alt% (token(String("begin")) %then% (statement %then% many(symbol(";") %then% statement))
                    %then% token(String("end")))
             %alt% (token(String("call")) %then% identifier())
             %alt% (token(String("while")) %then% condition %then% token(String("do")) %then% statement %using% function(x) {
                statementVal <- paste(unlist(x[-c(1,2,3)]), collapse=" ") # exclude "while" "condition" and "do"
                cond <- x[[2]] # the condition to be checked.
                print(statementVal)
                return(x)
              })
    )

condition <- (expr %then% (token(String("<="))
                           %alt% token(String(">="))
                           %alt% symbol("=") 
                           %alt% symbol("<")
                           %alt% symbol(">")) 
                   %then% expr %using% function(x) {
                     return(unlist(x)) # this function is evaluated when evalCond is called
                   })

expr <- ((term %then% 
            symbol("+") %then%
            expr %using% function(x) {
              #print(unlist(c(x)))
              return(getVar(x[[1]])+getVar(x[[3]]))
            }) %alt% 
           (term %then% 
              symbol("-") %then%
              expr %using% function(x) {
                #print(unlist(c(x)))
                return(getVar(x[[1]])-getVar(x[[3]]))
              }) %alt% term)

term <- ((factor %then% 
            symbol("*") %then%
            term %using% function(x) {
              #print(unlist(c(x)))
              return(getVar(x[[1]])*getVar(x[[3]]))
            }) %alt% 
           (factor %then% 
              symbol("/") %then%
              term %using% function(x) {
                return(getVar(x[[1]])/getVar(x[[3]]))
              }) %alt% factor)

factor <- ((symbol("(") %then%
              expr %then%
              symbol(")") %using% function(x){
                #print(unlist(c(x)))
                return(getVar(x[[2]]))
              }) %alt% (natural() %using% function(x) {
                as.numeric(x)
              })
                 %alt% (identifier()))

