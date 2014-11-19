parses(returns, "1") ("abc") 
returns("1") ("abc")

parses(failure) ("abc")
failure() ("abc")

parses(item) ("")
item() ("")

parses(item) ("abc")
item() ("abc")

# checking functionality of then
then(item(), returns("123")) ("abc")
( item() %>>=% returns("123") ) ("abc")

#' copying example in book.
#'
#' ```
#' p :: Parser (Char,Char)
#' p = do x ← item
#' item
#' y ← item
#' return (x,y)
#' ```
#' 
#' ```
#' parse p "abcdef" = [(('a', 'c'), "def")]
#' parse p "ab" = []
#' ```

( item() %>>=% returns("1") ) ("abc")
( item() %>>=% item() %>>=% item() %>>=% failure() ) ("abcdefghi")

# testing do function
do(do=list(x=item(), item(), y=item()), f = function(x,y) {c(x,y)}) ("abcdef")
do(do=list(x=item(), item(), y=item()), f = function(x,y) {c(x,y)}) ("ab")
parses(do, do=list(x=item(), item(), y=item()), f = function(x,y) {c(x,y)}) ("abcdef")

# testing choice function
choice(returns("1"), returns("2")) ("abcdef")
(item() %+++% returns("2")) ("abcdef")
parses(choice, returns("1"), returns("2")) ("abcde")

# testing sat
sat(is.character) ("abc")
sat(is.character) ("123")

Char("p") ("pdf")
Char("q") ("pdf")
Digit() ("abc")
Digit() ("1abc")

parse(Digit) ("123")
parse(Char, "a") ("abc")

String("ab") ("abcde")
String("ab") ("1234")
parse(String, "ab") ("abcd")

# testing many and many1
many(Digit()) ("123abc")
many(Digit()) ("abcde")
many1(Digit()) ("abc")

ident() ("abc def")
nat() ("123a")
space() ("    abc")

natural() ("   123  1")

# do many example
many(do(do=list(y=symbol(","),
                x=natural()), 
        function(x,y){
          unlist(c(y,x))
        })) (", 123 ,456 ,7, 8, 9 ")

#' below is an example on parsing a list of numbers
#' The parser will parse things like:
#' 
#' [1,2,3]
#' 
#' but not
#' 
#' [1,2,]
#' numlist :: Parser [Int]
numlist <- function(...) {do(
  do = list(
    symbol("["),
    n=natural(),
    ns=many(do(do=list(y=symbol(","),
                       x=natural()), 
               function(x,y){
                 unlist(c(y,x))
                 
                 # trying to coerce into R object
                 # this sort of works, but tuples aren't really supported in R
                 return(c(eval(parse(text=x))))
               })),
    symbol("]")
    ),
  f=function(n,ns){
    #unlist(c("[", n, ns, "]"))
    unlist(c(n, ns))
    
    # using eval to actually return an R object...
    # this sort of works, but tuples aren't really supported in R
    return(c(eval(parse(text=n)), ns))
    
  })}
numlist() ("[12,34,5] abcde")
parses(numlist) ("[1,2,3] abcde")
numlist() ("[1,2,] abcde")
parses(numlist) ("[1,2,] abcde")
(numlist() %>>=% identifier()) ("[1,23,4] abcd")

#' parsing with nested do lists
#' 
#' The parser defined below is simply to parse things in the form:
#' 1
#' 1+2
#' 1+2+3+...
#' 
#' unlimited times, using only do lists (not the many function)
#' this is important because we would need to modify it to accept other
#' expressions as well
expr <- do(do=list(t=natural()),
  f=function(t, leftover_) {return(   
    (do(do=list(
      symbol("+"),
      e=expr()
      ),
    f=function(t,e) {
      as.numeric(t) + as.numeric(e)
    }) %+++% returns(t)) (leftover_)
  )})

do(do=list(t=natural()), f=function(t,leftover_) {return(leftover_)}) ("123 123")
natural() ("123 123")


#' expression example
#' expr :: = term ( + expr | e)
#' term :: = factor ( * term | e) 
#' factor :: = (expr) | nat
#' nat :: = 0 | 1 | 2 | ...

