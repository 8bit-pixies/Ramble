parse(returns, "1") ("abc") 
returns("1") ("abc")

parse(failure) ("abc")
failure() ("abc")

parse(item) ("")
item() ("")

parse(item) ("abc")
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
parse(do, do=list(x=item(), item(), y=item()), f = function(x,y) {c(x,y)}) ("abcdef")

# testing choice function
choice(returns("1"), returns("2")) ("abcdef")
(item() %+++% returns("2")) ("abcdef")
parse(choice, returns("1"), returns("2")) ("abcde")

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
