#' Example with expressions, will not be exported
#' expression example
#' expr :: = term + term | term - term | term
#' term :: = factor * factor | factor / factor | factor
#' factor :: = (expr) | digit+

expr <- ((term %then% 
            symbol("+") %then%
            expr %using% function(x) {
              print(unlist(c(x)))
              return(sum(as.numeric(unlist(c(x))[c(1,3)])))
            }) %alt% 
           (term %then% 
              symbol("-") %then%
              expr %using% function(x) {
                print(unlist(c(x)))
                return(Reduce("-", as.numeric(unlist(c(x))[c(1,3)])))
              }) %alt% term)


term <- ((factor %then% 
             symbol("*") %then%
             term %using% function(x) {
               print(unlist(c(x)))
               return(prod(as.numeric(unlist(c(x))[c(1,3)])))
             }) %alt% 
           (factor %then% 
              symbol("/") %then%
              term %using% function(x) {
                print(unlist(c(x)))
                return(Reduce("/", as.numeric(unlist(c(x))[c(1,3)])))
              }) %alt% factor)

factor <- ((
    symbol("(") %then%
      expr %then%
      symbol(")") %using% 
      function(x){
        print(unlist(c(x)))
        return(as.numeric(unlist(c(x))[2]))
        })
    %alt% natural())

expr("1+2+3+4+5+6")
expr("1+(2+3)*4+5+6")
factor("(1)")
factor("1")
expr("1+(2*2)")
expr("(1+1)*2")
expr("(1+2)*3")
expr("1*(2+3)*4*5")
expr("(4-2)+3")
expr("4-2+3") # order is done incorrectly.
expr("4/2")

