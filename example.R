#' Example with expressions, will not be exported
#' expression example
#' expr :: = term ( + expr | e)
#' term :: = factor ( * term | e) 
#' factor :: = (expr) | nat
#' nat :: = 0 | 1 | 2 | ...

expr <- ((term %then% 
            symbol("+") %then%
            expr %using% function(x) {
              print(unlist(c(x)))
              return(paste(unlist(c(x)), collapse=""))
            }) %alt% term)


term <- ((factor %then% 
             symbol("*") %then%
             term %using% function(x) {
               print(unlist(c(x))[c(1,3)])
               return(paste(unlist(c(x)), collapse=""))
             }) %alt% factor)

factor <- ((
    symbol("(") %then%
      expr %then%
      symbol(")") %using% 
      function(x){
        print(unlist(c(x)))
        return(paste(unlist(c(x)), collapse=""))
        })
    %alt% natural())

expr("1+2+3+4+5+6")
expr("1+(2+3)*4+5+6")
factor("(1)")
factor("1")
expr("1+(2*2)")
expr("(1+1)*2")
expr("1+2")
term("1*1")






