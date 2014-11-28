#' Example with expressions, will not be exported
#' expression example
#' expr :: = term ( + expr | e)
#' term :: = factor ( * term | e) 
#' factor :: = (expr) | nat
#' nat :: = 0 | 1 | 2 | ...

expr <- do(f=term, 
           function(f, leftover_){
             return(
               (do(s=symbol("+"),
                   t=expr,
                   function(s,t) {
                     print(unlist(c(f,s,t)))
                     return(unlist(c(f,s,t)))
                   })
                %+++% return(f)) (leftover_)
             )
           })

term <- do(f=factor, 
           function(f, leftover_){
             return(
               (do(s=symbol("*"),
                   t=term,
                   function(s,t) {
                     print(unlist(c(f,s,t)))
                     return(unlist(c(f,s,t)))
                   })
                %+++% return(f)) (leftover_)
             )
           })

factor <- (do(sl=symbol("("),
                   e=expr,
                   sr=symbol(")"),
              function(sl, e, sr) {
                print(unlist(c(sl, e, sr)))
                return(unlist(c(sl, e, sr)))
              })
           %+++% natural ())

factor("(1)")
factor("1")
expr("1+(2*2)")
expr("(1+1)*2")
expr("1+2")
term("1*1")






