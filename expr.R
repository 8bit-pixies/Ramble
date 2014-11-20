#' expression example
#' expr :: = term ( + expr | e)
#' term :: = factor ( * term | e) 
#' factor :: = (expr) | nat
#' nat :: = 0 | 1 | 2 | ...

expr <- do(do=list(t=term),
               f=function(t, leftover_) {
                 return(
                   (do(
                     list(s=symbol("+"),
                          e=expr), 
                     function(s,e) { 
                       print(unlist(c(t,s,e)))
                       return(unlist(c(t,s,e)) )
                       })
                    %+++% returns(t)) (leftover_)
                 )
               })

term <- do(list(f=factor), 
           function(f, leftover_){
             return(
               (do(list(s=symbol("*"),
                        t=term),
                   function(s,t) {
                     print(unlist(c(f,s,t)))
                     return(unlist(c(f,s,t)))
                   })
                 %+++% return(f)                 
                 ) (leftover_)
             )
           })

factor <- (do(list(sl=symbol("("),
                  e=expr,
                  sr=symbol(")")),
             function(sl, e, sr) {
               print(unlist(c(sl, e, sr)))
               return(unlist(c(sl, e, sr)))
               })
           %+++% natural ())

factor("(1)")
factor("1")

expr("1+(2*2)")


