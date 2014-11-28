#' Example with expressions, will not be exported

expr <- do(list(t=term,
                function(t, leftover_) {
                  return(
                    (do(list(s=symbol("+"),
                             e=expr, 
                             function(s,e) { 
                               print(unlist(c(t,s,e)))
                               return(unlist(c(t,s,e)) )
                             }))
                     %+++% returns(t)) (leftover_)
                  )
                }))

term <- do(list(f=factor, 
                function(f, leftover_){
                  return(
                    (do(list(s=symbol("*"),
                             t=term,
                             function(s,t) {
                               print(unlist(c(f,s,t)))
                               return(unlist(c(f,s,t)))
                             }))
                     %+++% return(f)                 
                    ) (leftover_)
                  )
                }))

factor <- (do(list(sl=symbol("("),
                   e=expr,
                   sr=symbol(")"),
                   function(sl, e, sr) {
                     print(unlist(c(sl, e, sr)))
                     return(unlist(c(sl, e, sr)))
                   }))
           %+++% natural)

expr("1+(2*2)")

