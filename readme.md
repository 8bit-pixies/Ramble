This project is just an example to examine the functional components of R. 

Goals
=====

Create a [parser combinator](http://en.wikipedia.org/wiki/Parser_combinator) written in
pure R. This is mostly a proof of concept, but could be useful or helpful to someone.

This is inspired by **Programming in Haskell by Graham Hutton**, and also the [JavaScript port](https://github.com/matthandlersux/functional-parser).


Notes
=====

Some notes are as follows:

`returns`
---------

Similar to the JavaScript port, the `return` function has been renamed to `returns`.

bind/then/`>>=`?
----------------

This _thing_ seems to have many names. Since I'm a beginner in Haskell I might have offended someone with the wrong name. Feel free to correct me!

do notation
-----------

This function accepts a list, and treats the last entry as the "returns". It interates over each element in the list sequentially.


Example
=======

```r
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
```

**Output**:  

```r
> factor("(1)")
[1] "(" "1" ")"
$result
[1] "(" "1" ")"

$leftover
[1] ""

> factor("1")
$result
[1] "1"

$leftover
[1] ""

> expr("1+(2*2)")
[1] "2" "*" "2"
[1] "(" "2" "*" "2" ")"
[1] "1" "+" "(" "2" "*" "2" ")"
$result
[1] "1" "+" "(" "2" "*" "2" ")"

$leftover
[1] ""


```
