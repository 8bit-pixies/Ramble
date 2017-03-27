Ramble
======

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Ramble)](http://cran.r-project.org/package=Ramble)
[![Travis-CI Build Status](https://travis-ci.org/chappers/Ramble.svg?branch=master)](https://travis-ci.org/chappers/Ramble)
[![Coverage Status](http://codecov.io/github/chappers/Ramble/coverage.svg?branch=master)](http://codecov.io/github/chappers/Ramble?branch=master)

This project is just an example to examine the functional components of R. 

Installation
============

Ramble is now on CRAN:

    install.packages("Ramble")
    
The development version can be installed from github:

    # install.packages("devtools")
    devtools::install_github("chappers/Ramble")

Goals
=====

Create a [parser combinator](http://en.wikipedia.org/wiki/Parser_combinator) written in
pure R. This is mostly a proof of concept, but could be useful or helpful to someone.

This is inspired by **Programming in Haskell by Graham Hutton**, and also the 
[JavaScript port](https://github.com/matthandlersux/functional-parser), and 
Python's [recursive descent parsing library](https://pypi.python.org/pypi/funcparserlib/).

**References:**  

*  [Higher-order functions for parsing](http://eprints.nottingham.ac.uk/221/1/parsing.pdf)

Contributing
============

You can contribute by opening issues on Github or implementing things yourself and making a pull request. 

Please ensure that package passes all checks with `--as-cran` flag (i.e. via `devtools::check(args = c('--as-cran'))`) before submitting a pull request. 

How it Works
============

To understand the differences between Ramble and other combinatory parsers please read [Ramble: A Parser Combinator in R](https://github.com/chappers/Ramble/blob/master/docs/ramble-introduction.pdf).

Example
=======

You may view examples for:

*  Parsing xml file
*  Creating a simple calculator
*  Reading a number given in words, and converting it to the appropriate numeric value

Within the `examples/*` folder. Below is the calculator example.

```r
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
```

**Output**:  

```r
> expr("(1+1)*2")
[1] "1" "+" "1"
[1] "(" "2" ")"
[1] "2" "*" "2"
[1] "1" "+" "1"
[1] "(" "2" ")"
[1] "2" "*" "2"
[1] "1" "+" "1"
[1] "(" "2" ")"
[1] "2" "*" "2"
$result
[1] 4

$leftover
[1] ""

> expr("(1+2)*3")
[1] "1" "+" "2"
[1] "(" "3" ")"
[1] "3" "*" "3"
[1] "1" "+" "2"
[1] "(" "3" ")"
[1] "3" "*" "3"
[1] "1" "+" "2"
[1] "(" "3" ")"
[1] "3" "*" "3"
$result
[1] 9

$leftover
[1] ""

> expr("1*(2+3)*4*5")
[1] "2" "+" "3"
[1] "(" "5" ")"
[1] "4" "*" "5"
[1] "5"  "*"  "20"
[1] "1"   "*"   "100"
[1] "2" "+" "3"
[1] "(" "5" ")"
[1] "4" "*" "5"
[1] "5"  "*"  "20"
[1] "1"   "*"   "100"
[1] "2" "+" "3"
[1] "(" "5" ")"
[1] "4" "*" "5"
[1] "5"  "*"  "20"
[1] "1"   "*"   "100"
$result
[1] 100

$leftover
[1] ""

> expr("(4-2)+3")
[1] "4" "-" "2"
[1] "(" "2" ")"
[1] "4" "-" "2"
[1] "(" "2" ")"
[1] "4" "-" "2"
[1] "(" "2" ")"
[1] "2" "+" "3"
$result
[1] 5

$leftover
[1] ""
```
