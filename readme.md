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


