This project is just an example to examine the functional components of R. 

Goals
=====

Create a [parser combinator](http://en.wikipedia.org/wiki/Parser_combinator) written in
pure R. This is mostly a proof of concept, but could be useful or helpful to someone.

This is inspired by **Programming in Haskell by Graham Hutton**, and also the [JavaScript port](https://github.com/matthandlersux/functional-parser).

File Structure
==============

Currently this isn't really an R package. Everything lives in `parser.r` with sample usage in `example.r`.

Notes
=====

Some notes are as follows:

`parse`
-------

If you look at my examples, you'll realise that it looks really strange compared to the method prescribed in the Haskell way. This is not _really_ intentional, but it sort of just happened. If you have awesome ideas how this could be in some other OO structure, let me know, otherwise I will leave it as it is! (I like it this way!)

`returns`
---------

Similar to the JavaScript port, the `return` function has been renamed to `returns`.

bind/then/`>>=`?
----------------

This _thing_ seems to have many names. Since I'm a beginner in Haskell I might have offended someone with the wrong name. Feel free to correct me!

do notation
-----------

This piece in the code is really ugly. I will change it to `invisible(apply())` again, if you have any suggestions please let me know.


