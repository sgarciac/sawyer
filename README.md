# sawyer
A Common Lisp lisp parser for [TOML](https://github.com/toml-lang/toml), fully supporting version 0.4.0

Sawyer depends on the following parsing libraries, which are unfortunately not in quicklisp, so you might want to install them by hand (cloning them in the folder "local-projects" inside your quicklisp directory will do).

 * [re](https://github.com/massung/re)
 * [lexer](https://github.com/massung/lexer)
 * [parse](https://github.com/massung/parse)

Usage:

```commonlisp
(sawyer:parse-toml-string "whatever = {a = [1,2,3]}")  ; => (:OBJ ("whatever"
:OBJ ("a" . #(1 2 3)))) 
```
 


