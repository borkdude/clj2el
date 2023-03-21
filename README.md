# clj2el

The clj2el tool transpiles Clojure to Emacs Lisp. It is currently incomplete,
but contributions are welcome. It is targeted at folks who know Clojure better
than Emacs Lisp.

See the interactive web page [here](https://borkdude.github.io/clj2el/).

## CLI

There's also a tiny babashka CLI. The CLI can be installed with [bbin]:

    bbin install io.github.borkdude/clj2el

And used like this:

    $ cat source.clj
    (defn foo [x & xs] xs)

    (inc 2)

    (map inc [1 2 3])
    $ cat source.clj | clj2el
    (defun foo (x &rest xs) xs)

    (1+ 2)

    (mapcar #'1+ (vector 1 2 3))

[bbin]: https://github.com/babashka/bbin
