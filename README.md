# cl-oju

![build](https://github.com/eigenhombre/cl-oju/actions/workflows/build.yml/badge.svg)

<img src="/words.jpg" width="400">

> What is *oju*? A Mysterious Sauce used as a special ingredient
> sourced (some would say pilfered) from an advanced civilization
> thirty years in the future (far beyond the late 1980's).

`cl-oju`: a few Clojure idioms I missed from Common Lisp (a few of which I'm
still getting used to the standard CL idiom for).

If you're coming from Clojure, it may be better to learn "the Common
Lisp way" of doing things than to rely on these functions; but this
library can still be a reference point to see *a* way of doing things
you're used to.

Note that I stick to the functional *interface* of familiar Clojure
functions, but do not worry about immutable data structures or
laziness in this library.

Supported operators:

    comment
    drop
    filter
    frequencies
    group-by
    interleave
    interpose
    juxt
    neg?
    partial
    partition-all
    partition-n   (called "partition" in Clojure)
    pos?
    rand-int
    rand-nth
    range
    repeatedly
    slurp
    sort-by
    spit
    take

# Usage

This is not on Quicklisp (yet).  Check it out from source and put it in `LISP_HOME`.  Then,

    (ql:quickload 'cl-oju)

# Testing / Building

Only tested in SBCL, but the code is general enough that it should
work in any compliant Common Lisp implementation.  `make test` should
be sufficient to run the tests.  You can also run them in Docker
(`make docker`), which is what the CI for this project does.  You'll
obviously need SBCL, `make` and Docker installed for this.
