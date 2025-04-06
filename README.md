# cl-oju

![build](https://github.com/eigenhombre/cl-oju/actions/workflows/build.yml/badge.svg)

<img src="/words.jpg" width="400">

> What is *oju*? A Mysterious Sauce used as a special ingredient
> sourced (some would say pilfered) from an advanced civilization
> thirty years in the future (far beyond the late 1980's).

`cl-oju`: a few idioms, mostly relating to sequences, that I miss
when writing Common Lisp.

There are many more comprehensive utility libraries out there, such as
[Serapeum](https://github.com/ruricolist/serapeum).  This one is
focused on a fairly narrow goal: to provide fairly simple equivalents
to core Clojure functions.  Since I frequently reach for (and write)
these out of habit, I wanted a place to put them.

I haven't needed to worry too much about comprehensiveness or
performance, though PRs to improve either will be gratefully reviewed.
I typically add functions here when I reach for them out of habit while
working in Common Lisp.

Note that I stick to the functional *interface* of familiar Clojure
expressions, but do not worry about immutable data structures or
laziness in this library.

If you're new to Common Lisp coming from Clojure, it may be better to
learn "the Common Lisp way" of doing things than to rely on these
functions; but this library can still be a reference point to see *a*
way of doing things you're used to.

Supported operators:

    comment
    comp
    distinct
    drop
    drop-while
    filter
    frequencies
    group-by
    if-let
    if-not
    interleave
    interpose
    juxt
    neg?
    not=          (based on #'EQUAL)
    partial
    partition-all
    partition-by
    clj-partition (called "partition" in Clojure;
                   also aliased to "partition-n")
    pos?
    clj-print     (called "print" in Clojure)
    println
    rand-int
    rand-nth
    range
    repeat
    repeatedly
    slurp
    sort-by
    spit
    str
    take
    take-while
    when-not
    with-out-str

# Usage

Available on [Quicklisp](https://quicklisp.org) or
[Ultralisp](https://ultralisp.org/projects/eigenhombre/cl-oju):

    (ql:quickload :cl-oju)

Example:

    (in-package :cl-oju)

    (mapcar (juxt (comp #'length #'str)
                  #'identity)
            (repeatedly 10 (lambda () (rand-int 1000))))
    ;;=>
    '((3 717) (3 508) (3 238) (3 366) (2 50)
      (3 564) (3 395) (2 18) (3 446) (3 935))

# Testing / Building

`make test` should be sufficient to run the tests using SBCL, or `make
test-ecl` for ECL.  You can also run them in Docker (`make docker`),
which is what the CI for this project does.  You'll obviously need
`make` and Docker installed for this.

# License

[MIT](https://github.com/eigenhombre/cl-oju/blob/master/LICENSE)

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
