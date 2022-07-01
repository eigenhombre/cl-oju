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
these out of habit, I wanted a place to put them.  I haven't needed to
worry too much about comprehensiveness or performance, though PRs to
improve either will be gratefully reviewed.

Note that I stick to the functional *interface* of familiar Clojure
functions, but do not worry about immutable data structures or
laziness in this library.

If you're new to Common Lisp coming from Clojure, it may be better to
learn "the Common Lisp way" of doing things than to rely on these
functions; but this library can still be a reference point to see *a*
way of doing things you're used to.

Supported operators:

    comment
    comp
    drop
    filter
    frequencies
    group-by
    interleave
    interpose
    juxt
    neg?
    not=          (based on #'EQUAL)
    partial
    partition-all
    partition-by
    partition-n   (called "partition" in Clojure)
    pos?
    rand-int
    rand-nth
    range
    repeatedly
    slurp
    sort-by
    spit
    str
    take
    take-while

# Usage

This is not on Quicklisp (yet; [issue filed](https://github.com/quicklisp/quicklisp-projects/issues/2183)).  In the mean time, you'll need to check this repo out from source and put it in `LISP_HOME`.  Then,

    (ql:quickload :cl-oju)

Example:

    (in-package :cl-oju)

    (mapcar (juxt (comp #'length #'str)
                  #'identity)
            (repeatedly 10 (lambda () (rand-int 1000))))
    ;;=>
    '((3 717) (3 508) (3 238) (3 366) (2 50) (3 564) (3 395) (2 18) (3 446) (3 935))

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
