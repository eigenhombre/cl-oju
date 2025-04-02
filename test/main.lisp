(defpackage :cl-oju-tests
  (:use :cl :1am :cl-oju)
  (:export :run-tests))

(in-package #:cl-oju-tests)

(defun run-tests () (1am:run))

(test comment-test
  (is (null (comment)))
  (is (null (comment ())))
  (is (null (comment (a b c))))
  (is (null (comment (/ 1 0)))))

(test range-test
  (is (equal '(0 1 2) (range 3)))
  (is (equal '(3 4 5) (range 3 6)))
  (is (equal 10000 (length (range 10000)))))

(test take-test
  (is (equal (take 9 "this is a test")
             '(#\t #\h #\i #\s #\  #\i #\s #\  #\a))))

(test drop-test
  (is (equal (drop 3 (cl-oju:range 10))
             '(3 4 5 6 7 8 9))))

(test interpose-test
  (is (equal (interpose :sep nil) nil))
  (is (equal (interpose :sep '(1)) '(1)))
  (is (equal (interpose :sep '(1 2 3)) '(1 :SEP 2 :SEP 3))))

(test interleave-test
  (is (equal (interleave () ()) ()))
  (is (equal (interleave '(1 2 3) '(:a :b :c))
             '(1 :a 2 :b 3 :c)))
  (is (equal (interleave '(1 2) '(:a :b :c))
             '(1 :a 2 :b))))

(test partition-all-test
  (is (equal (partition-all 2 2 '(a b c d))
             '((a b) (c d))))
  (is (equal (partition-all 2 1 '(a b c d))
             '((a b) (b c) (c d) (d)))))

(test clj-partition-test
  (is (equal (clj-partition 2 2 '(a b c d))
             '((a b) (c d))))
  (is (equal (clj-partition 2 1 '(a b c d))
             '((a b) (b c) (c d)))))
(test juxt-test
  (is (equal (funcall (juxt #'car #'cdr) '(1 2 3))
             '(1 (2 3)))))

(test compose-test
  (is (eq 1 (funcall (comp #'1+) 0)))
  (is (eq 0 (funcall (comp #'1+ #'1-) 0)))
  (is (equal "0" (funcall (comp #'str #'1+ #'1-) 0)))
  (is (equal "((0 1 2 3))" (funcall (comp #'str #'list #'range) 4))))

(test sort-by-test
  (is (equal (sort-by #'car '((2 3) (1 1)))
             '((1 1) (2 3))))
  (is (equal (sort-by (comp #'parse-integer #'car)
                      '(("2" 3) ("1" 1)))
             '(("1" 1) ("2" 3))))
  (is (equal (sort-by #'first
                      '(("b" 3) ("a" 1)))
             '(("a" 1) ("b" 3))))
  (is (equal (sort-by #'first '((1) (3) (2)))
             '((1) (2) (3))))
  (is (equal (sort-by #'car '(("a") ("c") ("b")))
             '(("a") ("b") ("c"))))
  (is (equal (sort-by (lambda (l) (apply #'+ l))
                      '((-1 2) (1 1 1) (2 0 0)))
             '((-1 2) (2 0 0) (1 1 1))))
  (is (equal (sort-by #'first '(("b" 3) ("a" 1)))
             '(("a" 1) ("b" 3))))
  (is (equal (sort-by (juxt #'first #'second)
                      '((1 0) (0 0)))
             '((0 0) (1 0))))
  (let ((sort-fn (juxt
                  (lambda (a) (second (assoc 'date a :test #'string=)))
                  (lambda (a) (second (assoc 'account a :test #'string=))))))
    (is (equal (sort-by sort-fn
                        '(((date "D2") (account "aaa"))
                          ((date "D1") (account "aaa"))))
               '(((date "D1") (account "aaa"))
                 ((date "D2") (account "aaa")))))
    (is (equal (sort-by (juxt sort-fn)
                        '(((date "D1") (account "bbb"))
                          ((date "D1") (account "aaa"))))
               '(((date "D1") (account "aaa"))
                 ((date "D1") (account "bbb")))))))

(test group-by-test
  (is (equal (assoc t (group-by #'evenp '(1 2 3 4 5 6))
                    :test #'equal)
             '(T (6 4 2))))
  (is (equal (assoc "y" (group-by #'(lambda (x) (nth 5 x))
                                  '((1 _ _ _ _ "x")
                                    (2 _ _ _ _ "y")
                                    (3 _ _ _ _ "x")
                                    (4 _ _ _ _ "y")
                                    (5 _ _ _ _ "x")
                                    (6 _ _ _ _ "y")))
                    :test #'equal)
             '("y" ((6 _ _ _ _ "y") (4 _ _ _ _ "y") (2 _ _ _ _ "y"))))))

(test partial-test
  (is (equal (funcall (partial #'+)) 0))
  (is (equal (funcall (partial #'* 2) 3) 6)))

(test pos-neg-test-test
  (is (pos? 1))
  (is (not (pos? 0)))
  (is (not (pos? -1)))
  (is (neg? -1))
  (is (not (neg? 0)))
  (is (not (neg? 1))))

(test filter-test
  (is (null ()))
  (is (equal (range 3)
             (range 3)))
  (is (null (filter (constantly nil)
                           (range 10))))
  (is (equal (range 10)
             (filter (constantly t)
                            (range 10))))
  (is (equal '(1 3 5) (filter #'oddp (range 6)))))

(test take-while-test
  (is (null (take-while #'evenp ())))
  (is (equal '(0)
             (take-while #'evenp (range 3))))
  (let ((f (lambda (x) (< x 3))))
    (is (equal '(0 1 2)
               (take-while f (range 100))))))

(test partition-by-test
  (is (null (partition-by #'evenp ())))
  (is (equal '((0) (1) (2))
             (partition-by #'evenp
                           (range 3))))
  (is (equal '((0) (1) (2))
             (partition-by #'oddp
                           (range 3))))
  (is (equal '((a a a) (b) (c c c) (b) (a a a))
             (partition-by
              (lambda (x) (eq 'b x))
              '(a a a b c c c b a a a)))))

(test str-test
  (is (equal "" (str)))
  (is (equal "1" (str 1)))
  (is (equal "abc" (str "a" "bc"))))

(test not=-test
  (is (not= 0 1))
  (is (not (not= 1 1)))
  (is (not= 'a 'b))
  (is (not (not= 'a 'a)))
  (is (not (not= '(1 (2 3)) (list 1 (list (+ 1 1) 3)))))
  (is (not= '(1 (2 3)) (list 1 (list (+ 1 1) 1)))))

(test if-not-test
  (is (equal t (if-not nil t (/ 1 0))))
  (is (equal t (if-not nil t)))
  (is (equal 'foo (if-not t (/ 1 0) 'foo))))

(test when-not-test
  (is (not (when-not t 3 4)))
  (is (= 4 (when-not () 3 4))))

(test if-let-test
  (is (equal 1 (if-let (x (car '(1 2 3)))
                 x
                 2)))
  (is (equal '(42 38)
             (if-let (x 42)
               (if-let (y 38)
                 (list x y)
                 :nope)
               :nope)))
  (is (equal '(42 38)
             ;; From Fogus:
             (if-let (x 42)
               (if-let (y 38)
                 (list x y)))))
  (is (equal :nope
             (if-let (x nil)
               3
               :nope)))
  (is (null (if-let (x nil)
              3))))

(test repeat-test
  (is (equal () (repeat 0 '*)))
  (is (equal '(* * *) (repeat 3 '*))))

(test repeatedly-test
  (is (equal () (repeatedly 0 (constantly '*))))
  (is (equal '(* * *) (repeatedly 3 (constantly '*)))))

(test with-out-str-test
  (is (equal "3" (with-out-str
                   (clj-print "3")))))

(test distinct-test
  (is (equal () (distinct ())))
  (is (equal '(1) (distinct '(1))))
  (is (equal '(1) (distinct '(1 1 1))))
  (is (equal '(a b c) (distinct '(a b a c a b a b b)))))
