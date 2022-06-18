(in-package #:cl-oju-tests)

(defun run-tests () (1am:run))

(test comment
  (is (null (cl-oju:comment)))
  (is (null (cl-oju:comment ())))
  (is (null (cl-oju:comment (a b c))))
  (is (null (cl-oju:comment (/ 1 0)))))

(test range
  (is (equal '(0 1 2) (cl-oju:range 3)))
  (is (equal '(3 4 5) (cl-oju:range 3 6)))
  (is (equal 10000 (length (cl-oju:range 10000)))))

(test take
  (is (equal (cl-oju:take 9 "this is a test")
             '(#\t #\h #\i #\s #\  #\i #\s #\  #\a))))

(test drop
  (is (equal (cl-oju:drop 3 (cl-oju:range 10))
             '(3 4 5 6 7 8 9))))

(test interpose
  (is (equal (cl-oju:interpose :sep nil) nil))
  (is (equal (cl-oju:interpose :sep '(1)) '(1)))
  (is (equal (cl-oju:interpose :sep '(1 2 3)) '(1 :SEP 2 :SEP 3))))

(test interleave
  (is (equal (cl-oju:interleave '(1 2 3) '(:a :b :c))
             '(1 :a 2 :b 3 :c))))

(test partition-all
  (is (equal (cl-oju:partition-all 2 2 '(a b c d))
             '((a b) (c d))))
  (is (equal (cl-oju:partition-all 2 1 '(a b c d))
             '((a b) (b c) (c d) (d)))))

(test partition-n
  (is (equal (cl-oju:partition-n 2 2 '(a b c d))
             '((a b) (c d))))
  (is (equal (cl-oju:partition-n 2 1 '(a b c d))
             '((a b) (b c) (c d)))))
(test juxt
  (is (equal (funcall (cl-oju:juxt #'car #'cdr) '(1 2 3))
             '(1 (2 3)))))

(test sort-by
  (is (equal (cl-oju:sort-by #'car '((2 3) (1 1)))
             '((1 1) (2 3))))
  (is (equal (cl-oju:sort-by (lambda (x) (parse-integer (car x)))
                             '(("2" 3) ("1" 1)))
             '(("1" 1) ("2" 3))))
  (is (equal (cl-oju:sort-by #'first
                             '(("b" 3) ("a" 1)))
             '(("a" 1) ("b" 3))))
  (is (equal (cl-oju:sort-by
              (cl-oju:juxt #'first #'second)
              '((1 0) (0 0)))
             '((0 0) (1 0))))
  (is (equal (cl-oju:sort-by
              (cl-oju:juxt
               (lambda (a) (second (assoc 'date a :test #'string=)))
               (lambda (a) (second (assoc 'account a :test #'string=))))
              '(((date "2020-01-02")
                (account "aaa"))
               ((date "2020-01-01")
                (account "aaa"))))
             '(((date "2020-01-01")
                (account "aaa"))
               ((date "2020-01-02")
                (account "aaa")))))
  (is (equal (cl-oju:sort-by
              (cl-oju:juxt
               (lambda (a) (second (assoc 'date a :test #'string=)))
               (lambda (a) (second (assoc 'account a :test #'string=))))
              '(((date "2020-01-01")
                (account "bbb"))
               ((date "2020-01-01")
                (account "aaa"))))
             '(((date "2020-01-01")
                (account "aaa"))
               ((date "2020-01-01")
                (account "bbb"))))))


(test group-by
  (is (equal (cl-oju:group-by #'evenp '(1 2 3 4 5 6))
             '((NIL (5 3 1)) (T (6 4 2)))))
  (is (equal (cl-oju:group-by (lambda (x) (nth 5 x))
                              '((1 _ _ _ _ "x")
                                (2 _ _ _ _ "y")
                                (3 _ _ _ _ "x")
                                (4 _ _ _ _ "y")
                                (5 _ _ _ _ "x")
                                (6 _ _ _ _ "y")))
             '(("x" ((5 _ _ _ _ "x") (3 _ _ _ _ "x") (1 _ _ _ _ "x")))
               ("y" ((6 _ _ _ _ "y") (4 _ _ _ _ "y") (2 _ _ _ _ "y")))))))

(test partial
  (is (equal (funcall (cl-oju:partial #'+)) 0))
  (is (equal (funcall (cl-oju:partial #'* 2) 3) 6)))

(test pos-neg-test
  (is (cl-oju:pos? 1))
  (is (not (cl-oju:pos? 0)))
  (is (not (cl-oju:pos? -1)))
  (is (cl-oju:neg? -1))
  (is (not (cl-oju:neg? 0)))
  (is (not (cl-oju:neg? 1))))

(test filter
  (is (null ()))
  (is (equal (cl-oju:range 3)
             (cl-oju:range 3)))
  (is (null (cl-oju:filter (constantly nil)
                           (cl-oju:range 10))))
  (is (equal (cl-oju:range 10)
             (cl-oju:filter (constantly t)
                            (cl-oju:range 10))))
  (is (equal '(1 3 5) (cl-oju:filter #'oddp (cl-oju:range 6)))))

(test take-while
  (is (null (cl-oju:take-while #'evenp ())))
  (is (equal '(0)
             (cl-oju:take-while #'evenp (cl-oju:range 3))))
  (let ((f (lambda (x) (< x 3))))
    (is (equal '(0 1 2)
               (cl-oju:take-while f (cl-oju:range 100))))))

(test partition-by
  (is (null (cl-oju:partition-by #'evenp ())))
  (is (equal '((0) (1) (2))
             (cl-oju:partition-by #'evenp
                                  (cl-oju:range 3))))
  (is (equal '((0) (1) (2))
             (cl-oju:partition-by #'oddp
                                  (cl-oju:range 3))))
  (is (equal '((a a a) (b) (c c c) (b) (a a a))
             (cl-oju:partition-by
              (lambda (x) (eq 'b x))
              '(a a a b c c c b a a a)))))

(test str
  (is (equal "" (cl-oju:str)))
  (is (equal "1" (cl-oju:str 1)))
  (is (equal "abc" (cl-oju:str "a" "bc"))))
