(in-package #:cl-oju-tests)

(defun run-tests () (1am:run))

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
