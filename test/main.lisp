(in-package #:cl-oju-tests)

(defun run-tests () (1am:run))

(test range
  (is (equalp '(0 1 2) (cl-oju::range 3))))
