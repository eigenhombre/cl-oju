(defpackage cl-oju
  (:use :cl :trivialtests)
  (:export :slurp
           :spit
           :take
           :drop
           :frequencies
           :interpose
           :partition-all
           :rand-nth
           :rand-int
           :range
           :repeatedly))
(in-package :cl-oju)

(defun slurp (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun spit (name s)
  (with-open-file (stream name
                          :direction :output
                          :if-exists :supersede)
    (write-string s stream)))

(defun range (n)
  (loop for x upto (1- n) collect x))

(defun take (n l)
  (loop for x in l repeat n collect x))

(defun drop (n l)
  (nthcdr n l))

(dotests
 (test= (drop 3 (range 10))
        '(3 4 5 6 7 8 9)))

(defun rand-nth (l)
  (nth (random (length l)) l))

(defun interpose (sep coll)
  (cdr (loop for x in coll append (list sep x))))

(dotests
 (test= (interpose :sep nil) nil)
 (test= (interpose :sep '(1)) '(1))
 (test= (interpose :sep '(1 2 3)) '(1 :SEP 2 :SEP 3)))

(defun partition-all (cell-size step-size lst)
  (loop for cell on lst
     by #'(lambda (lst1) (nthcdr step-size lst1))
     collecting (take cell-size cell)))

(dotests
 (test= (partition-all 2 2 '(a b c d))
        '((a b) (c d)))
 (test= (partition-all 2 1 '(a b c d))
        '((a b) (b c) (c d) (d))))

;; Adapted from https://codereview.stackexchange.com/a/223128:
(defun frequencies (lst)
  (let ((m (make-hash-table :test #'equalp)))
    (loop for e in lst
       do (incf (gethash e m 0)))
    (loop for k being the hash-key of m
       using (hash-value v)
       collect (list k v))))

(defun rand-int (n) (random n))

(defun repeatedly (n f)
  (loop repeat n collect (funcall f)))
