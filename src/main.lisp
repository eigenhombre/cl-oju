(in-package :cl-oju)

(defun str (&rest args)
  (format nil "~{~a~}" args))

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

(defun range (n &optional m)
  (if m
      (loop for x from (the fixnum n) below (the fixnum m)
            collect x)
      (loop for x upto (1- (the fixnum n)) collect x)))

(defun take (n l)
  (if (stringp l)
      (loop for x across l repeat (the fixnum n) collect x)
      (loop for x in l repeat (the fixnum n) collect x)))

(defun drop (n l)
  (nthcdr n l))

(defun fast-length (coll)
  ;; Is this a good idea?
  (if (vectorp coll)
      (array-dimension coll 0)
      (length coll)))

(defun rand-nth (l)
  ;; Not sure how to optimize this: I want to support both lists and
  ;; arrays:
  (if (vectorp l)
      (aref l (random (fast-length l)))
      (nth (random (fast-length l)) l)))

(defun interpose (sep coll)
  ;;(cdr (loop for x in coll append (list sep x)))
  ;; Really nice solution, adapted from `coredump`
  ;; (https://stackoverflow.com/questions/58093923/\
  ;; how-can-i-paste-a-element-between-all-elements-of-a-list):
  (loop
    for (x . xs) on coll
    collect x
    when xs collect sep))

(defun interleave (lst1 lst2)
  (loop with ret
        for l1 in lst1
        for l2 in lst2
        do
           (push l1 ret)
           (push l2 ret)
        finally (return (nreverse ret))))

(defun partition-all (cell-size step-size lst)
  (loop for cell on lst
        by #'(lambda (lst1) (nthcdr step-size lst1))
        collect (take cell-size cell)))

(defun clj-partition (cell-size step-size lst)
  (let ((ret nil))
    (loop for cell on lst
          by #'(lambda (lst1) (nthcdr step-size lst1))
          do (let ((this (take cell-size cell)))
               (when (= (length (the list this))
                        (the fixnum cell-size))
                 (push (take cell-size cell) ret))))
    (nreverse ret)))

;; The above used to be called `partition-n`:
(defun partition-n (cell-size step-size lst)
  (clj-partition cell-size step-size lst))

;; Adapted from https://codereview.stackexchange.com/a/223128:
(defun frequencies (lst)
  (let ((m (make-hash-table :test #'equalp)))
    (loop for e in lst
          do (incf (the fixnum (gethash e m 0))))
    (loop for k being the hash-key of m
            using (hash-value v)
          collect (list k v))))

(defun rand-int (n) (random (the fixnum n)))

(defun repeatedly (n f)
  (loop repeat (the fixnum n)
        collect (funcall (the function f))))

;; PG ANSI Common Lisp p. 110 /
;; https://stackoverflow.com/questions/19424954/compose-in-common-lisp
(defun comp (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun juxt (&rest fs)
  (lambda (x)
    (loop for f in fs collect (funcall
                               (the function f) x))))

(defmacro comment (&rest ign)
  (declare (ignore ign)))

;; Clojure's `sort-by` behaves a bit differently than CL's `sort`.
;; `sort-by` uses `compare`, which, like java.util.Comparable returns
;; positive, negative or zero integer rather than `t` (for strictly
;; less than) vs. `nil` (otherwise).
;; Thanks to @dsletten (David Sletten) for suggesting the following
;; code / multimethod approach:
(defgeneric compare< (a b)
  (:documentation "Determine whether A is less than B in the appropriate sense."))
(defmethod compare< ((a number) (b number)) (< a b))
(defmethod compare< ((a string) (b string)) (string< a b))
(defmethod compare< ((a symbol) (b symbol)) (compare< (symbol-name a) (symbol-name b)))
(defmethod compare< ((a character) (b character)) (char< a b))
(defmethod compare< ((a cons) (b cons))
  (or (compare< (car a) (car b))
      (and (not (compare< (car b) (car a)))
           (compare< (cdr a) (cdr b)))))
(defmethod compare< ((a cons) (b null)) nil)
(defmethod compare< ((a null) (b cons)) t)
(defmethod compare< ((a null) (b null)) nil)

(defun sort-by (fn coll)
  (sort (copy-seq coll) #'compare< :key fn))

(defun group-by (fn coll)
  (let ((ret (make-hash-table :test #'equal)))
    (loop for item in coll do
      (let ((k (funcall fn item)))
        (push item (gethash k ret))))
    (loop for k being the hash-keys in ret using (hash-value v)
          collect (list k v))))


(defun partial (fun &rest xs)
  (lambda (&rest inner-xs)
    (apply fun
           (append xs inner-xs))))

(defun pos? (n) (< 0 n))
(defun neg? (n) (< n 0))

;; You should just use `remove-if-not`!
(defun filter (f l)
  (remove-if-not f l))

(defun take-while (f l)
  (loop for x in l
        with ret
        do (progn
             (when (not (funcall f x))
               (return (nreverse ret)))
             (setq ret (cons x ret)))
        finally (return l)))

(defun partition-by (f l)
  (when l
    (let* ((fst (first l))
           (fv (funcall f fst))
           (run (cons fst
                      (take-while (lambda (x)
                                    (equal fv (funcall f x)))
                                  (rest l)))))
      (cons run (partition-by f (nthcdr (length run) l))))))

;; Real Lispers are going to hate me for this:
(defun not= (a b) (not (equal a b)))

(defmacro if-not (pred then &optional else)
  `(if (not ,pred)
       ,then
       ,else))

(defmacro when-not (pred &rest body)
  `(when (not ,pred)
     ,@body))


;; Possibly crude implementation, will revisit as needed:
(defun print-clj (&rest args)
  (loop for arg in args
        for first = t then nil
        do (when (not first)
             (princ " "))
           (format t "~a" arg)))

(defun println (&rest args)
  (apply print-clj args)
  (format t "~%"))
