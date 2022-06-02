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

(defun range (n &optional m)
  (if m
      (loop for x from (the fixnum n) upto (1- (the fixnum m))
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
  (let ((ret))
    (loop
      for l1 in lst1
      for l2 in lst2
      do (setf ret (cons l2 (cons l1 ret))))
    (nreverse ret)))

(defun partition-all (cell-size step-size lst)
  (loop for cell on lst
        by #'(lambda (lst1) (nthcdr step-size lst1))
        collect (take cell-size cell)))

(defun partition-n (cell-size step-size lst)
  (let ((ret nil))
    (loop for cell on lst
          by #'(lambda (lst1) (nthcdr step-size lst1))
          do (let ((this (take cell-size cell)))
               (when (= (length (the list this))
                        (the fixnum cell-size))
                 (push (take cell-size cell) ret))))
    (nreverse ret)))

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

(defun juxt (&rest fs)
  (lambda (x)
    (loop for f in fs collect (funcall
                               (the function f) x))))

(defmacro comment (&rest ign)
  (declare (ignore ign)))

(defun cmp (a b)
  (cond ((and (numberp a) (numberp b)) (< a b))
        ((and (stringp a) (stringp b)) (string< a b))
        (t (error (format nil "Don't know how to compare ~a and ~a!"
                          a b)))))

(defun sort-by (fn coll)
  (sort (copy-seq coll) #'cmp :key fn))

(defun group-by (fn coll)
  (let ((ret (make-hash-table :test #'equal)))
    (loop for item in coll do
      (let+ ((k (funcall fn item))
             (existing (gethash k ret)))
        (setf (gethash k ret) (cons item existing))))
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
