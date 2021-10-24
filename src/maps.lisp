(in-package :cl-oju)

;; Incomplete implementation of a small translation layer which allows
;; one to represent hash maps with literal lists.  Do not use yet.

(defun as-maplists (x)
  (cond ((hash-table-p x)
         (loop for k being each hash-key of x
                 using (hash-value v)
               with ret = (list 'map)
               do (nconc ret (list k v))
               finally (return ret)))
        ((listp x) (mapcar #'as-maplists x))
        (t x)))

(defun as-maps (x)
  (cond
    ((atom x) x)
    ((eq (car x) 'MAP)
     (loop for (k v) in (partition-all 2 2 (cdr x))
           with ret = (make-hash-table)
           do (setf (gethash k ret) v)
           finally (return ret)))))

(comment
 (as-maps '(MAP :a 1 :b 2))
 ;;=>
 ;;'#<HASH-TABLE :TEST EQL :COUNT 2 {7014A8A803}>

 (as-maplists (as-maps '(MAP :a 1 :b 2)))
 ;;=>
 '(MAP :A 1 :B 2))
