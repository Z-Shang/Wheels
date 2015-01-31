(defpackage #:cl-wheels-logic
  (:use :cl))

(in-package #:cl-wheels-logic)

(defun mix-two-lists (lsta &optional (lstb nil))
  (if (null lstb)
      lsta
      (let ((r '()))
        (loop for o in lsta
           do (loop for a in lstb
                 do (push
                     (if (listp a)
                         (cons o a)
                         (list o a))
                     r)))
        r)))

(defun cartesian-product (&rest lsts)
  (reduce 'mix-two-lists lsts :initial-value nil :from-end t))

(defun l-not (sym)
  (cond
    ((equal sym 't)
     'f)
    ((equal sym 'f)
     't)))

(defun l-and (syma symb)
  (if (and (equal syma 't)
           (equal symb 't))
      't
      'f))

(defun l-or (syma symb)
  (if (or (equal syma 't)
          (equal symb 't))
      't))

(defun )
