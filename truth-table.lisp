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

(defun l-and (&rest syms)
  (if (member 'f syms)
      'f
      't))

(defun l-or (&rest syms)
  (if (member 't syms)
      't
      'f))

(defun join-two-lists (a b)
  (if (not (equal (length a)
                  (length b)))
      'ERR-LENGTH-NOT-EQUAL
      (if (null a)
          nil
          (cons (cons (car a) (car b)) (join-two-lists (cdr a) (cdr b))))))

(defun logic-to-string (l)
  (cond
    ((equal l 't)
     "T")
    ((equal l 'f)
     "F")
    ((equal 'l-or (car l))
     (format nil "~@{~A ~^+ ~}" (mapcar #'logic-to-string (cdr l))))
    ((equal 'l-and (car l))
     (format nil "~@{~A ~^* ~}" (mapcar #'logic-to-string (cdr l))))
    ((equal 'l-not (car l))
     (format nil "(~A)'" (logic-to-string (cadr l))))))

(defun lookup (sym env)
  (if (null env)
      nil
      (if (equal sym (caar env))
          (cdr (car env))
          (lookup sym (cdr env)))))

(defun eval-logic (arglst exp)
;  (format *standard-output* "~%~A~%~A~%" arglst exp)
  (if (atom exp)
      (lookup exp arglst)
      (let ((vlst
             (loop for a in (cdr exp)
                collect (eval-logic arglst a)))
            (f (car exp)))
        (apply f vlst))))

(defun gen-truth-table (args exps)
  (let ((alst (apply #'cartesian-product (make-list (length args) :initial-element '(t f)))))
    (format *standard-output* "~{~A~T~T~T~}=>~T~T~T~{~A~T~T~T~}~%" args exps)
    (loop for l in alst
       do (format *standard-output* "~{~A~T~T~T~}=>~T~T~T~{~A~T~T~T~}~%"
                  l
                  (mapcar #'(lambda (x) (eval-logic (join-two-lists args l)
                                               x)) exps)))))
