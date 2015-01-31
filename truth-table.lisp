(defpackage #:cl-wheels-logic
  (:use :cl)
  (:export :land
           :lor
           :lnot
           :gen-truth-table))

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

(defun lnot (sym)
  (cond
    ((equal sym 't)
     'f)
    ((equal sym 'f)
     't)))

(defun land (&rest syms)
  (if (member 'f syms)
      'f
      't))

(defun lor (&rest syms)
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
  (if (listp l)
      (cond
        ((equal 'lor (car l))
         (format nil "~{~A~^+~}" (mapcar #'logic-to-string (cdr l))))
        ((equal 'land (car l))
         (format nil "~{~A~^*~}" (mapcar #'logic-to-string (cdr l))))
        ((equal 'lnot (car l))
         (format nil "(~A)'" (logic-to-string (cadr l)))))
      (cond
        ((equal l 't)
         "T")
        ((equal l 'f)
         "F")
        (t
         (symbol-name l)))))

(defun lookup (sym env)
  (if (null env)
      nil
      (if (equal sym (caar env))
          (cdr (car env))
          (lookup sym (cdr env)))))

(defun eval-logic (arglst exp)
  (if (atom exp)
      (lookup exp arglst)
      (let ((vlst
             (loop for a in (cdr exp)
                collect (eval-logic arglst a)))
            (f (car exp)))
        (apply f vlst))))

(defun count-minterm (lst)
  (let ((blst
         (loop for a in lst
            collect (if (equal a 't)
                        1
                        0))))
    (reduce (lambda (x y) (+ (* x 2) y)) blst)))

(defun gen-truth-table (args exps &optional (f nil))
  (let* ((alst (apply #'cartesian-product (make-list (length args) :initial-element '(t f))))
         (slst (sort
                (loop for l in alst
                   collect (list (count-minterm l)
                                 l
                                 (mapcar #'(lambda (x)
                                             (eval-logic
                                              (join-two-lists args l)
                                              x))
                                         exps)))
                #'(lambda (x y) (< (car x) (car y))))))
    (format *standard-output* "Min ~{~A~T~T~T~}=>~T~T~T~{~A~T~T~T~}~%"
            args
            (mapcar #'logic-to-string exps))
    (loop for l in slst
       do (format *standard-output* "~A~T~T~T~{~A~T~T~T~}=>~T~T~T~{~A~T~T~T~}~%"
                  (first l)
                  (second l)
                  (third l)))
    (if f
        (let ((flst
               (sort
                (loop for l in alst
                   collect (list (count-minterm l)
                                 l
                                 (eval-logic
                                  (join-two-lists args l) f)))
                #'(lambda (x y) (< (car x) (car y))))))
          (format *standard-output* "~%============~%")
          (format *standard-output* "Min ~{~A~T~T~T~}=>~T~T~T~A~T~T~T~%"
                  args
                  (logic-to-string f))
          (loop for l in flst
             do (format *standard-output*
                        "~A~T~T~T~{~A~T~T~T~}=>~T~T~T~A~T~T~T~%"
                        (first l)
                        (second l)
                        (third l)))
          (format *standard-output* "f(~{~A~^, ~}) = Sum(~{~A~^, ~})~%"
                  args
                  (loop for l in flst
                     when (equal 't (third l))
                     collect (first l)))))))

(provide 'cl-wheels-logic)
