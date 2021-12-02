(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../common/common.lisp"))

(use-package :advent)

(defun parse-line (s)
  (with-input-from-string (f s)
    (list (read f) (read f))))

(defun forward (n pos depth aim)
  (if aim
      (values (+ pos n) (+ depth (* aim n)) aim)
    (values (+ pos n) depth)))

(defun down (n pos depth aim)
  (if aim
      (values pos depth (+ aim n))
    (values pos (+ depth n))))

(defun up (n pos depth aim)
  (if aim
      (values pos depth (- aim n))
    (values pos (- depth n))))

(defun move (lines &optional aim)
  (let ((pos 0)
        (depth 0))
    (loop
       for cmd in lines
       do (multiple-value-bind (npos ndepth naim)
              (funcall (first cmd) (second cmd) pos depth aim)
            (setf pos npos)
            (setf depth ndepth)
            (setf aim naim)))
    (* pos depth)))

(defun part-1 (&optional (data #'test-data))
  (move (funcall data #'parse-line)))

(defun part-2 (&optional (data #'test-data))
  (move (funcall data #'parse-line) 0))
