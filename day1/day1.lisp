(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun steps (depths)
  (let ((i (car depths)))
    (loop for d in (cdr depths)
    collect (prog1
          (- d i)
        (setf i d)))))

(defun part-1 (&optional (data #'test-data))
  (let ((lines (funcall data #'read-from-string)))
    (time (count-if #'plusp (steps lines)))))

(defun part-2 (&optional (data #'test-data))
  (let* ((lines (funcall data #'read-from-string))
         (windows (loop
                     for x = (pop lines)
                     for y = (first lines)
                     for z = (second lines)
                     while (and x y z)
                     collect (+ x y z))))
    (time (count-if #'plusp (steps windows)))))
