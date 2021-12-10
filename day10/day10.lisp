(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun corrupted-score (c)
  (case c
    ((#\)) 3)
    ((#\]) 57)
    ((#\}) 1197)
    ((#\>) 25137)))

(defun incomplete-score (c)
  (case c
    ((#\() 1)
    ((#\[) 2)
    ((#\{) 3)
    ((#\<) 4)))

(defun parse-chunks (s)
  (loop
     with stack = nil
     for c across s
     do (if (member c '(#\[ #\( #\{ #\<))
            (push c stack)
          (unless (case (pop stack)
                    ((#\() (char= c #\)))
                    ((#\[) (char= c #\]))
                    ((#\{) (char= c #\}))
                    ((#\<) (char= c #\>)))
            (return-from parse-chunks (corrupted-score c))))
     finally (return stack)))

(defun autocomplete (stack)
  (loop
     with score = 0
     for top in stack
     do (setf score (+ (* score 5) (incomplete-score top)))
     finally (return score)))

(defun part-1 (&optional (data #'test-data))
  (time (let ((errors (funcall data #'parse-chunks)))
          (reduce #'+ (remove-if-not #'numberp errors)))))

(defun part-2 (&optional (data #'test-data))
  (time (let* ((chunks (funcall data #'parse-chunks))
               (incomplete (remove-if #'numberp chunks))
               (scores (map 'vector #'autocomplete incomplete)))
          (aref (sort scores #'<) (truncate (length scores) 2)))))
