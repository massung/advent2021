(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defstruct board ns marks)

(defparameter *bingos*
  (list #*1111100000000000000000000
        #*0000011111000000000000000
        #*0000000000111110000000000
        #*0000000000000001111100000
        #*0000000000000000000011111
        #*1000010000100001000010000
        #*0100001000010000100001000
        #*0010000100001000010000100
        #*0001000010000100001000010
        #*0000100001000010000100001))

(defun bingop (marks)
  (some #'(lambda (b) (equal (bit-and b marks) b)) *bingos*))

(defun read-board (s)
  (let ((hash (make-hash-table))
        (marks (make-array 25 :element-type 'bit)))
    (dotimes (i 25 (make-board :ns hash :marks marks))
      (setf (gethash (read s) hash) i))))

(defun read-calls (s)
  (read-numbers (read-line s) :sep ","))

(defun read-input (fn)
  (with-open-file (s (format nil "~a/~a" (puzzle-pathname 4) fn))
    (let ((calls (read-calls s))
          (boards (loop while (read-line s nil) collect (read-board s))))
      (values calls boards))))

(defun place-mark (board call)
  (let ((i (gethash call (board-ns board))))
    (when i
      (setf (bit (board-marks board) i) 1)
      (bingop (board-marks board)))))

(defun unmarked (board)
  (loop
     with b = (board-ns board)
     for k being the hash-keys of b
     for mark = (bit (board-marks board) (gethash k b))
     when (zerop mark)
     sum k))

(defun play (calls boards)
  (dolist (call calls)
    (dolist (board boards)
      (when (place-mark board call)
        (return-from play (* (unmarked board) call))))))

(defun find-last (calls boards)
  (if (null (second boards))
      (play calls boards)
    (let ((call (pop calls)))
      (find-last calls (remove-if #'(lambda (b) (place-mark b call)) boards)))))

(defun part-1 (&optional (fn "test.txt"))
  (multiple-value-bind (calls boards)
      (read-input fn)
    (time (play calls boards))))

(defun part-2 (&optional (fn "test.txt"))
  (multiple-value-bind (calls boards)
      (read-input fn)
    (time (find-last calls boards))))
