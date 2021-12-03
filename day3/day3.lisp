(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../common/common.lisp"))

(defun count-bits (lines bit &aux (n0 0) (n1 0))
  (dolist (line lines (if (> n0 n1) 0 1))
    (if (char= (char line bit) #\0)
        (incf n0)
      (incf n1))))

(defun common-bit (lines i)
  (code-char (+ (count-bits lines i) (char-code #\0))))

(defun o2 (lines &optional (i 0))
  (if (null (second lines))
      (first lines)
    (let ((c (common-bit lines i)))
      (o2 (remove-if-not #'(lambda (s) (char= (char s i) c)) lines) (1+ i)))))

(defun co2 (lines &optional (i 0))
  (if (null (second lines))
      (first lines)
    (let ((c (common-bit lines i)))
      (co2 (remove-if #'(lambda (s) (char= (char s i) c)) lines) (1+ i)))))

(defun part-1 (&optional (data #'test-data))
  (let* ((lines (funcall data))
         (n (length (first lines)))
         (gamma 0))
    (dotimes (i n)
      (setf gamma (logior (ash gamma 1) (count-bits lines i))))
    (let ((epsilon (logand (lognot gamma) (1- (ash 1 n)))))
      (* gamma epsilon))))

(defun part-2 (&optional (data #'test-data))
  (let* ((lines (funcall data))
         (o2 (o2 lines))
         (co2 (co2 lines)))
    (* (parse-integer o2 :radix 2)
       (parse-integer co2 :radix 2))))
