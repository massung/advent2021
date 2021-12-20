(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-image (lines)
  (loop
     with image = (make-hash-table :test 'equal)
     for y from 0
     for line in lines
     do (loop
           for x from 0
           for c across line
           when (char= c #\#)
           do (setf (gethash (cons x y) image) 1))
     finally (return image)))

(defun parse-input (lines)
  (values (pop lines)
          (parse-image (rest lines))
          (length (third lines))
          (length (rest lines))))

(defun enhance-pixel (input iea x y)
  (flet ((pixel (x y)
           (gethash (cons x y) input 0)))
    (let ((n (logior (ash (pixel (1- x) (1- y)) 8)
                     (ash (pixel     x  (1- y)) 7)
                     (ash (pixel (1+ x) (1- y)) 6)
                     (ash (pixel (1- x)     y)  5)
                     (ash (pixel     x      y)  4)
                     (ash (pixel (1+ x)     y)  3)
                     (ash (pixel (1- x) (1+ y)) 2)
                     (ash (pixel     x  (1+ y)) 1)
                     (ash (pixel (1+ x) (1+ y)) 0))))
      (char= (char iea n) #\#))))

(defun enhance (input iea width height)
  (loop
     with output = (make-hash-table :test 'equal)
     for y from -1 to height
     do (loop
           for x from -1 to width
           for lit = (enhance-pixel input iea x y)
           when lit
           do (setf (gethash (cons x y) output) 1))
     finally (return output))))

(defun print-image (image width height)
  (loop
     for y below height
     do (loop
           for x below width
           do (write-char (if (gethash (cons x y) image) #\# #\.))
           finally (terpri))
     finally (terpri)))

(defun part-1 (&optional (data #'test-data))
  (multiple-value-bind (iea image width height)
      (parse-input (funcall data))
    (time (dotimes (i 2 (hash-table-count image))
            (setf image (enhance image iea width height))
            (incf width 2)
            (incf height 2)
            (print-image image width height)))))


#|
(defun parse-image (lines)
  (flet ((parse-bits (s)
           (let ((r (map 'string #'(lambda (c) (if (char= c #\.) #\0 #\1)) s)))
             (parse-integer r :radix 2))))
    (map 'vector #'parse-bits lines)))

(defun read-row-bits (input x y width)
  (if (or (< y 0) (>= y (length input)))
      0
    (let ((b (- width 2 x)))
      (case b
        ((-2) (ash (ldb (byte 1 0) (aref input y)) 2))
        ((-1) (ash (ldb (byte 2 0) (aref input y)) 1))
        (otherwise (ldb (byte 3 b) (aref input y)))))))

(defun enhance (input x y width)
  (logior (ash (read-row-bits input x (1- y) width) 6)
          (ash (read-row-bits input x y width) 3)
          (read-row-bits input x (1+ y) width)))

(defun process-image (input iea width)
  (loop
     with output = (make-array 0 :adjustable t :fill-pointer 0)
     for y from -1 to (length input)
     do (let ((row (loop
                      with i = 0
                      for x from -1 to width
                      for n = (enhance input x y width)
                      for bit = (if (char= (char iea n) #\#) 1 0)
                      do (setf i (logior (ash i 1) bit))
                      finally (return i))))
          (vector-push-extend row output))
     finally (return (values output (+ width 2)))))

(defun print-image (output width)
  (terpri)
  (loop
     for row across output
     do (loop
           for x from (1- width) downto 0
           for bit = (ldb (byte 1 x) row)
           do (write-char (if (zerop bit) #\. #\#))
           finally (terpri))))

(defun count-lit (image)
  (flet ((count-bits (n)
           (do ((i 0))
               ((zerop n) i)
             (incf i (logand n 1))
             (setf n (ash n -1)))))
    (loop for n across image sum (count-bits n))))

(defun part-1 (&optional (data #'test-data))
  (multiple-value-bind (iea image width)
      (parse-input (funcall data))
    (time (dotimes (i 2 (count-lit image))
            (setf (values image width) (process-image image iea width))
            (when (eq data #'test-data)
              (print-image image width))))))

(defun part-2 (&optional (data #'test-data))
  (let ((lines (funcall data #'read-from-string)))
    (time nil)))
|#
