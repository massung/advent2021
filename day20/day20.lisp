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
           for bit = (if (char= c #\#) 1 0)
           do (setf (gethash (cons x y) image) bit))
     finally (return image)))

(defun parse-input (lines)
  (values (pop lines)
          (parse-image (rest lines))
          (length (third lines))
          (length (rest lines))))

(defun enhance-pixel (input iea x y default)
  (flet ((pixel (x y)
           (gethash (cons x y) input default)))
    (let ((n (logior (ash (pixel (1- x) (1- y)) 8)
                     (ash (pixel     x  (1- y)) 7)
                     (ash (pixel (1+ x) (1- y)) 6)
                     (ash (pixel (1- x)     y)  5)
                     (ash (pixel     x      y)  4)
                     (ash (pixel (1+ x)     y)  3)
                     (ash (pixel (1- x) (1+ y)) 2)
                     (ash (pixel     x  (1+ y)) 1)
                     (ash (pixel (1+ x) (1+ y)) 0))))
      (if (char= (char iea n) #\#) 1 0))))

(defun enhance (input iea width height default)
  (loop
     with output = (make-hash-table :test 'equal)
     for y from -1 to height
     do (loop
           for x from -1 to width
           for bit = (enhance-pixel input iea x y default)
           do (setf (gethash (cons (1+ x) (1+ y)) output) bit))
     finally (return output)))

(defun print-image (image width height)
  (terpri)
  (loop
     for y below height
     do (loop
           for x below width
           for bit = (gethash (cons x y) image)
           do (write-char (if (plusp bit) #\# #\.))
           finally (terpri))
     finally (terpri)))

(defun run (data n)
  (multiple-value-bind (iea image width height)
      (parse-input (funcall data))
    (time (let ((default 0))
            (dotimes (i n)
              (setf image (enhance image iea width height default))

              ;; increase the size of the image
              (incf width 2)
              (incf height 2)

              ;; this is stupid - toggle the default between first and last bit of iea
              (setf default (let ((c (char iea (if (zerop default) 0 511))))
                              (if (char= c #\#) 1 0))))
            (loop for bit being the hash-values of image sum bit)))))

(defun part-1 (&optional (data #'test-data))
  (run data 2))

(defun part-2 (&optional (data #'test-data))
  (run data 50))
