(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-coords (lines)
  (let ((coords (loop
                   for line = (pop lines)
                   while (plusp (length line))
                   collect (re:with-re-match (m (re:match-re "(%d+),(%d+)" line))
                             (cons (parse-integer $1) (parse-integer $2))))))
    (values coords lines)))

(defun parse-folds (lines)
  (loop
     for line in lines
     collect (re:with-re-match (m (re:find-re "(%a)=(%d+)" line))
               (list (char $1 0) (parse-integer $2)))))

(defun parse-input (lines)
  (multiple-value-bind (coords rest)
      (parse-coords lines)
    (values coords (parse-folds rest))))

(defun empty-matrix (w h)
  (flet ((make-row ()
           (make-array w :element-type 'bit)))
    (make-array h :initial-contents (loop for i below h collect (make-row)))))

(defun build-matrix (coords)
  (let ((max-x (1+ (apply #'max (mapcar #'car coords))))
        (max-y (1+ (apply #'max (mapcar #'cdr coords)))))
    (loop
       with m = (empty-matrix max-x max-y)
       for coord in coords
       for x = (car coord)
       for y = (cdr coord)
       do (setf (bit (aref m y) x) 1)
       finally (return m))))

(defun fold-x (m pos)
  (loop
     with folded = (make-array (length m))
     for y from 0
     for row across m
     do (let ((left (subseq row 0 pos))
              (right (subseq row (1+ pos))))
          (setf (aref folded y) (bit-ior left (reverse right))))
     finally (return folded)))

(defun fold-y (m pos)
  (loop
     with folded = (make-array pos)
     for y from (1- pos) downto 0
     for opposite from (1+ pos)
     for row = (aref m y)
     do (setf (aref folded y)
              (if (< opposite (length m))
                  (bit-ior row (aref m opposite))
                (copy-seq row)))
     finally (return folded)))

(defun fold-matrix (m folds)
  (flet ((fold (m fold)
           (destructuring-bind (axis pos)
               fold
             (case axis
               ((#\x) (fold-x m pos))
               ((#\y) (fold-y m pos))))))
    (reduce #'fold folds :initial-value m)))

(defun part-1 (&optional (data #'test-data))
  (multiple-value-bind (coords folds)
      (parse-input (funcall data))
    (time (let ((m (fold-matrix (build-matrix coords) (subseq folds 0 1))))
            (loop for v across m sum (loop for b across v sum b))))))

(defun part-2 (&optional (data #'test-data))
  (multiple-value-bind (coords folds)
      (parse-input (funcall data))
    (time (loop
             for bits across (fold-matrix (build-matrix coords) folds)
             do (print bits)))))
