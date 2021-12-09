(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun build-matrix (lines)
  (let ((cols (length (first lines))))
    (loop
       with m = (make-array (list (length lines) cols))
       for line in lines
       for row from 0
       do (loop
             for c across line
             for col from 0
             do (setf (aref m row col) (digit-char-p c)))
       finally (return m))))

(defun height (m row col &optional (default 10))
  (destructuring-bind (rows cols)
      (array-dimensions m)
    (or (and (< -1 row rows)
             (< -1 col cols)
             (aref m row col))
        default)))

(defun low-point-p (m row col)
  (let ((x (height m row col nil)))
    (values x (and (< x (height m (1- row) col))
                   (< x (height m (1+ row) col))
                   (< x (height m row (1- col)))
                   (< x (height m row (1+ col)))))))

(defun flood-fill-basin (m row col)
  (let ((positions `((,row ,col))))
    (flet ((fill-basin (nrow ncol)
             (when (< (height m nrow ncol) 9)
               (push (list nrow ncol) positions))))
      (loop
         with filled = nil
         for p = (pop positions)
         do (if (null p)
                (return-from flood-fill-basin (length filled))
              (unless (member p filled :test #'equalp)
                (destructuring-bind (row col)
                    p
                  (push p filled)
                  (fill-basin (1- row) col)
                  (fill-basin (1+ row) col)
                  (fill-basin row (1- col))
                  (fill-basin row (1+ col)))))))))

(defun collect-basins (m)
  (let ((basins nil))
    (destructuring-bind (rows cols)
        (array-dimensions m)
      (dotimes (row rows basins)
        (dotimes (col cols)
          (multiple-value-bind (x lowp)
              (low-point-p m row col)
            (when lowp
              (push (list x row col) basins))))))))

(defun part-1 (&optional (data #'test-data))
  (let ((m (build-matrix (funcall data))))
    (time (loop
             for basin in (collect-basins m)
             sum (destructuring-bind (x row col)
                     basin
                   (declare (ignore row col))
                   (1+ x))))))

(defun part-2 (&optional (data #'test-data))
  (let ((m (build-matrix (funcall data))))
    (time (let ((basin-sizes (mapcar #'(lambda (basin)
                                         (destructuring-bind (x row col)
                                             basin
                                           (declare (ignore x))
                                           (flood-fill-basin m row col)))
                                     (collect-basins m))))
            (reduce #'* (subseq (sort basin-sizes #'>) 0 3))))))
