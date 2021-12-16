(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp")

  ;; additional libraries
  (require :pathfind))

(defun build-matrix (lines)
  (loop
     with w = (length (first lines))
     for line in lines
     for h from 1
     collect (map 'list #'digit-char-p line) into grid
     finally (return (make-array (list w h) :initial-contents grid))))

(defun get-cost (m x y)
  (destructuring-bind (w h)
      (array-dimensions m)
    (multiple-value-bind (qx rx) (truncate x w)
      (multiple-value-bind (qy ry) (truncate y h)
        (let ((cost (+ (aref m ry rx) qx qy)))
          (if (< cost 10)
              cost
            (- cost 9)))))))

(defun find-path (m x1 y1 x2 y2)
  (flet ((edges (pos)
           (destructuring-bind (x y) pos
             (loop
                for (dx dy) in '((1 0) (0 1))
                for nx = (+ x dx)
                for ny = (+ y dy)
                when (and (<= 0 nx x2) (<= 0 ny y2))
                collect `((,nx ,ny) ,(get-cost m nx ny))))))

    ;; preform the pathfind
    (pf:pathfind (list x1 y1) (list x2 y2) #'edges)))

(defun run (m &optional (dim 1))
  (destructuring-bind (w h)
      (array-dimensions m)
    (find-path m 0 0 (1- (* w dim)) (1- (* h dim)))))

(defun part-1 (&optional (data #'test-data))
  (time (run (build-matrix (funcall data)))))

(defun part-2 (&optional (data #'test-data))
  (time (run (build-matrix (funcall data)) 5)))
