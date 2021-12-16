(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp")

  ;; additional libraries
  (require :pathfind))

(defun build-map (lines)
  (flet ((parse-row (line)
           (map 'vector #'digit-char-p line)))
    (map 'vector #'parse-row lines)))

(defun expand-row (row inc)
  (apply #'concatenate 'vector (loop
                                  for k below 5
                                  collect (map 'list #'(lambda (x)
                                                         (let ((nx (+ x inc k)))
                                                           (if (< nx 10)
                                                               nx
                                                             (1+ (rem nx 10)))))
                                               row))))

(defun tile-map (m)
  (let* ((h (length m))
         (rows (loop
                  for y below (* h 5)
                  collect (multiple-value-bind (q r)
                              (truncate y h)
                            (expand-row (aref m r) q)))))
    (coerce rows 'vector)))

(defun end-pos (m)
  (list (1- (length (aref m 0))) (1- (length m))))

(defun find-path (m x1 y1 x2 y2)
  (flet ((edges (pos)
           (destructuring-bind (x y)
               pos
             (loop
                for (dx dy) in '((1 0) (0 1))
                for nx = (+ x dx)
                for ny = (+ y dy)
                when (and (<= 0 nx x2) (<= 0 ny y2))
                collect `((,nx ,ny) ,(aref (aref m ny) nx))))))

    ;; preform the pathfind
    (pf:pathfind (list x1 y1) (list x2 y2) #'edges)))

(defun part-1 (&optional (data #'test-data))
  (let ((m (build-map (funcall data))))
    (time (apply #'find-path m 0 0 (end-pos m)))))

(defun part-2 (&optional (data #'test-data))
  (let ((m (tile-map (build-map (funcall data)))))
    (time (apply #'find-path m 0 0 (end-pos m)))))
