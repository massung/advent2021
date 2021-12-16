(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-graph (lines)
  (let ((edges (make-hash-table :test 'equal)))
    (flet ((add-edge (from to)
             (if (gethash from edges)
                 (push to (gethash from edges))
               (setf (gethash from edges) (list to)))))
      (dolist (line lines edges)
        (re:with-re-match (m (re:match-re "([^-]+)%-(.+)" line))
          (add-edge $1 $2)
          (add-edge $2 $1))))))

(defun find-paths (graph &key allow-double-back-p)
  (labels ((discover (path double-backed-p)
             (if (string= (car path) "end")
                 (list path)
               (loop
                  for x in (gethash (car path) graph)
                  for small = (lower-case-p (char x 0))
                  for visited = (member x path :test 'equal)
                  when (and (or (not small)
                                (not visited)
                                (not double-backed-p))
                            (string/= x "start"))
                  append (discover (cons x path)
                                   (or double-backed-p (and visited small)))))))
    (discover (list "start") (not allow-double-back-p))))

(defun part-1 (&optional (data #'test-data))
  (let ((graph (parse-graph (funcall data))))
    (time (length (find-paths graph)))))

(defun part-2 (&optional (data #'test-data))
  (let ((graph (parse-graph (funcall data))))
    (time (length (find-paths graph :allow-double-back-p t)))))