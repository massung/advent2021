(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-ages (s)
  (mapcar #'parse-integer (re:split-re "," s :all t)))

(defun count-fish (ages days)
  (let ((lookup (make-hash-table :test 'equalp)))
    (labels ((count-fish-acc (age days)
               (multiple-value-bind (n okp)
                   (gethash (cons age days) lookup)
                 (if okp
                     n
                   (let ((n 0))
                     (do ((days-left (- days age 1) (- days-left 7)))
                         ((minusp days-left) n)
                       (incf n (1+ (count-fish-acc 8 days-left))))
                     (setf (gethash (cons age days) lookup) n))))))
      (loop for age in ages sum (1+ (count-fish-acc age days))))))

(defun part-1 (&optional (data #'test-data))
  (let ((ages (first (funcall data #'parse-ages))))
    (count-fish ages 18)))

(defun part-2 (&optional (data #'test-data))
  (let ((ages (first (funcall data #'parse-ages))))
    (count-fish ages 256)))
