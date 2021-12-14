(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defstruct pair char left right counts)

(defun parse-pairs (lines)
  (loop
     with pairs = (make-hash-table :test 'equal)
     for line in lines
     do (re:with-re-match (m (re:match-re "(%a%a) %-> (%a)" line))
          (setf (gethash $1 pairs)
                (make-pair :char (char $2 0)
                           :left (format nil "~c~a" (char $1 0) $2)
                           :right (format nil "~a~c" $2 (char $1 1))
                           :counts (make-array 41 :initial-element nil))))
     finally (return pairs)))

(defun parse-input (lines)
  (values (first lines) (parse-pairs (nthcdr 2 lines))))

(defun merge-counts (a b)
  (loop for c being the hash-keys of b do (incf (gethash c a 0) (gethash c b))))

(defun expand (pair pairs steps)
  (let ((p (gethash pair pairs)))
    (cond
      ((zerop steps)
       (make-hash-table))
      ((aref (pair-counts p) steps)
       (aref (pair-counts p) steps))
      (t
       (let ((counts (make-hash-table)))
         (setf (gethash (pair-char p) counts) 1)

         ;; get the counts of the left and right pairs, update this pair's counts
         (merge-counts counts (expand (pair-left p) pairs (1- steps)))
         (merge-counts counts (expand (pair-right p) pairs (1- steps)))

         ;; set the counts of this pair now that it's been computed
         (setf (aref (pair-counts p) steps) counts))))))

(defun template-counts (s)
  (loop
     with counts = (make-hash-table)
     for c across s
     do (incf (gethash c counts 0))
     finally (return counts)))

(defun expand-template (s pairs steps)
  (loop
     with counts = (template-counts s)
     for i below (1- (length s))
     do (merge-counts counts (expand (subseq s i (+ i 2)) pairs steps))
     finally (return counts)))

(defun max-min (counts)
  (let ((max (loop for x being the hash-values of counts maximize x))
        (min (loop for x being the hash-values of counts minimize x)))
    (- max min)))

(defun part-1 (&optional (data #'test-data))
  (multiple-value-bind (template pairs)
      (parse-input (funcall data))
    (time (max-min (expand-template template pairs 10)))))

(defun part-2 (&optional (data #'test-data))
  (multiple-value-bind (template pairs)
      (parse-input (funcall data))
    (time (max-min (expand-template template pairs 40)))))
