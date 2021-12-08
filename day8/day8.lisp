(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-input (s)
  (multiple-value-bind (patterns output)
      (re:split-re "%s*%|%s*" s)
    (values (re:split-re "%s+" patterns :all t)
            (re:split-re "%s+" output :all t))))

(defun possible-digits (s)
  (case (length s)
    ((2) '(1))
    ((3) '(7))
    ((4) '(4))
    ((5) '(2 3 5))
    ((6) '(0 6 9))
    ((7) '(8))))

(defparameter *positions*
  #((0 1 2 4 5 6)     ; 0
    (2 5)             ; 1
    (0 2 3 4 6)       ; 2
    (0 2 3 5 6)       ; 3
    (1 2 3 5)         ; 4
    (0 1 3 5 6)       ; 5
    (0 1 3 4 5 6)     ; 6
    (0 2 5)           ; 7
    (0 1 2 3 4 5 6)   ; 8
    (0 1 2 3 5 6)))   ; 9

(defun make-digit (m s)
  (let ((posns (sort (loop for c across s collect (cadr (assoc c m))) #'<)))
    (position posns *positions* :test #'equal)))

(defun possible-positions (pattern)
  (reduce #'union (possible-digits pattern) :key #'(lambda (d) (aref *positions* d))))

(defun permutations (m)
  (let ((perms nil))
    (labels ((perm (m acc exclude)
               (if (null m)
                   (push acc perms)
                 (loop
                    for i in (cdar m)
                    unless (member i exclude)
                    do (perm (cdr m) (cons (list (caar m) i) acc) (cons i exclude))))))
      (perm m nil nil))
    perms))

(defun build-signal-map (patterns)
  (let ((m (map 'list #'(lambda (c) (cons c '(0 1 2 3 4 5 6))) "abcdefg")))

    ;; initial signal map of char -> possible positions
    (dolist (p (sort (copy-seq patterns) #'< :key #'length))
      (let ((posns (possible-positions p)))
        (loop
           for c across p
           for a = (assoc c m)
           do (rplacd a (sort (intersection (cdr a) posns) #'<)))))

    ;; sort shortest seq -> longest and strip
    (setf m (sort m #'< :key #'length))
    (dolist (a m)
      (loop
         for b in m
         unless (equal (cdr a) (cdr b))
         do (rplacd b (sort (set-difference (cdr b) (cdr a)) #'<))))

    ;; permute until predicate tests across all patterns succeed
    (flet ((holds-true-p (perms)
             (and (= (length perms) 7)
                  (loop for pat in patterns always (make-digit perms pat)))))
      (find-if #'holds-true-p (permutations m)))))

(defun part-1 (&optional (data #'test-data))
  (let ((lines (funcall data)))
    (loop
       for line in lines
       sum (multiple-value-bind (patterns output)
               (parse-input line)
             (declare (ignore patterns))
             (count-if #'(lambda (s) (let ((g (possible-digits s)))
                                       (member (car g) '(1 4 7 8))))
                       output)))))

(defun part-2 (&optional (data #'test-data))
  (let ((lines (funcall data)))
    (loop
       for line in lines
       sum (multiple-value-bind (patterns output)
               (parse-input line)
             (let ((m (build-signal-map patterns)))
               (loop
                  for i in '(1000 100 10 1)
                  for s in output
                  for n = (* (make-digit m s) i)
                  sum n))))))
