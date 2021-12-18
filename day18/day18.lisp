(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defstruct node parent x)

(defmethod print-object ((node node) stream)
  (let ((x (node-x node)))
    (if (atom x)
        (print-object x stream)
      (format stream "(~a ~a)" (car x) (cdr x)))))

(defun parse-elt (s)
  (let ((c (read-char s)))
    (if (char= c #\[)
        (parse-pair s)
      (make-node :x (digit-char-p c)))))

(defun parse-pair (s)
  (let ((x (parse-elt s))
        (comma (read-char s))
        (y (parse-elt s))
        (term (read-char s)))
    (assert (and (char= comma #\,) (char= term #\])))
    (let ((node (make-node :x (cons x y))))
      (prog1 node
        (setf (node-parent x) node
              (node-parent y) node)))))

(defun parse-line (line)
  (with-input-from-string (s line)
    (parse-elt s)))

(defun leftmost-value (node)
  (if (atom (node-x node))
      (values node)
    (leftmost-value (car (node-x node)))))

(defun rightmost-value (node)
  (if (atom (node-x node))
      (values node)
    (rightmost-value (cdr (node-x node)))))

(defun leftmost-pair (node)
  (multiple-value-bind (node)
      (leftmost-value node)
    (values (node-parent node))))

(defun rightmost-pair (node)
  (multiple-value-bind (node)
      (rightmost-value node)
    (values (node-parent node))))

(defun prev-value (node)
  (let ((p (node-parent node)))
    (when p
      (let ((left (car (node-x p))))
        (if (eq left node)
            (prev-value p)
          (rightmost-value left))))))

(defun next-value (node)
  (let ((p (node-parent node)))
    (when p
      (let ((right (cdr (node-x p))))
        (if (eq right node)
            (next-value p)
          (leftmost-value right))))))

(defun explode-pairs (root)
  (labels ((explode (node depth)
             (let ((x (node-x node)))
               (when (consp x)
                 (or (when (and (> depth 4) (atom (car x)) (atom (cdr x)))
                       (let ((left (prev-value node))
                             (right (next-value node)))
                         (when left
                           (incf (node-x left) (node-x (car (node-x node)))))
                         (when right
                           (incf (node-x right) (node-x (cdr (node-x node)))))

                         ;; replace the value of this node
                         (setf (node-x node) 0)))

                     ;; try left then right
                     (explode (car x) (1+ depth))
                     (explode (cdr x) (1+ depth)))))))
    (explode root 1)))

(defun split-pairs (node)
  (let ((x (node-x node)))
    (if (atom x)
        (when (> x 9)
          (setf (node-x node)
                (cons (make-node :parent node :x (floor x 2))
                      (make-node :parent node :x (ceiling x 2)))))
      (or (split-pairs (car x))
          (split-pairs (cdr x))))))

(defun reduce-pairs (root)
  (loop while (or (explode-pairs root) (split-pairs root)) finally (return root)))

(defun add-pairs (a b)
  (let ((pair (make-node :parent nil :x (cons a b))))
    (prog1
        pair
      (setf (node-parent a) pair
            (node-parent b) pair))))

(defun magnitude (node)
  (let ((x (node-x node)))
    (if (atom x)
        x
      (let ((left (magnitude (car x)))
            (right (magnitude (cdr x))))
        (+ (* left 3) (* right 2))))))

(defun sum-lines (lines)
  (loop
     with ans = (parse-line (pop lines))
     for line = (pop lines)
     while line
     do (setf ans (reduce-pairs (add-pairs ans (parse-line line))))
     finally (return (magnitude ans))))

(defun part-1 (&optional (data #'test-data))
  (sum-lines (funcall data)))

(defun part-2 (&optional (data #'test-data))
  (let ((lines (funcall data)))
    (loop
       for a = (pop lines)
       while lines
       maximize (loop
                   for b in lines
                   for n = (add-pairs (parse-line a) (parse-line b))
                   for m = (add-pairs (parse-line b) (parse-line a))
                   maximize (max (magnitude (reduce-pairs n))
                                 (magnitude (reduce-pairs m)))))))
