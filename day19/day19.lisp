(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defun parse-beacons (s)
  (loop
     for line = (read-line s nil)
     while (plusp (length line))
     collect (with-input-from-string (s line)
               (let ((x (read s))
                     (a (read-char s))
                     (y (read s))
                     (b (read-char s))
                     (z (read s)))
                 (assert (and (char= a #\,) (char= b #\,)))
                 (list x y z)))))

(defun parse-scanners (s)
  (loop
     for line = (read-line s nil)
     while line
     collect (let ((scanner (with-re-match (m (find-re "%d+" line))
                              (parse-integer $$))))
               (declare (ignore scanner))
               (parse-beacons s))))

(defun sort-beacons (xs)
  (flet ((less-than (a b)
           (or (< (first a) (first b))
               (< (second a) (second b))
               (< (third a) (third b)))))
    (sort xs #'less-than)))

(defun relation-map (beacons origin)
  (destructuring-bind (mx my mz)
      origin
    (sort-beacons (loop
                     for (x y z) in beacons
                     collect (list (- x mx) (- y my) (- z mz))))))

(defun rotate-beacons (beacons m)
  (flet ((rotate-point (x y z r u f)
           (list (+ (* (first r) x) (* (first u) y) (* (first f) z))
                 (+ (* (second r) x) (* (second u) y) (* (second f) z))
                 (+ (* (third r) x) (* (third u) y) (* (third f) z)))))
    (loop for (x y z) in beacons collect (apply #'rotate-point x y z m))))

(defparameter *rotation-vectors*
  '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0) (0 0 1) (0 0 -1)))

(defparameter *rotation-matrices*
  (destructuring-bind (r -r u -u f -f)
      *rotation-vectors*
    (list (list  r  u  f)
          (list  r -u  f)
          (list  r  u -f)
          (list  r -u -f)
          (list  r  f  u)
          (list  r -f  u)
          (list  r  f -u)
          (list  r -f -u)
          (list  u  r  f)
          (list  u -r  f)
          (list  u  r -f)
          (list  u -r -f)
          (list  u  f  r)
          (list  u -f  r)
          (list  u  f -r)
          (list  u -f -r)
          (list  f  r  u)
          (list  f -r  u)
          (list  f  r -u)
          (list  f -r -u)
          (list  f  u  r)
          (list  f -u  r)
          (list  f  u -r)
          (list  f -u -r)
          (list -r  u  f)
          (list -r -u  f)
          (list -r  u -f)
          (list -r -u -f)
          (list -r  f  u)
          (list -r -f  u)
          (list -r  f -u)
          (list -r -f -u)
          (list -u  r  f)
          (list -u -r  f)
          (list -u  r -f)
          (list -u -r -f)
          (list -u  f  r)
          (list -u -f  r)
          (list -u  f -r)
          (list -u -f -r)
          (list -f  u  r)
          (list -f -u  r)
          (list -f  u -r)
          (list -f -u -r)
          (list -f  r  u)
          (list -f -r  u)
          (list -f  r -u)
          (list -f -r -u))))

(defun all-orientations (scanner)
  (loop for m in *rotation-matrices* collect (rotate-beacons scanner m)))

(defun find-beacons (a b)
  (loop
     for origin-a in a
     for ma = (relation-map a origin-a)
     do (loop
           for m in *rotation-matrices*
           for rb = (sort-beacons (rotate-beacons b m))
           do (loop
                 for origin-b in rb
                 for mb = (relation-map rb origin-b)
                 for i = (intersection ma mb :test 'equal)
                 when (>= (length i) 12)
                 do (return-from find-beacons
                      (let ((delta (mapcar #'- origin-a origin-b)))
                        (values delta (loop for p in rb collect (mapcar #'+ p delta)))))))))

(defun find-all-beacons (scanners)
  (let ((all (pop scanners))
        (deltas ()))
    (flet ((align (scanner)
             (multiple-value-bind (delta aligned)
                 (find-beacons all scanner)
               (prog1 aligned
                 (when aligned
                   (setf all (sort-beacons (remove-duplicates (append all aligned) :test 'equal)))
                   (push delta deltas))))))
      (do ()
          ((null scanners) (values deltas all))
        (format t "Aligning ~a beacons to ~a scanners...~%" (length all) (length scanners))
        (let ((left (remove-if #'align scanners)))
          (setf scanners (if (= (length left) (length scanners))
                             nil
                           left)))))))

(defun manhattan (deltas)
  (loop
     for a = (pop deltas)
     while a
     maximize (loop
                 for b in deltas
                 maximize (reduce #'+ (mapcar #'(lambda (x1 x2) (abs (- x1 x2))) b a)))))

(defun run (scanners)
  (multiple-value-bind (deltas beacons)
      (find-all-beacons scanners)
    (print deltas)
    (format t ";; beacons = ~a~%" (length beacons))
    (format t ";; max range = ~a~%" (manhattan deltas))))

(defun part-1 (&optional (pathname #'test-pathname))
  (with-open-file (s (funcall pathname))
    (time (run (parse-scanners s)))))

(defun part-2 (&optional (data #'test-data))
  (part-1 data))
