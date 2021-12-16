(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/Users/jeff/Projects/advent/common/common.lisp"))

(defstruct bits seq pos)
(defstruct header version type)
(defstruct packet header child-packets value)

(defun parse-data (line)
  (make-bits :seq (parse-integer line :radix 16)
             :pos (* (length line) 4)))

(defun read-bits (bits n)
  (ldb (byte n (decf (bits-pos bits) n)) (bits-seq bits)))

(defun read-header (bits)
  (make-header :version (read-bits bits 3) :type (read-bits bits 3)))

(defun read-literal (bits)
  (loop
     with value = 0
     for code = (read-bits bits 5)
     for data = (logand code #xf)
     do (setf value (logior (ash value 4) data))
     when (zerop (logand code #x10))
     return value))

(defun read-dynamic-packets (bits len)
  (let ((sb (make-bits :seq (read-bits bits len) :pos len)))
    (loop until (zerop (bits-pos sb)) collect (read-packet sb))))

(defun read-counted-packets (bits)
  (loop for i below (read-bits bits 11) collect (read-packet bits)))

(defun read-packet (bits)
  (let ((header (read-header bits)))
    (if (= (header-type header) 4)  ; literal packet
        (make-packet :header header :value (read-literal bits))
      (let ((sub-packets (let ((i (read-bits bits 1)))
                           (if (zerop i)
                               (let ((len (read-bits bits 15)))
                                 (read-dynamic-packets bits len))
                             (read-counted-packets bits))))

            ;; operator packet
            (op (case (header-type header)
                  ((0) #'+)
                  ((1) #'*)
                  ((2) #'min)
                  ((3) #'max)
                  ((5) #'(lambda (x y) (if (> x y) 1 0)))
                  ((6) #'(lambda (x y) (if (< x y) 1 0)))
                  ((7) #'(lambda (x y) (if (= x y) 1 0))))))
        (make-packet :header header
                     :child-packets sub-packets
                     :value (reduce op sub-packets :key #'packet-value))))))

(defun sum-versions (packet)
  (let ((v (header-version (packet-header packet))))
    (+ v (loop for sp in (packet-child-packets packet) sum (sum-versions sp)))))

(defun part-1 (&optional (data #'test-data))
  (time (dolist (bits (funcall data #'parse-data))
          (print (sum-versions (read-packet bits))))))

(defun part-2 (&optional (data #'test-data))
  (time (dolist (bits (funcall data #'parse-data))
          (print (packet-value (read-packet bits))))))
