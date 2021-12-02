(defpackage :advent
  (:use :cl)
  (:export
   #:puzzle
   #:puzzle-pathname
   #:read-lines
   #:read-data
   #:test-data
   #:real-data))

(in-package :advent)

(defun puzzle ()
  "Returns the current puzzle number (day of month)."
  (nth-value 3 (decode-universal-time (+ 3600 (get-universal-time)))))

(defun puzzle-pathname (&optional (day (puzzle)))
  "Returns the path to the day's puzzle data."
  (format nil "/Users/jeff/Projects/advent/day~a" day))

(defun read-lines (filename &optional (reader #'identity))
  "Collect each of the lines in a file, optionally parsing."
  (with-open-file (f filename)
    (loop
       for line = (read-line f nil)
       while line
       collect (funcall reader line))))

(defun test-data (&optional (reader #'identity))
  "Loads the test data for this puzzle."
  (read-lines (format nil "~a/test.txt" (puzzle-pathname)) reader))

(defun real-data (&optional (reader #'identity))
  "Loads the real data for this puzzle."
  (read-lines (format nil "~a/real.txt" (puzzle-pathname)) reader))
