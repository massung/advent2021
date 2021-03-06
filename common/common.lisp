(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :boost-re)

  ;; helper package for parsing
  (use-package :boost-re))

(defparameter *day* (nth-value 3 (decode-universal-time (+ 3600 (get-universal-time)))))

(defun puzzle-pathname (&optional (day *day*))
  "Returns the path to the day's puzzle data."
  (format nil "/Users/jeff/Projects/advent/day~a" day))

(defun read-lines (filename &optional (reader #'identity))
  "Collect each of the lines in a file, optionally parsing."
  (with-open-file (f filename)
    (loop
       for line = (read-line f nil)
       while line
       collect (funcall reader line))))

(defun test-pathname ()
  "Returns the pathname of the test data."
  (format nil "~a/test.txt" (puzzle-pathname)))

(defun real-pathname ()
  "Returns the pathname of the real data."
  (format nil "~a/real.txt" (puzzle-pathname)))

(defun test-data (&optional (reader #'identity))
  "Loads the test data for this puzzle."
  (read-lines (format nil "~a/test.txt" (puzzle-pathname)) reader))

(defun real-data (&optional (reader #'identity))
  "Loads the real data for this puzzle."
  (read-lines (format nil "~a/real.txt" (puzzle-pathname)) reader))

(defun read-numbers (str &key (sep #r/%s+/) (read #'parse-integer))
  (mapcar read (split-re sep str :all t)))
