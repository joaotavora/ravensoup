;;     Copyright (C) 2019 João Távora
(defpackage :ravensoup
  (:use #:cl)
  (:export
   #:spellable-p-hash
   #:spellable-p-ascii
   #:spellable-p-mixed
   #:spellable-p-mixed-trained
   #:benchmark-all))
(in-package :ravensoup)

(deftype utf-string ()
  #+(or cmucl sbcl) '(simple-array character)
  #-(or sbcl cmucl) 'string)

(deftype ascii-string ()
  ;; Relevant:
  ;;     https://groups.google.com/forum/#!msg/comp.lang.lisp/Awu4pj12EDY/C-_5wNOk45wJ
  #+(or sbcl) '(simple-array character)
  #-(or sbcl) 'simple-base-string)

(defun spellable-p-hash (message bowl)
  "Non-nil if MESSAGE can be spelled using letters in BOWL."
  (declare (optimize (speed 3))
           (type utf-string message)
           (type utf-string bowl))
  (let ((needed (make-hash-table :test #'eql))
        (len (length message)))
    (declare (type fixnum len))
    (loop for letter across message
          do (incf (the fixnum (gethash letter needed 0))))
    (loop for letter across bowl
          unless (minusp (decf (the fixnum (gethash letter needed 0))))
            do (decf len)
          when (zerop len)
            do (return-from spellable-p-hash t)
          finally (return nil))))

(defun spellable-p-ascii (message bowl)
  "Non-nil if MESSAGE can be spelled using letters in BOWL.
MESSAGE must be coercible to CL:SIMPLE-BASE-STRING (usually means
ASCII or something equivalent), or this fails horribly.  MESSAGE's
length must fit in a fixnum."
  (declare (optimize (speed 3))
           (type ascii-string message)
           (type ascii-string bowl))
  (let ((needed (make-array 256 :element-type 'fixnum))
        (len (length message)))
    (declare (type fixnum len)
             #-allegro
             (dynamic-extent needed))
    (loop for letter across message
          do (incf (aref needed (char-code letter))))
    (loop for letter across bowl
          unless (minusp (decf (aref needed (char-code letter))))
            do (decf len)
          when (zerop len)
            do (return-from spellable-p-ascii t)
          finally (return nil))))

(defconstant +fast-set-len+ 1024
  "Size of the stack-allocated array for SPELLABLE-P-MIXED and
SPELLABLE-P-MIXED-TRAINED.  Can't be too high or SBCL won't be able to
do that optimization.")

(defun spellable-p-mixed (message bowl)
  "Non-nil if MESSAGE can be spelled using letters in BOWL.
MESSAGE's length must fit in a fixnum."
  (declare (optimize (speed 3))
           (type utf-string message)
           (type utf-string bowl))
  (let ((fast-needed (make-array +fast-set-len+ :element-type 'fixnum))
        (more-needed (make-hash-table :test #'eql))
        (len (length message)))
    (declare (type fixnum len)
             #-allegro
             (dynamic-extent fast-needed))
    (loop for letter across message
          for code = (char-code letter)
          if (< code +fast-set-len+)
            do (incf (aref fast-needed code))
          else
            do (incf (the fixnum (gethash letter more-needed 0))))
    (loop for letter across bowl
          for code = (char-code letter)
          for decremented
            = (if (< code +fast-set-len+)
                  (decf (aref fast-needed code))
                  (decf (the fixnum (gethash letter more-needed 0))))
          unless (minusp decremented)
            do (decf len)
          when (zerop len)
            do (return-from spellable-p-mixed t)
          finally (return nil))))

(defvar *dataset* nil
  "If non-nil bound to the current dataset being tested by ")

(defun spellable-p-mixed-trained
    (message bowl &optional (char-code-fn (if *dataset*
                                              (trained-function *dataset*)
                                              #'char-code)))
  "Non-nil if MESSAGE can be spelled using letters in BOWL.
MESSAGE's length must fit in a fixnum."
  (declare (optimize (speed 3))
           (type utf-string message)
           (type utf-string bowl)
           (type (function (character) fixnum) char-code-fn))
  (let ((fast-needed (make-array +fast-set-len+ :element-type 'fixnum))
        (more-needed (make-hash-table :test #'eql))
        (len (length message)))
    (declare (type fixnum len)
             #-allegro
             (dynamic-extent fast-needed))
    (loop for letter across message
          for code = (funcall char-code-fn letter)
          if (< code +fast-set-len+)
            do (incf (aref fast-needed code))
          else
            do (incf (the fixnum (gethash letter more-needed 0))))
    (loop for letter across bowl
          for code = (funcall char-code-fn letter)
          for decremented
            = (if (< code +fast-set-len+)
                  (decf (aref fast-needed code))
                  (decf (the fixnum (gethash letter more-needed 0))))
          unless (minusp decremented)
            do (decf len)
          when (zerop len)
            do (return-from spellable-p-mixed-trained t)
          finally (return nil))))


;;; Helpers
;;;
(defun phrases-and-bowl (dataset)
  "Get test phrases and alphabet bowl from DATASET, a pathname.
Split the DATASET, a file, in two: use the first half to get phrases ,
one per line, and the second half for the BOWL.  Return those two
values: PHRASES, a list of strings BOWL, a string.

MESSAGE's length must fit in a fixnum."
  (let* (phrases
         (bowl
           (with-output-to-string (s)
             (with-open-file (f dataset :external-format :utf-8)
               (loop with size = (file-length f)
                     for line = (read-line f nil nil)
                     while line
                     if (< (file-position f)
                           (/ size 2))
                       do (push line phrases)
                     else
                       do (write-line line s))))))
    (values phrases bowl)))

(defvar *training-cache* (make-hash-table :test #'equal)
  "A cache of training functions.")

(defun trained-function (dataset &optional howmany)
  "Make a function like CL:CHAR-CODE, but trained on DATASET.
The function will produce lower numbers for characters occuring more
often in DATASET.  If non-nil HOWMANY, use just the that many
characters from the start of DATASET."
  (or
   (gethash (cons dataset howmany) *training-cache*)
   (setf (gethash (cons dataset howmany) *training-cache*)
         (let* ((slurped
                  (with-open-file (f dataset :external-format :utf-8)
                    (let ((seq (make-array (or howmany
                                               (file-length f))
                                           :element-type 'character
                                           :fill-pointer t)))
                      (setf (fill-pointer seq)
                            (read-sequence seq f))
                      seq)))
                (histogram (let ((ht (make-hash-table)))
                             (loop for char across slurped
                                   do (incf (gethash (char-code char) ht 0)))
                             ht))
                (histogram-alist
                  (let ((alist nil))
                    (maphash (lambda (k v)
                               (push (cons k v) alist))
                             histogram)
                    alist))
                (most-frequent (subseq (sort histogram-alist #'> :key #'cdr)
                                       0 (min +fast-set-len+
                                              (hash-table-count histogram))))
                (table (make-array char-code-limit
                                   :element-type 'fixnum
                                   :initial-element -1)))
           (loop for (code . nil) in most-frequent
                 for i from 0
                 do (setf (aref table code) i))
           ;; Now return the lambda
           (values
            (lambda (char)
              (let ((opt (aref table (char-code char))))
                (if (minusp opt)
                    (+ +fast-set-len+ (char-code char)) ; avoid collision
                    opt)))
            histogram
            histogram-alist
            most-frequent
            table)))))

(defun benchmark (fn *dataset* &optional (repetitions 1))
  "Test FN on DATASET."
  (multiple-value-bind (phrases bowl)
      (phrases-and-bowl *dataset*)
    (flet ((do-it ()
             (loop repeat repetitions
                   do (loop
                        repeat 5000
                        for msg in phrases
                        do (funcall fn msg bowl)))))
      #+cl-ppcre
      (multiple-value-bind (match submatches)
          (ppcre:scan-to-strings
           (ppcre:create-scanner
            #+sbcl "([-0-9\.]+) user"
            #+ccl "([-0-9\.]+) seconds.*user mode"
            #+cmucl "([-0-9\.e]+) seconds of user run time"
            :multi-line-mode t)
           (with-output-to-string (*trace-output*)
             (time (do-it))))
        (when match
          (princ (elt submatches 0) *trace-output*)
          (princ "s")))
      #-cl-ppcre
      (time (do-it)))
    t))

(defun benchmark-all (&key
                        (functions
                         '(spellable-p-hash
                           spellable-p-ascii
                           spellable-p-mixed
                           spellable-p-mixed-trained))
                        (datasets
                         '("big-ascii.txt"
                           "das-kapital-utf-8.txt"
                           "big-chinese-utf-8.txt")))
  (mapc #'trained-function datasets) ; warm-up cache
  (loop
    for dataset in datasets
    do (format *trace-output* "~&~a~%" dataset)
       (loop for function in functions
             do (format t "~&   ~a:  " function)
                (force-output )
                (ignore-errors (benchmark function dataset)))))




;;; Very basic tests
;;;
(defun make-random-ascii-string (count)
  (let ((str (make-string count)))
    (loop for i from 0 below (length str)
          do (setf (aref str i) (code-char (random 256))))
    str))

(defun basic-test ()
  (flet ((test (fn msg bowl expected)
           (assert (eq expected
                       (funcall fn msg bowl))
                   ()
                   "Expected ~a on ~a and ~a to be ~a, but wasn't"
                   fn msg bowl expected)))
    (loop for fn in
          '(spellable-p-hash
            spellable-p-ascii
            spellable-p-mixed
            spellable-p-mixed-trained)
          do
             (test fn "foo" "oof" t)
             (test fn "foo" "oo" nil)
             (test fn "foo" "ooooof" t)
             (test fn "foo" "ofaksjhdksajdh" nil)
             (test fn "foo" "ofaksjhdksajdho" t)
             ;; despite the chinese character, this next one doesn't
             ;; trip `spellable-p-ascii' because of the early exit
             (test fn "foo" "ofaksojhdksajd前h" t))
    'OK))

(defun smoke-test (fn1 fn2 &optional (repetitions 100))
  (let ((bowl (make-random-ascii-string 3000))
        (messages (loop repeat 10000
                        collect (make-random-ascii-string (random 1000)))))
    (cons
     (* repetitions 10000)
     (loop
       repeat repetitions
       sum (loop
             for msg in messages
             for v1 = (funcall fn1 msg bowl)
             for v2 = (funcall fn2 msg bowl)
             do (assert (eq v1 v2)
                        nil
                        "Hmmm. For~%~%msg: '~a'~%~%and bowl: '~a'~%~%~a~
returned ~a but ~a returned ~a" msg bowl fn1 v1 fn2 v2)
             when v1 sum 1)))))

;; Local Variables:
;; coding: utf-8
;; End:
