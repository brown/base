;;;; Copyright 2011 Google Inc.  All Rights Reserved

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;     * Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;;     * Redistributions in binary form must reproduce the above
;;;; copyright notice, this list of conditions and the following disclaimer
;;;; in the documentation and/or other materials provided with the
;;;; distribution.
;;;;     * Neither the name of Google Inc. nor the names of its
;;;; contributors may be used to endorse or promote products derived from
;;;; this software without specific prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Author: brown@google.com (Robert Brown)

;;;; Test base code.

(in-package #:common-lisp-user)

(defpackage #:com.google.base-test
  (:documentation "Test code in the COM.GOOGLE.BASE package.")
  (:use #:common-lisp
        #:com.google.base
        #:hu.dwim.stefil)
  (:export #:test-base))

(in-package #:com.google.base-test)
(declaim #.*optimize-default*)

(defsuite (test-base :in root-suite) ()
  (run-child-tests))

(in-suite test-base)

(defclass point ()
  ((x :reader x :initarg :x)
   (y :reader y :initarg :y)
   (hidden :reader hidden :initarg hidden)))

(define-print-object point ((x :x "x is ~D") (y :y "y is ~D") (hidden 'hidden)))

(defun point-equal (point1 point2)
  (and (= (x point1) (x point2))
       (= (y point1) (y point2))
       (= (hidden point1) (hidden point2))))

(deftest define-print-object-print-read-consistency ()
  (let* ((point1 (make-instance 'point :x 100 :y 200 'hidden 300))
         (point2 (with-standard-io-syntax (read-from-string (write-to-string point1)))))
    (is (point-equal point1 point2))))

(deftest define-print-object-unreadable-printing ()
  (let* ((point (make-instance 'point :x 11 :y 22 'hidden 123456789))
         (output (with-standard-io-syntax (write-to-string point :readably nil))))
    (is (search "x is 11" output))
    (is (search "y is 22" output))
    (is (null (search "123456789" output)))))

(defun tree-search (x tree)
  (cond ((equal x tree) t)
        ((consp tree) (or (tree-search x (car tree)) (tree-search x (cdr tree))))
        (t nil)))

(deftest define-print-object-no-format-strings ()
  ;; Test that the code uses (call-next-method) when no format strings are supplied.
  (is (tree-search '(call-next-method) (macroexpand-1 '(define-print-object foo ((z :z)))))))

(deftest define-print-object-syntax-errors ()
  (flet ((signals-error (form)
           (signals error (macroexpand-1 form))))
    (signals-error '(define-print-object ((z :z "~D zebras"))))
    (signals-error '(define-print-object foobar ((z "~D zebras"))))
    (signals-error '(define-print-object foobar ((z :z zebras))))
    (signals-error '(define-print-object foobar ((z initarg zebras))))
    (signals-error '(define-print-object foobar ((z))))
    (signals-error '(define-print-object foobar ((z :z "~D zebras" junk))))))
