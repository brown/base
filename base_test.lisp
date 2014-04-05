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

(defsuite (test-base :in root-suite) ()
  (run-child-tests))

(in-suite test-base)

;;; syntax tests

(defclass foo ()
  ((x :reader x :initarg :x :type integer)
   (y :reader y :initarg y :type symbol)
   (hidden :reader hidden :initarg :hidden :type integer)))

(define-print-object foo ((x :x "x is ~D") (y 'y "y is ~S") (hidden :hidden)))

(defun foo-equal (foo1 foo2)
  (and (= (x foo1) (x foo2))
       (eq (y foo1) (y foo2))
       (= (hidden foo1) (hidden foo2))))

(deftest define-print-object-print-read-consistency ()
  (let* ((foo1 (make-instance 'foo :x 100 'y 'hello :hidden 300))
         (foo2 (with-standard-io-syntax (read-from-string (write-to-string foo1)))))
    (is (foo-equal foo1 foo2))))

(deftest define-print-object-unreadable-printing ()
  (let* ((foo (make-instance 'foo :x 11 'y 'world :hidden 123456789))
         (output (with-standard-io-syntax
                   (let ((*package* (find-package :com.google.base-test)))
                     (write-to-string foo :readably nil)))))
    (is (search "x is 11" output))
    (is (search "y is WORLD" output))
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

;;; octet tests

(deftest octet-types ()
  (let ((octets (make-octet-vector 10)))
    (is (typep octets 'octet-vector))
    (is (typep octets '(octet-vector 10)))
    (is (typep (aref octets 5) 'octet))))
