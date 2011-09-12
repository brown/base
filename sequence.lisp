;;;; Copyright 2011 Google Inc. All Rights Reserved.
;;;; Author: madscience@google.com (Moshe Looks)

;;;; Functions on sequences.

(in-package #:com.google.base)
(declaim #.*optimize-default*)

(defun prefixp (prefix sequence &key (test #'eql))
  "Does PREFIX match a prefix of SEQUENCE?"
  (let ((mismatch (mismatch prefix sequence :test test)))
    (or (null mismatch) (= mismatch (length prefix)))))

(defun suffixp (suffix sequence &key (test #'eql))
  "Does SUFFIX match a suffix of SEQUENCE?"
  (let ((mismatch (mismatch suffix sequence :test test :from-end t)))
    (or (null mismatch) (zerop mismatch))))
