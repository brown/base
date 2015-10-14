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

;;;; Commonly used macros.

(in-package #:com.google.base)

(defmacro defconst (name value &optional (documentation nil documentation-present-p))
  "Identical to CL:DEFCONSTANT except that the global constant variable NAME is
bound to VALUE at compile time so it can be used in #. reader forms.
Additionally, if the DEFCONST form is evaluated a second time, the constant is
not rebound if the new value is EQUALP to the old.  CL:DEFCONSTANT requires
that the two values be EQL to avoid undefined behavior."
  (assert (symbolp name)
          (name) "constant name ~S is not a symbol" name)
  (assert (or (not documentation-present-p) (stringp documentation))
          (documentation) "documentation for constant ~S is not a string" name)
  (let ((temp (gensym))
        (documentation (when documentation-present-p (list documentation))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,temp ,value))
         (if  (and (boundp ',name) (equalp (symbol-value ',name) ,temp))
              ;; Return the same result as CL:DEFCONSTANT.
              ',name
              (defconstant ,name ,temp ,@documentation))))))

(defmacro define-print-object (class-name accessor-info)
  "Generates a CL:PRINT-OBJECT generic function for class CLASS-NAME using
ACCESSOR-INFO, a list containing elements of the form
(accessor initarg [format-string]).

If a format-string is not provided for a slot, then the generated PRINT-OBJECT
function outputs nothing for that slot when *PRINT-READABLY* is false.

Given a POINT class with slots X, Y, and HIDDEN.  The following
DEFINE-PRINT-OBJECT form:

  (define-print-object point
    ((x :x \"x is ~D\")
     (y :y \"y is ~D\")
     (hidden :hidden)))

expands to a PRINT-OBJECT function similar to:

  (defmethod print-object ((point point) stream)
    (if *print-readably*
      (progn (write-string \"#.\" stream)
             (write `(make-instance 'point
                                    :x ,(x point)
                                    :y ,(y point)
                                    :hidden ,(hidden point))
                    :stream stream))
      (print-unreadable-object (point stream :type t :identity t)
        (format stream \"x is ~D y is ~D\" (x point) (y point)))))"
  (dolist (info accessor-info)
    (destructuring-bind (accessor initarg &optional format &rest rest)
        info
      (assert (null rest) () "too many arguments")
      (assert (and accessor (symbolp accessor))
              (accessor) "~S is not an accessor" accessor)
      ;; Allow keywords and quoted symbols as initargs.
      (assert (or (keywordp initarg)
                  (and (listp initarg)
                       (= (length initarg) 2)
                       (eq (first initarg) 'quote)
                       (and (symbolp (second initarg)))))
              (initarg) "initarg ~S is not a keyword or quoted symbol" initarg)
      (assert (or (not format) (stringp format))
              (format) "~S is not a format string" format)))
  (let* ((object (gensym))
         (stream (gensym))
         (initargs
           (loop for (accessor initarg) in accessor-info
                 collect `',initarg
                 collect ``',(,accessor ,object)))
         (format-accessors
           (loop for (accessor nil format) in accessor-info
                 when format collect `(,accessor ,object)))
         (format-string
           (with-output-to-string (s)
             (loop for (nil nil format) in accessor-info do
               (when format (format s "~A " format))))))
    `(defmethod print-object ((,object ,class-name) ,stream)
       (if *print-readably*
           (progn (write-string "#." ,stream)
                  (write `(make-instance ',',class-name  ,,@initargs) :stream ,stream))
           ,(if (string= format-string "")
                '(call-next-method)
                `(print-unreadable-object (,object ,stream :type t :identity t)
                   (format ,stream ,format-string ,@format-accessors)))))))
