;;;; Copyright 2014 Google Inc.  All Rights Reserved

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

(defclass fast-unsafe-source-file (cl-source-file)
  ()
  (:documentation
"A Common Lisp source file that is compiled with high optimization settings."))

(defun call-with-compiler-policy (thunk policy)
  #+abcl
  (let ((system::*debug* system::*debug*)
        (system::*safety* system::*safety*)
        (system::*space* system::*space*)
        (system::*speed* system::*speed*))
    (proclaim policy)
    (funcall thunk))
  #+clisp
  (let ((previous-policy
          (loop for key being the hash-keys of system::*optimize* using (hash-value value)
                collect (cons key value))))
    (unwind-protect
         (progn (proclaim policy)
                (funcall thunk))
      (clrhash system::*optimize*)
      (loop for (key . value) in previous-policy
            do (setf (gethash key system::*optimize*) value))))
  #+clozure
  (let ((ccl::*nx-cspeed* ccl::*nx-cspeed*)
        (ccl::*nx-debug* ccl::*nx-debug*)
        (ccl::*nx-safety* ccl::*nx-safety*)
        (ccl::*nx-space* ccl::*nx-space*)
        (ccl::*nx-speed* ccl::*nx-speed*))
    (proclaim policy)
    (funcall thunk))
  #+(or cmucl scl)
  (let ((c::*default-cookie* c::*default-cookie*))
    (proclaim policy)
    (funcall thunk))
  #+ecl
  (let ((c::*debug* c::*debug*)
        (c::*safety* c::*safety*)
        (c::*space* c::*space*)
        (c::*speed* c::*speed*))
    (proclaim policy)
    (funcall thunk))
  #+sbcl
  (let ((sb-c::*policy* sb-c::*policy*))
    (proclaim policy)
    (funcall thunk))
  #-(or abcl clisp clozure cmucl ecl sbcl scl)
  (progn
    (warn "unable to safely change compiler optimization policy")
    (funcall thunk)))

(defmethod perform :around ((operation compile-op) (component fast-unsafe-source-file))
  (let ((policy (symbol-value (read-from-string "com.google.base:*optimize-fast-unsafe*"))))
    (call-with-compiler-policy #'call-next-method policy)))

(defmethod perform :around ((operation load-op) (component fast-unsafe-source-file))
  (let ((policy (symbol-value (read-from-string "com.google.base:*optimize-fast-unsafe*"))))
    (call-with-compiler-policy #'call-next-method policy)))

(defsystem com.google.base
  :name "Lisp base"
  :description "Universally useful Lisp code."
  :long-description "Code that should be useful for any Lisp application."
  :version "1.4"
  :author "Robert Brown"
  :license "New BSD license.  See the copyright messages in individual files."
  :depends-on (#-(or allegro ccl clisp sbcl) trivial-utf-8)
  :in-order-to ((test-op (test-op com.google.base-test)))
  :components
  ((:file "package")
   (:file "optimize" :depends-on ("package"))
   (:file "syntax" :depends-on ("package" "optimize"))
   (:file "error" :depends-on ("package" "optimize"))
   (:file "type" :depends-on ("package" "optimize" "syntax"))
   (:fast-unsafe-source-file "octet" :depends-on ("package" "optimize" "type"))
   (:file "sequence" :depends-on ("package" "optimize"))))
