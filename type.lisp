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

;;;; Constants and types that are very commonly used.

(in-package #:com.google.base)

;;; Lisp integer types with the same numeric range as C++ ints.

(deftype int8 () "A signed 8-bit integer." '(signed-byte 8))
(deftype int16 () "A signed 16-bit integer." '(signed-byte 16))
(deftype int32 () "A signed 32-bit integer." '(signed-byte 32))
(deftype int64 () "A signed 64-bit integer." '(signed-byte 64))

(deftype uint8 () "An unsigned 8-bit integer." '(unsigned-byte 8))
(deftype uint16 () "An unsigned 16-bit integer." '(unsigned-byte 16))
(deftype uint32 () "An unsigned 32-bit integer." '(unsigned-byte 32))
(deftype uint64 () "An unsigned 64-bit integer." '(unsigned-byte 64))

;;; Vector index types.

(defconst +maximum-vector-index+ (1- array-dimension-limit) "Largest valid vector index.")

(deftype vector-index ()
  "Integer that can be used as a subscript for accessing an array or vector element."
  '(integer 0 #.+maximum-vector-index+))
