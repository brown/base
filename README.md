# com.google.base

Universally useful Common Lisp code.

The base package contains code that's highly likely to be useful in every Lisp
application.  The package contains:

## ASDF fast-unsafe-source-file component 

The ASDF component type FAST-UNSAFE-SOURCE-FILE allows you to specify maximum
optimization for one Lisp source file.  It is used just like the FILE
component.  In the system definition below, foo is compiled with default
optimization settings, while bar is compiled for maximum run-time speed:

```
(defsystem example
  :defsystem-depends-on (com.google.base)
  :components
  ((:file "foo")
   (:fast-unsafe-source-file "bar")))
```

## A nicer version of DEFCONSTANT

#### defconst name value &optional documentation

```
Identical to CL:DEFCONSTANT except that the global constant variable NAME is
bound to VALUE at compile time so it can be used in #. reader forms.
Additionally, if the DEFCONST form is evaluated a second time, the constant is
not rebound if the new value is EQUALP to the old.  CL:DEFCONSTANT requires
the two values be EQL to avoid undefined behavior.
```

## A missing-argument function

#### missing-argument

```
Calls ERROR with an argument that indicates a required &KEY or &OPTIONAL
function argument was not supplied.  For example, the following function will
signal an error condition for a missing :AGE or :ADDRESS argument.

(defun foo (name &key (age (missing-argument)) (address (missing-argument)))
  ...
  )

A (MISSING-ARGUMENT) form also useful as the value of a slot :INITFORM when the
slot is required.  For instance, the following class definition ensures that a
value is supplied for SLOT when an instance is created:

(defclass foo ()
  ((slot
    :accessor :slot
    :initform (missing-argument))))
```

## Types and functions for manipulating UTF-8 strings

#### octet

```
The type (UNSIGNED-BYTE 8).
```
#### octet-vector &optional length

```
The type `(SIMPLE-ARRAY OCTET (,LENGTH).
```

#### make-octet-vector octet-count &key initial-contents

```
Creates an OCTET-VECTOR containing OCTET-COUNT octets.  If INITIAL-CONTENTS is
not supplied, each element of the vector is initialized to zero.  Otherwise,
the vector is initialized to the contents of list INITIAL-CONTENTS.
```
#### string-to-utf8-octets string &key (start 0) (end (length string))

```
Converts STRING into an OCTET-VECTOR by UTF-8 encoding each character.
```
#### utf8-octets-to-string octets &key (start 0) (end (length octets))

```
Converts OCTETS, a vector of UTF-8 encoded octets, into a string.
```

## Sequence functions

#### prefixp prefix sequence &key (test #'eql)

```
Does PREFIX match a prefix of SEQUENCE?
```

#### suffixp suffix sequence &key (test #'eql)

```
Does SUFFIX match a suffix of SEQUENCE?
```

## Lisp type definitions for common C integer types

The following types are defined:

```
int8
int16
int32
int64
uint8
uint16
uint32
uint64
```
