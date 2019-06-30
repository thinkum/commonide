;; asn1-proto.lisp - Generalized prototyping, Eval-over-Wire with ASN.1 schema
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2019 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------


(defpackage #:commonide/proto/asn.1
  (:use #:ltp/common/mop #:ltp/common #:cl)
  (:nicknames #:commonide.proto.asn.1))

(in-package  #:commonide/proto/asn.1)

(eval-when (:compile-toplevel :execute)

(defmacro mkv (contents &optional (element-type t))
  (with-symbols (%c)
    `(let ((,%c ,contents))
       (make-array (length ,%c)
                   :element-type ,element-type)
       )))
)

(defstruct (asn.1-oid
             (:constructor
              %mk-asn.1-oid (oid-bytes oid-symbols))
             (:conc-name #:asn.1-oid@))
  ;; TBD: Registry and referencing; interp @ tools, REPL, subsq.
  (oid-bytes
   (mk-lf (mkv nil 'fixnum))
   :type (simple-array fixnum (*))
   :read-only t)
  (oid-symbols
   (mk-lf (mkv nil 'string))
   :type (simple-array string (*))
   :read-only t
   ))

;; TBD: Generalized Type Layouts for object/value signatures onto ASN.1

;; TBD/Topic: Language Models. Project Resource Models, and ASN.1
;; Protocol Specifications

;; TBD Syntactic transformations for interfaces to scalar values in ASN.1,
;; ASN.1 -> C -> Lisp, transitively ASN.1 -> Lisp (cf. ports)
;;
;; TBD Type signatures, transitively ASN.1 -> Lisp
;;
;; TBD Encoding Rules for arbitrary transport devices, generally,
;; onto same ASN.1 -> Lisp toolchain.

;; TBD A minimal bibliography for ASN.1 - Language Description, Surveys
;; of Applications, and Generalized Programming Patterns
;;
;; - NB 'Any Type' declations in ASN.1
;; - NB "Strong typing" with ASN.1 in applications
;; - NB Structure and Modularization in ASN.1 Protocol Definitions
