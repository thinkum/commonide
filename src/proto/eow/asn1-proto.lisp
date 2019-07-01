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

#+sb-unicode
(pushnew :asn.1-unicode *features* :test #'eq)
;; TBD Other

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

(defstruct (asn.1-declaration
             (:conc-name declaration-)
             (:constructor))
  (name
   (mk-lf "Unnamed Declaration")
   :type simple-string
   :read-only t))

(defstruct (asn.1-type-declaration
             (:include asn.1-declaration)
             (:constructor))
  ;; NB ASN.1 "Any" type in abstract structural declarations

  ;; NB: Type tagging for disambiguation of generalized union values -
  ;; refer to annotation, subsq.
  )

;; TBD: Signature syntax for ASN.1 generalized union signatures
;; i.e SEQUENCE, SET, CHOICE
;; ... and ENUMERATION? as relevant in module-specific tagging
;; behaviors, described to an extent at p. 113 (PDF p. 141) of
;; [Dubuisson]
;;
;; - Note that each generalized union memebr must have a tag stored with
;; it, complimentary to the tag - in the same sense - as assigned to the
;; type of the member. Refer, perhaps, to [Dubuisson] PDF pp. 234-246

;; NB: If this system is implemented for interoperation with an ASN.1
;; interpreter e.g in C, this type signature model may be revised for
;; further support of the particular interpreter tool.


(defstruct (asn.1-structure-declaration
             ;; TBD: Too simplified (??)
             (:include asn.1-declaration)
             (:constructor))
  )


;; -- Prototypes towards a parser semantics

;; (defvar +asn.1-readtable+ ...)

;; (defvar *case-fold-asn.1-identifiers* ??)
;; ^ TBD: Compiler macro for IDENTIFIER-P dispatching on that value,
;; during evaluation

(defun* identifier-p (str)
  (declare (type string str)
           (values boolean (or character null) &optional))
  (let ((len (length str))
        (in-dash))
    (declare (type array-index len))
    (cond
      ((zerop len) (values nil nil))
      (t
       (let ((c0 (char str 0))
             (c-last (char str (1- len))))
         (cond
           ((not (and (alpha-char-p c0)
                      ;; TBD case-folding per runtime config
                      (lower-case-p c0)))
            (values nil c0))
           ((char= c-last #\-)
            (values nil c-last))
           (t
            (do ((n 1 (1+ n)))
                ((= n len) (values t nil))
              (declare (type array-index n))
              (let ((c (aref str n)))
                (cond
                  ((alphanumericp c)
                   (setq in-dash nil))
                  ((or (char= c #\-)
                       ;; NB: #\NON-BREAKING_HYPHEN
                       #+:asn.1-unicode
                       (char= c (mk-lf (code-char #x2011))))
                   (if in-dash (return (values nil c))
                       (setq in-dash t)))
                  (t (return (values nil c))
                     )))))))))))

;; (identifier-p "frob")

;; (identifier-p "Frob")

;; (identifier-p "-")

;; (identifier-p "frob-b")

;; (identifier-p "frob--b")

;; (identifier-p "frob-b-")


;; ^ cf. ASN OID symbolic elements
;; Ref: ITU-T Recommendation X.680 (08/2015) subclause 12.3,
;; "Identifiers," and clause 11, "The ASN.1 Character Set"
;;
;; NB: A subset of/ Unicode characters is supported for identifier
;; tokens in ASN.1, per X.680 (08/2015) subclauses 11.1 and 12.3
;; generally: Letter characters, Digit characters, and "Hyphens" broadly
;; (see susbq) such that may be understood as denoting complimetary
;; subsets of code points in UCS. As such, this software system will
;; rely on the implementation's support for character predicate
;; functions onto the respective UCS code points.
;;
;; NB: ITU-T Recommendation X.680 (08/2015) permits the usage of a
;; non-breaking hyphen character in identifier tokens, but specifies -
;; in effect - that it is to be interpreted as representing a hyphen-
;; minus character.
;;
;; NB: LTP/COMMON:SIMPLIFY-STRING


(defun* simplify-identifier (str)
  (declare (type string str)
           (values simple-string &optional))
  (let* ((len (length str))
         (buf (make-array len
                          :element-type 'character
                          :initial-contents str))
         non-base-str)
    (declare (type array-index len)
             (type simple-string buf))
    (labels ((parse ()
               (if non-base-str (values buf)
                   (coerce buf 'simple-base-string))))
      (dotimes (n len (parse))
        (let ((c (schar buf n)))
         (cond
          ;; NB: #\NON-BREAKING_HYPHEN interpreted as #\-
           #+:asn.1-unicode
           ((char= c (mk-lf (code-char #x2011)))
            (setf (schar buf n) #\-))
           ((typep c 'base-char)) ;; no-op
           (t
            (setq non-base-str t))))))))


;; (typep (simplify-identifier "frob") 'simple-base-string)
;; =>T

#+NIL
(eval-when ()
  #+asn.1-unicode
  (let ((s  (make-string 3 :initial-element #\a)))
    ;; NB: #\NON-BREAKING_HYPHEN
    (setf (schar s 1) (code-char #x2011))
    (setq s (simplify-identifier s))
    (values s (aref s 1)))
  ;; => "a-a", #\-

)
