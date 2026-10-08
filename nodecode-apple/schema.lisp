;;;; schema.lisp --- tool schemas in the dialect Foundation Models' GenerationSchema decodes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi's packages/ai/src/utils/schema/foundation-models.ts
;;;; (toFoundationModelsSchema, decodeFoundationModelsArguments) and
;;;; dereference.ts (dereferenceJsonSchema).
;;;;
;;;; The decoder is strict: every object needs `title', `x-order' (which also
;;;; selects the generated properties), `required' and `additionalProperties:
;;;; false'; an `anyOf' needs a `title'; a node must carry `type', `const',
;;;; `$ref' or `anyOf'. What it cannot express -- free-form maps, open `{}'
;;;; subschemas, tuples, mixed-type enums, `allOf' -- is generated as a
;;;; JSON-encoded string instead, and DECODE-ARGUMENTS parses those back, so a
;;;; tool receives the arguments its schema describes.
;;;;
;;;; Decoded JSON here: an object a hash table, an array a vector, true T,
;;;; false NIL (told from absent by GETHASH's second value), null :NULL.

(in-package #:nodecode-apple)

(defparameter +encoded-suffix+ "(JSON-encoded value)"
  "What an encoded property's description ends with.")

(defun merged (&rest objects)
  "A fresh object with the members of OBJECTS, later ones over earlier."
  (let ((out (make-hash-table :test 'equal)))
    (dolist (object objects out)
      (when (hash-table-p object)
        (maphash (lambda (key value) (setf (gethash key out) value)) object)))))

(defun without (object &rest keys)
  "A copy of OBJECT without KEYS."
  (let ((out (merged object)))
    (dolist (key keys out) (remhash key out))))

(defun title-of (name)
  "NAME with every character but [A-Za-z0-9_] an underscore."
  (map 'string (lambda (char) (if (or (and (alphanumericp char) (< (char-code char) 128)) (char= char #\_)) char #\_))
       name))

(defun description-of (node)
  (and (hash-table-p node) (nlk:json-value node :string "description")))

(defun member-p (node key)
  "Whether NODE has KEY at all, whatever its value (false included)."
  (and (hash-table-p node) (nth-value 1 (gethash key node))))

;;; --- $ref ----------------------------------------------------------------------------

(defun local-ref (ref root)
  "The definition a local REF (#/$defs/Name, #/definitions/Name) names in ROOT, or NIL."
  (ppcre:register-groups-bind (block name) ("^#/(\\$defs|definitions)/(.+)$" ref)
    (let ((resolved (nlk:json-value root :object block name)))
      resolved)))

(defun dereference-node (node root visiting)
  (cond ((and (vectorp node) (not (stringp node)))
         (map 'vector (lambda (item) (dereference-node item root visiting)) node))
        ((not (hash-table-p node)) node)
        ((stringp (gethash "$ref" node))
         (let ((ref (gethash "$ref" node)))
           (if (member ref visiting :test #'equal)
               (make-hash-table :test 'equal)
               (let ((resolved (local-ref ref root)))
                 (if (null resolved)
                     node
                     (let ((inlined (dereference-node resolved root (cons ref visiting))))
                       (if (or (= 1 (hash-table-count node)) (not (hash-table-p inlined)))
                           inlined
                           (without (merged inlined node) "$ref"))))))))
        (t (let ((out (make-hash-table :test 'equal)))
             (maphash (lambda (key value)
                        (unless (member key '("$defs" "definitions") :test #'equal)
                          (setf (gethash key out) (dereference-node value root visiting))))
                      node)
             out))))

(defun dereference (schema)
  "SCHEMA with its local $refs inlined and its definitions dropped; a cycle
is broken with {} (dereferenceJsonSchema)."
  (if (and (hash-table-p schema) (or (member-p schema "$defs") (member-p schema "definitions")))
      (dereference-node schema schema '())
      schema))

;;; --- lowering ----------------------------------------------------------------------------

(defun schema-object (title properties required description)
  "An object node as the decoder demands one."
  (nlk:json-object "type" "object"
                   "title" title
                   "properties" properties
                   "required" (coerce required 'vector)
                   "x-order" (coerce (loop for key being the hash-keys of properties collect key) 'vector)
                   "additionalProperties" nil
                   :opt "description" description))

(defun lower (node title path encoded)
  "NODE lowered, or NIL when it is not expressible. ENCODED, a box (a cons
whose car collects paths) or NIL, says whether a part may fall back to an
encoded string."
  (when (hash-table-p node)
    (let ((lowered (lower-shape node title path encoded))
          (description (description-of node)))
      (when (and lowered description (not (member-p lowered "description")))
        (setf (gethash "description" lowered) description))
      lowered)))

(defun string-literals (node)
  "The string values NODE allows as a const or an all-string enum, or NIL."
  (cond ((not (hash-table-p node)) nil)
        ((stringp (gethash "const" node)) (list (gethash "const" node)))
        ((let ((enum (nlk:json-value node :array "enum")))
           (and enum (every #'stringp enum) (coerce enum 'list))))))

(defun lower-shape (node title path encoded)
  (let ((enum (nlk:json-value node :array "enum"))
        (branches (concatenate 'list (nlk:json-array node "anyOf") (nlk:json-array node "oneOf")))
        (type (gethash "type" node)))
    (cond
      ((member-p node "const")
       (and (stringp (gethash "const" node)) (nlk:json-object "const" (gethash "const" node))))
      (enum (and (plusp (length enum)) (every #'stringp enum)
                 (nlk:json-object "type" "string" "enum" enum)))
      (branches (lower-union node branches title path encoded))
      ((member-p node "allOf")
       (let ((all (nlk:json-array node "allOf")))
         (and (= 1 (length all)) (hash-table-p (aref all 0))
              (lower (merged (without node "allOf") (aref all 0)) title path encoded))))
      ((and (vectorp type) (not (stringp type)))
       (let ((types (remove-duplicates (remove "null" (coerce type 'list) :test #'equal)
                                       :test #'equal :from-end t)))
         (cond ((null types) (nlk:json-object "type" "null"))
               ((null (rest types)) (lower-shape (merged node (nlk:json-object "type" (first types))) title path encoded))
               (t (lower-union node (mapcar (lambda (one) (merged node (nlk:json-object "type" one))) types)
                               title path encoded)))))
      (t
       (let ((type (or type (and (hash-table-p (gethash "properties" node)) "object"))))
         (cond ((member type '("string" "boolean" "null") :test #'equal) (nlk:json-object "type" type))
               ((member type '("integer" "number") :test #'equal)
                (nlk:json-object "type" type
                                 :opt "minimum" (nlk:json-value node :number "minimum")
                                 :opt "maximum" (nlk:json-value node :number "maximum")))
               ((equal type "array") (lower-array node title path encoded))
               ((equal type "object") (lower-object node title path encoded))))))))

(defun lower-union (node branches title path encoded)
  (let* ((shared (without node "anyOf" "oneOf" "type"))
         (options (remove-if (lambda (branch) (and (hash-table-p branch) (equal "null" (gethash "type" branch))))
                             (coerce branches 'list))))
    (cond ((null options) (nlk:json-object "type" "null"))
          ((null (rest options))
           (and (hash-table-p (first options)) (lower (merged shared (first options)) title path encoded)))
          ((every #'string-literals options)
           (nlk:json-object "type" "string"
                            "enum" (coerce (remove-duplicates (mapcan #'string-literals options)
                                                              :test #'equal :from-end t)
                                           'vector)))
          (t (let ((any '()))
               (loop for option in options
                     for index from 0
                     for lowered = (lower (if (hash-table-p option) (merged shared option) option)
                                          (format nil "~a_~d" title index) path nil)
                     do (unless lowered (return-from lower-union nil))
                        (remhash "description" lowered)
                        (push lowered any))
               (nlk:json-object "title" title "anyOf" (coerce (nreverse any) 'vector)))))))

(defun lower-array (node title path encoded)
  (unless (or (nlk:json-value node :array "items") (nlk:json-value node :array "prefixItems"))
    (let ((items (child (if (member-p node "items") (gethash "items" node) t)
                        (format nil "~a_item" title) (append path (list "*")) encoded)))
      (and items
           (nlk:json-object "type" "array" "items" items
                            :opt "minItems" (nlk:json-value node :number "minItems")
                            :opt "maxItems" (nlk:json-value node :number "maxItems"))))))

(defun lower-object (node title path encoded)
  (let* ((properties (or (nlk:json-value node :object "properties") (make-hash-table :test 'equal)))
         (closed (and (member-p node "additionalProperties") (null (gethash "additionalProperties" node))))
         (lowered (make-hash-table :test 'equal)))
    ;; a property-less object that is not explicitly closed is a free-form map
    (unless (and (zerop (hash-table-count properties)) (not closed))
      (loop for key being the hash-keys of properties using (hash-value property)
            for child = (child property (format nil "~a_~a" title (title-of key)) (append path (list key)) encoded)
            do (if child (setf (gethash key lowered) child) (return-from lower-object nil)))
      (schema-object title lowered
                     (remove-if-not (lambda (key) (and (stringp key) (nth-value 1 (gethash key lowered))))
                                    (coerce (nlk:json-array node "required") 'list))
                     nil))))

(defun child (node title path encoded)
  "NODE lowered, else -- where ENCODED allows it -- a string the model
fills with NODE's value JSON-encoded, PATH recorded for DECODE-ARGUMENTS."
  (or (lower node title path encoded)
      (when encoded
        (push path (car encoded))
        (let ((description (description-of node)))
          (nlk:json-object "type" "string"
                           "description" (if description
                                             (format nil "~a ~a" description +encoded-suffix+)
                                             +encoded-suffix+))))))

(defun foundation-schema (schema name)
  "(values SCHEMA ENCODED-PATHS): tool NAME's parameters SCHEMA in the
decoder's dialect, and the argument paths the model emits JSON-encoded
(toFoundationModelsSchema)."
  (let* ((box (list nil))
         (root (dereference schema))
         (lowered (lower root (title-of name) '() box)))
    (if (equal "object" (nlk:json-value lowered :string "type"))
        (values lowered (reverse (car box)))
        ;; only objects are tool parameters; an open root takes no arguments
        (values (schema-object (title-of name) (make-hash-table :test 'equal) '() (description-of root)) '()))))

;;; --- decoding the arguments ---------------------------------------------------------------

(defun parse-encoded (value)
  "VALUE parsed when it is a string holding JSON, else itself."
  (if (stringp value)
      (handler-case (nlk:decode-json value :whole t) (error () value))
      value))

(defun decode-at (container path)
  (let ((key (first path)) (last (null (rest path))))
    (cond ((equal key "*")
           (when (and (vectorp container) (not (stringp container)))
             (dotimes (index (length container))
               (if last
                   (setf (aref container index) (parse-encoded (aref container index)))
                   (decode-at (aref container index) (rest path))))))
          ((and (hash-table-p container) (nth-value 1 (gethash key container)))
           (if last
               (setf (gethash key container) (parse-encoded (gethash key container)))
               (decode-at (gethash key container) (rest path)))))))

(defun decode-arguments (arguments paths)
  "ARGUMENTS (an object) with the values at PATHS parsed back from their JSON
encoding (decodeFoundationModelsArguments)."
  (dolist (path paths arguments)
    (decode-at arguments path)))
