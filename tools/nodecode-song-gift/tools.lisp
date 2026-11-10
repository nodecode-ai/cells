;;;; tools.lisp --- the four tools, what each sends, and what it answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was a manifest over one hosted MCP server, and that server
;;;; advertises four read-only tools: list_occasions, list_styles,
;;;; guide_story_and_lyrics and get_pricing_options. This cell keeps the four
;;;; by their own names -- the server's schema is not pinned in the plugin's
;;;; repository, so each tool sends only the members the call named, and any
;;;; other argument the server grows goes through `extra' unchanged.
;;;;
;;;; Every tool answers text: the occasion names, style names and guidance
;;;; are what the model reads, and they stay where it can use them.

(in-package #:nodecode-song-gift)

(defun bolt (&rest pairs)
  "A JSON object from PAIRS (KEY VALUE ...), leaving out every NIL VALUE: a
request carries only what the call asked for."
  (loop for (key value) on pairs by #'cddr
        when value append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

(defun merge-extra (object extra)
  "OBJECT with EXTRA's members set on it, EXTRA a decoded JSON object or NIL."
  (when (hash-table-p extra)
    (maphash (lambda (key value)
               (unless (eq value :null) (setf (gethash key object) value)))
             extra))
  object)

(defun array-p (value)
  "Whether VALUE is a JSON array (a vector that is not a string)."
  (and (vectorp value) (not (stringp value))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a ... (~d more characters)"
                   (subseq text 0 limit) (- (length text) limit)))))

(defun scalar-text (value limit)
  "VALUE -- a JSON scalar -- as one line."
  (cond ((stringp value) (clip value limit))
        ((null value) "false")
        ((eq :null value) "null")
        ((eq t value) "true")
        ((integerp value) (format nil "~d" value))
        ((numberp value) (format nil "~f" value))
        (t (format nil "~a" value))))

(defun nested-p (value)
  "Whether VALUE is a JSON object or a non-empty array: it gets its own lines."
  (or (hash-table-p value) (and (array-p value) (plusp (length value)))))

(defun members-of (object)
  "OBJECT's members as (NAME . VALUE) pairs."
  (let ((members '()))
    (when (hash-table-p object)
      (maphash (lambda (key value) (push (cons key value) members)) object))
    (nreverse members)))

(defun format-value (value stream indent limit)
  "VALUE -- any decoded JSON -- laid out as indented text."
  (let ((pad (make-string indent :initial-element #\Space)))
    (cond
      ((hash-table-p value)
       (let ((members (members-of value)))
         (if (null members)
             (format stream "~a{}~%" pad)
             (dolist (member members)
               (format stream "~a~a: " pad (car member))
               (if (nested-p (cdr member))
                   (progn (terpri stream) (format-value (cdr member) stream (+ indent 2) limit))
                   (format stream "~a~%" (scalar-text (cdr member) limit)))))))
      ((array-p value)
       (if (zerop (length value))
           (format stream "~a(none)~%" pad)
           (loop for entry across value
                 for index from 1
                 do (format stream "~a~d. " pad index)
                    (if (nested-p entry)
                        (progn (terpri stream) (format-value entry stream (+ indent 3) limit))
                        (format stream "~a~%" (scalar-text entry limit))))))
      (t (format stream "~a~a~%" pad (scalar-text value limit))))))

(defun result-text (result)
  "RESULT -- an MCP tool result -- as its text content blocks joined."
  (let ((content (nlk:json-value result :array "content")))
    (and (array-p content)
         (format nil "~{~a~^~%~}"
                 (loop for block across content
                       when (and (hash-table-p block)
                                 (equal "text" (nlk:json-value block :text "type")))
                         collect (or (nlk:json-value block :text "text") ""))))))

(defun result-object (result)
  "RESULT's structured content, else its text when that text is one JSON value."
  (or (nlk:json-value result :object "structuredContent")
      (let* ((text (result-text result))
             (value (and text
                         (plusp (length (nlk:trimmed text)))
                         (handler-case (nlk:decode-json text) (error () nil)))))
        (and (or (hash-table-p value) (array-p value)) value))))

(defun answer (result)
  "RESULT -- an MCP tool result -- as what the model reads: a JSON answer laid
out as indented members, plain text as itself, an error marked as one."
  (let ((object (result-object result))
        (text (result-text result)))
    (concatenate 'string
                 (if (nlk:json-value result :boolean "isError") "ERROR: " "")
                 (cond
                   (object (with-output-to-string (stream)
                             (format-value object stream 0 (setting :max-chars))))
                   (text (clip text (* 8 (setting :max-chars))))
                   (t "the Custom Song Gift catalog answered nothing")))))

;;; --- list_occasions -------------------------------------------------------------

(nlk:define-contract occasions-call
  (:doc "List the gift occasions the Custom Song Gift catalog supports (birthday, anniversary,
wedding, Mother's Day, holidays and the rest), each with the storytelling memories that make a
song for it work. Public and read-only; it creates nothing and charges nothing.")
  ("extra" :object
   :doc "any other argument the catalog's own tool takes, by its own name"))

(defun occasions-tool (call)
  "list_occasions: the occasion catalog, as text."
  (answer (call-tool "list_occasions" (merge-extra (nlk:json-object) (occasions-call-extra call)))))

;;; --- list_styles -----------------------------------------------------------------

(nlk:define-contract styles-call
  (:doc "Compare the musical styles the studio records in -- acoustic, piano ballad, modern pop,
country, R&B, lofi, lullaby and the others -- with the mood each carries, so a gift song's style
can be chosen on purpose. Name an occasion to ask what suits it, or leave it out for the whole
list.")
  ("occasion" :string
   :doc "the occasion to answer for, such as birthday, anniversary, wedding or mothers_day")
  ("extra" :object
   :doc "any other argument the catalog's own tool takes, by its own name"))

(defun styles-tool (call)
  "list_styles: the style catalog, or one occasion's, as text."
  (answer (call-tool "list_styles"
                      (merge-extra (bolt "occasion" (nonempty (styles-call-occasion call)))
                                   (styles-call-extra call)))))

;;; --- guide_story_and_lyrics ------------------------------------------------------

(nlk:define-contract guide-call
  (:doc "Practical storytelling guidance for turning real memories into a song: how to name the
person, which sensory details to gather, what a chorus has to say, and how to structure the
verses for the occasion. Answers advice, not lyrics written for you; pass what the operator has
already told you so the advice fits it.")
  ("occasion" :string :doc "the occasion the song is for")
  ("style" :string :doc "the musical style the song is going to be in")
  ("recipient" :string :doc "who the song is for, as the operator describes them")
  ("memories" :array :of :string
   :doc "the real moments, places, habits or phrases the operator mentioned, one per element")
  ("extra" :object :doc "any other argument the catalog's own tool takes, by its own name"))

(defun memories-of (call)
  "The call's memories as a JSON array, or NIL when it named none."
  (let ((memories (guide-call-memories call)))
    (and (array-p memories)
         (plusp (length memories))
         (coerce (remove-if-not #'stringp (coerce memories 'list)) 'vector))))

(defun guide-tool (call)
  "guide_story_and_lyrics: storytelling advice for one gift song, as text."
  (answer (call-tool "guide_story_and_lyrics"
                      (merge-extra (bolt "occasion" (nonempty (guide-call-occasion call))
                                         "style" (nonempty (guide-call-style call))
                                         "recipient" (nonempty (guide-call-recipient call))
                                         "memories" (memories-of call))
                                   (guide-call-extra call)))))

;;; --- get_pricing_options ---------------------------------------------------------

(nlk:define-contract pricing-call
  (:doc "The studio's current prices and what a finished song comes with -- the keepsake page, the
lyrics, the audio download -- as the catalog states them. Answers the catalog, not a checkout: no
payment is taken and nothing is charged from here.")
  ("extra" :object :doc "any other argument the catalog's own tool takes, by its own name"))

(defun pricing-tool (call)
  "get_pricing_options: the prices and deliverables, as text."
  (answer (call-tool "get_pricing_options" (merge-extra (nlk:json-object) (pricing-call-extra call)))))
