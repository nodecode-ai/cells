;;;; cell.lisp --- the advice on the wire fold, the tool, the command, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHAT THE ORIGINAL DID, AND WHERE IT LANDS HERE. The plugin handed 71
;;;; vendor-prefixed regexes to Hermes's secret redactor, which runs over logs,
;;;; tool output and model-bound text; the seam was additive-only, so a format
;;;; could only widen what gets masked. Nodecode has no plugin-supplied
;;;; redaction table, so this cell catches the same text at the seam Nodecode
;;;; does have: WIRE-MESSAGES-FROM-EVENTS folds the durable log into the
;;;; provider-shaped messages of one turn, and the cell advises that fold. The
;;;; user text of every message and every tool result is masked on its way to
;;;; the provider; the system prompt and the assistant's own turns pass byte
;;;; for byte, and the log keeps what was written, so /sessions and every
;;;; transcript still show the token.
;;;;
;;;; The rows in formats.lisp are the original's patterns, unchanged. Each is
;;;; compiled by cl-ppcre once and matched between the boundaries Hermes used
;;;; -- a token glued to a word character is not a token -- and overlapping
;;;; matches over one run of characters collapse to a single mask.
;;;;
;;;; How a masked credential reads is the original's too. Ordinary text keeps
;;;; the first six and last four characters, so a person can still tell which
;;;; credential it was (`PMAK-6...e3b9'); a tool result gets the non-reusable
;;;; sentinel (`<<redacted: HashiCorp Vault service token>>') instead, because
;;;; a file's content is what a model tends to copy back into a config file.
;;;;
;;;; FAIL-OPEN, as the original was: a matcher that throws passes the text
;;;; through, counting the failure and saying it once in the operator's board.
;;;; A redaction is a habit, not a confidentiality guarantee.

(in-package #:nodecode-redaction-pack)

(defparameter +tool-name+ "redact_credentials")

(defparameter +word-chars+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"
  "The characters a credential may not be glued to: Hermes's own boundary,
(?<![A-Za-z0-9_-])(...)(?![A-Za-z0-9_-]), kept so a substring of a longer word
is not a token.")

(defparameter +ends-style+ "ends"
  "The default: a masked credential keeps its first six and last four characters.")

(defparameter +sentinel-style+ "sentinel"
  "The non-reusable mask a tool result gets: the vendor and the token's name, and
no character of the token itself.")

(defparameter +styles+ (list +ends-style+ +sentinel-style+)
  "What the `style' setting may hold.")

(defparameter +part-types+ '("text" "input_text" "output_text")
  "The content-part types whose `text' member carries words a person wrote.")

(defparameter +masked-roles+ '("user" "tool")
  "The folded message roles whose text is masked on the way out: what the
operator wrote, and what a tool handed back. The system prompt and the
assistant's own turns are never read.")

(defvar *active-formats* *formats*
  "The rows the running cell masks: all 71, or the `vendors' subset.")

(defvar *style* +ends-style+
  "How a masked credential reads in the operator's own text.")

(defvar *scanners* (make-hash-table :test #'equal)
  "PATTERN -> the cl-ppcre scanner compiled for it, built once and reused.")

(defvar *last-failure* nil
  "The class name of the last matcher failure, or NIL.")

(defvar *failures* 0
  "How many times the matcher failed since the cell started.")

(defvar *scrubbed* 0
  "How many messages went out with at least one mask in them.")

(defparameter +manual+
  "Every request masks, on its way to the provider, the vendor credentials a
model should not read: 71 formats Hermes does not ship, from a HashiCorp Vault
service token and a Doppler CLI token to a Cloudflare key, a Tailscale key, a
Docker Hub token, an Atlassian or HubSpot token and a Shopify access token. The
user text of every message and every tool result is masked; the system prompt
and the assistant's own turns are not, and the log keeps the originals, so
/sessions and every transcript still show what was written. Ordinary text keeps
the first six and last four characters (`PMAK-6...e3b9'); a tool result gets
`<<redacted: Vault service token>>' instead, so a file's content cannot be
copied back. Nothing leaves for a third party: the patterns are local, and
`vendors' narrows them to the ones you care about.
  /redaction-pack          what is masked, how many messages, the last failure
  /redaction-pack TEXT     the same text as it would leave, with what was found
  redact_credentials        the tool: the same preview for text the model has in hand"
  "What (help :redaction-pack) answers while the cell runs.")

(defun vendor-names (formats)
  "The distinct vendor names FORMATS carry, in the table's order."
  (let ((names '()))
    (dolist (format formats) (pushnew (token-format-vendor format) names :test #'string=))
    (nreverse names)))

(defun formats-for (vendors)
  "The rows VENDORS names, all of them when VENDORS is NIL or empty."
  (if (or (null vendors) (every #'string= vendors '("")))
      *formats*
      (remove-if-not (lambda (format)
                       (member (token-format-vendor format) vendors :test #'string=))
                     *formats*)))

;;; --- finding the credentials ---------------------------------------------------

(defun scanner-for (format)
  "FORMAT's pattern as a scanner: compiled once, then out of the cache."
  (let ((pattern (token-format-pattern format)))
    (or (gethash pattern *scanners*)
        (setf (gethash pattern *scanners*) (ppcre:create-scanner pattern)))))

(defun boundary-p (text start end)
  "Whether TEXT's START..END stands whole: no word character on either side."
  (and (or (zerop start) (null (find (char text (1- start)) +word-chars+)))
       (or (= end (length text)) (null (find (char text end) +word-chars+)))))

(defstruct (credential-span (:constructor make-span (start end format token)))
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (format nil)
  (token "" :type string))

(defun format-spans (text format)
  "Every whole token FORMAT's pattern finds in TEXT, as spans."
  (let ((hits '())
        (positions (ppcre:all-matches (scanner-for format) text)))
    (loop for (start end) on positions by #'cddr
          do (when (boundary-p text start end)
               (push (make-span start end format (subseq text start end)) hits)))
    (nreverse hits)))

(defun span-before-p (a b)
  "Whether A sorts before B: by start, and the longer span first at one start."
  (or (< (credential-span-start a) (credential-span-start b))
      (and (= (credential-span-start a) (credential-span-start b))
           (> (credential-span-end a) (credential-span-end b)))))

(defun merge-spans (spans)
  "SPANS as disjoint spans in text order: matches over one run of characters
are one credential, and the first of them names it."
  (let ((kept '()))
    (dolist (span (sort (copy-list spans) #'span-before-p))
      (let ((last (first kept)))
        (cond ((and last (< (credential-span-start span) (credential-span-end last)))
               (when (> (credential-span-end span) (credential-span-end last))
                 (setf (credential-span-end last) (credential-span-end span))))
              (t (push span kept)))))
    (nreverse kept)))

(defun find-spans (text &optional (formats *active-formats*))
  "Every vendor credential in TEXT, as disjoint spans in text order."
  (merge-spans (loop for format in formats appending (format-spans text format))))

;;; --- how a mask reads ----------------------------------------------------------

(defun ends-mask (token)
  "TOKEN the way the original's ordinary output read it: the first six and
last four characters, the middle gone."
  (if (>= (length token) 12)
      (concatenate 'string (subseq token 0 6) "..." (subseq token (- (length token) 4)))
      "***"))

(defun sentinel-mask (span)
  "SPAN as a value nothing can copy back: the original's sentinel, named for
the vendor and the token."
  (format nil "<<redacted: ~a ~a>>"
          (token-format-vendor (credential-span-format span))
          (token-format-token (credential-span-format span))))

(defun span-mask (span style)
  "SPAN masked in STYLE: ends or sentinel."
  (if (string= style +sentinel-style+)
      (sentinel-mask span)
      (ends-mask (credential-span-token span))))

(defun mask-spans (text spans &optional (style *style*))
  "TEXT with every span in SPANS masked; TEXT itself when there is none."
  (if (null spans)
      text
      (with-output-to-string (out)
        (let ((cursor 0))
          (dolist (span spans)
            (write-string (subseq text cursor (credential-span-start span)) out)
            (write-string (span-mask span style) out)
            (setf cursor (credential-span-end span)))
          (write-string (subseq text cursor) out)))))

(defun redact-text (text &key (style *style*) (formats *active-formats*))
  "TEXT with every vendor credential in it masked: on this machine, with no
service and no model. Never signals: text the matcher chokes on comes back as
it was."
  (if (or (null text) (zerop (length text)))
      text
      (handler-case (mask-spans text (find-spans text formats) style)
        (error (condition) (note-failure condition) text))))

(defun found-summary (spans)
  "One line on what SPANS found, or that nothing was."
  (if (null spans)
      "nothing found: the text comes back as it was"
      (let ((labels '()))
        (dolist (span spans)
          (pushnew (format nil "~a ~a"
                            (token-format-vendor (credential-span-format span))
                            (token-format-token (credential-span-format span)))
                   labels :test #'string=))
        (format nil "masked ~d credential~:p: ~{~a~^, ~}" (length spans) (reverse labels)))))

;;; --- the advice on the fold ----------------------------------------------------

(defun carry-source (from to)
  "Keep FROM's log position on the copy TO, so the editable context still
names it: MESSAGE-SOURCE is the fold's own side table."
  (ignore-errors
    (let ((source (nodecode.evolved::message-source from)))
      (when source (setf (nodecode.evolved::message-source to) source))))
  to)

(defun redact-content (content style)
  "CONTENT -- a string, or an array of parts -- with the words in it masked."
  (cond
    ((stringp content) (redact-text content :style style))
    ((vectorp content)
     (let ((copy (copy-seq content)) (changed nil))
       (dotimes (i (length copy))
         (let ((part (aref copy i)))
           (when (hash-table-p part)
             (let ((text (nlk:json-value part :string "text"))
                   (type (nlk:json-value part :string "type")))
               (when (and text (plusp (length text))
                          (member type +part-types+ :test #'string=))
                 (let ((masked (redact-text text :style style)))
                   (unless (string= masked text)
                     (setf (aref copy i) (nlk:copy-json-object part "text" masked)
                           changed t))))))))
       (if changed copy content)))
    (t content)))

(defun redact-message (message)
  "MESSAGE, a folded provider message: its user or tool text masked, a copy
when that changed anything and MESSAGE itself when it did not."
  (if (not (hash-table-p message))
      message
      (let ((role (nlk:json-value message :string "role"))
            (type (nlk:json-value message :string "type")))
        (if (and (stringp role)
                 (member role +masked-roles+ :test #'string=)
                 (or (null type) (string= type "message"))
                 (nth-value 1 (gethash "content" message)))
            (let* ((content (gethash "content" message))
                   (style (if (string= role "tool") +sentinel-style+ *style*))
                   (masked (redact-content content style)))
              (if (eq masked content)
                  message
                  (carry-source message (nlk:copy-json-object message "content" masked))))
            message))))

(defun redact-messages (messages)
  "MESSAGES with every user and tool text masked, in order."
  (mapcar #'redact-message messages))

(defun note-failure (condition)
  "Count CONDITION and say its class once: no input, no message, no traceback."
  (incf *failures*)
  (let ((class (string-downcase (symbol-name (type-of condition)))))
    (unless (equal class *last-failure*)
      (setf *last-failure* class)
      (ignore-errors
        (nle:notice (format nil "redaction-pack: masking failed (~a); the text goes out unchanged. /redaction-pack says how often." class)
                    :level :warning :key "nodecode-redaction-pack")))))

(defun redact-wire (next &rest args)
  "The advice on the fold: mask the messages it answers with, and hand its
CLEAN-P back untouched -- the covered-prefix cache is licensed by it."
  (multiple-value-bind (messages clean-p) (apply next args)
    (handler-case
        (let ((masked (redact-messages messages)))
          (when (and masked (not (eq masked messages))) (incf *scrubbed*))
          (values masked clean-p))
      (error (condition)
        (note-failure condition)
        (values messages clean-p)))))

;;; --- the tool ------------------------------------------------------------------

(nlk:define-contract redact-credentials-request
  (:doc "Mask the vendor credentials in TEXT here, on this machine, with no service and no model: 71 formats Hermes does not ship, from a HashiCorp Vault service token and a Doppler token to a Cloudflare key, a Tailscale key, a Docker Hub token and a Shopify access token. Ordinary style keeps the first six and last four characters of each, sentinel names the vendor and no character of the token; it answers the masked text and what was found.")
  ("text" :text :required t :doc "the text to mask")
  ("style" :text :default ""
           :doc "ends or sentinel; empty takes the cell's own setting"))

(defun tool-style (style)
  "STYLE as the tool asked for it, the cell's own when it named none or one
this cell does not know."
  (if (and (stringp style) (member style +styles+ :test #'string=))
      style
      *style*))

(defun redact-credentials-tool (request)
  "The redact_credentials tool: the masked text and one line on what was found."
  (let* ((text (redact-credentials-request-text request))
         (style (tool-style (redact-credentials-request-style request)))
         (spans (find-spans text)))
    (format nil "~a~%~%~a" (mask-spans text spans style) (found-summary spans))))

;;; --- /redaction-pack -----------------------------------------------------------

(defun status-text ()
  "What /redaction-pack answers with nothing typed."
  (format nil "redaction-pack: ~d format~:p from ~d vendor~:p masked in the user and tool text of every request, ~a style; ~d message~:p rewritten since the start; nothing leaves for a third party, and the log keeps the originals~@[; last matcher failure: ~a~]."
          (length *active-formats*) (length (vendor-names *active-formats*))
          *style* *scrubbed* *last-failure*))

(defun preview-text (text)
  "TEXT as it would leave, and what was found."
  (let ((spans (find-spans text)))
    (format nil "~a~%~a" (mask-spans text spans *style*) (found-summary spans))))

(defun run-slash (args session-id)
  "The /redaction-pack command: the state, or a preview of ARGS."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (values (if (plusp (length text)) (preview-text text) (status-text)) nil)))

;;; --- the section the cell runs on -----------------------------------------------

(defun canonical-vendor (name)
  "NAME as the table spells it, or NIL when the table has no such vendor."
  (find (nlk:trimmed name) *format-vendors* :test #'string-equal))

(defun read-settings (values table)
  "VALUES from the declaration, with the vendor names read as the table
spells them and a name it does not know refused before the cell starts."
  (declare (ignore table))
  (let ((names (remove-if (lambda (name) (zerop (length (nlk:trimmed name))))
                          (getf values :vendors)))
        (canonical '()))
    (dolist (name names)
      (let ((vendor (canonical-vendor name)))
        (unless vendor
          (nlk:config-error "redaction-pack.vendors names ~s, which is not a vendor this cell masks; it holds ~d of them, such as ~s and ~s"
                            name (length *format-vendors*)
                            (first *format-vendors*) (second *format-vendors*)))
        (pushnew vendor canonical :test #'string=)))
    (setf (getf values :vendors) (nreverse canonical))
    values))

(nlk:define-peripheral redaction-pack :not-running t)

(nle:define-cell redaction-pack
  (:section ("redaction-pack")
    (:guide "nothing needs setting: all 71 formats are masked in the user and tool text of every request; vendors narrows the list, style picks how a mask reads; /redaction-pack previews any text")
    ("vendors" :list
               :doc "mask only these vendors' formats, as the table spells them (HashiCorp Vault, Doppler, Tailscale): empty masks all 71")
    ("style" :choice :options '("ends" "sentinel") :default "ends"
             :doc "how a mask reads in your own text: ends keeps the first six and last four characters, sentinel names the vendor and none of the token"))
  (:settings #'read-settings)
  (:hook 'nodecode.evolved::wire-messages-from-events #'redact-wire)
  (:tool +tool-name+ (nlk:contract 'redact-credentials-request) 'redact-credentials-tool)
  (:command "redaction-pack" 'run-slash
            :description "What is masked, how many messages, or a preview of any text"
            :argument-hint "[TEXT]")
  (:help :redaction-pack
         "redaction-pack: masks 71 vendor credential formats in the text you send" +manual+)
  (:start (lambda ()
            (setf *active-formats* (formats-for (setting :vendors))
                  *style* (or (setting :style) +ends-style+)
                  *failures* 0
                  *last-failure* nil
                  *scrubbed* 0)
            (nle:on-stop (lambda ()
                           (setf *active-formats* *formats*
                                 *style* +ends-style+))))))
