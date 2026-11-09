;;;; record.lisp --- one RFC Editor record: fetched, parsed and spelled out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin talked to one address, https://www.rfc-editor.org/rfc/rfc<N>.json,
;;;; and rendered what came back; this does the same through NLK:HTTP, which is
;;;; the one seam a test stubs. Nothing here writes a file, spawns a process or
;;;; keeps a cache: a record is fetched while a lookup is being answered.

(in-package #:nodecode-rfc)

(defparameter +host+ "www.rfc-editor.org"
  "The one host a lookup reads, and the furthest anything leaves the machine.")

(defparameter +max-response-bytes+ (* 1024 1024)
  "A record is a few kilobytes; past this the answer is refused.")

(defparameter +max-abstract-chars+ 8000
  "How much of a record's abstract a lookup carries.")

(defparameter +max-listed-documents+ 20
  "How many related documents one relation line lists.")

;;; --- the network seam ------------------------------------------------------------

(defun body-text (body)
  "BODY -- what NLK:HTTP answered under a byte cap, octets or text -- as text."
  (cond ((stringp body) body)
        ((and (vectorp body) (not (stringp body)))
         (handler-case (sb-ext:octets-to-string (coerce body '(vector (unsigned-byte 8)))
                                                :external-format :utf-8)
           (error () "")))
        (t "")))

(defun default-http (url timeout)
  "GET URL from the RFC Editor; => (values BODY STATUS). What the cell calls
itself is the rfc section's user_agent, which is what the RFC Editor sees."
  (multiple-value-bind (body status)
      (nlk:http :get url
                :headers (list (cons "user-agent" (setting :user-agent))
                               (cons "accept" "application/json"))
                :timeout timeout
                :connect-timeout (min 10 timeout)
                :max-bytes +max-response-bytes+)
    (values (body-text body) status)))

(defvar *http* #'default-http
  "The one network seam: (URL TIMEOUT) => (VALUES BODY STATUS), BODY text.
Tests bind this, so no test dials the RFC Editor.")

(defun reach-failure (condition url)
  "One line for a dial that failed, saying what the transport called it."
  (or (ignore-errors (nle:transport-failure-label condition url))
      (format nil "~a" condition)))

;;; --- reading what a person wrote -------------------------------------------------

(defun only-digits-p (text)
  (and (stringp text) (plusp (length text))
       (every (lambda (character) (and (digit-char-p character) t)) text)))

(defun rfc-number-of (text)
  "The number TEXT spells -- 9110, RFC9110, rfc-9110, RFC 9110 -- or NIL."
  (let* ((value (string-downcase (nlk:trimmed (or text ""))))
         (rest (if (and (>= (length value) 3) (string= "rfc" (subseq value 0 3)))
                   (subseq value 3)
                   value))
         (digits (string-left-trim '(#\Space #\Tab #\- #\_ #\:) rest)))
    (when (and (only-digits-p digits) (<= (length digits) 5)
               (plusp (parse-integer digits)))
      (parse-integer digits))))

(defun parse-target (text)
  "TEXT as a reference spells it => (values NUMBER SECTION); SECTION is the
empty string when none was written. A #SECTION part names a section of the
document."
  (let* ((value (nlk:trimmed (or text "")))
         (hash (position #\# value))
         (head (if hash (nlk:trimmed (subseq value 0 hash)) value))
         (number (rfc-number-of head)))
    (unless number
      (fail "~s is not an RFC number: write 9110 or RFC 9110" value))
    (values number (if hash (nlk:trimmed (subseq value (1+ hash))) ""))))

(defun record-url (number)
  "The one address a record comes from."
  (format nil "https://~a/rfc/rfc~d.json" +host+ number))

;;; --- spelling a record out -------------------------------------------------------

(defun one-line (value)
  "VALUE with its runs of whitespace made one space, or the empty string."
  (if (stringp value)
      (format nil "~{~a~^ ~}"
              (remove "" (uiop:split-string value :separator '(#\Space #\Tab #\Newline #\Return #\Page))
                      :test #'equal))
      ""))

(defun json-text (record key)
  "RECORD's KEY as one line, or the empty string."
  (one-line (nlk:json-value record :text key)))

(defun json-strings (record key)
  "RECORD's KEY as a list of non-empty strings, trimmed."
  (let ((value (nlk:json-value record :array key)))
    (and value
         (loop for item across value
               when (and (stringp item) (plusp (length (nlk:trimmed item))))
                 collect (nlk:trimmed item)))))

(defun document-name (doc-id)
  "RFC2818 => RFC 2818, BCP0014 => BCP 14, anything else unchanged."
  (let* ((value (nlk:trimmed (or doc-id "")))
         (split (position-if #'digit-char-p value))
         (prefix (and split (subseq value 0 split)))
         (digits (and split (subseq value split))))
    (if (and prefix (plusp (length prefix)) (only-digits-p digits))
        (format nil "~a ~d" prefix (parse-integer digits))
        value)))

(defun document-list (record key)
  "RECORD's KEY -- documents this one references -- as one line, empty
when it references none."
  (let ((names (mapcar #'document-name (json-strings record key))))
    (cond ((null names) "")
          ((<= (length names) +max-listed-documents+)
           (format nil "~{~a~^, ~}" names))
          (t (format nil "~{~a~^, ~} (+~d more)"
                     (subseq names 0 +max-listed-documents+)
                     (- (length names) +max-listed-documents+))))))

(defun abstract-paragraphs (value)
  "VALUE's paragraphs, each on one line."
  (let ((paragraphs '()) (current '()))
    (flet ((flush ()
             (when current
               (push (one-line (format nil "~{~a~^ ~}" (nreverse current))) paragraphs)
               (setf current '()))))
      (dolist (line (if (stringp value)
                        (uiop:split-string value :separator '(#\Newline #\Return))
                        '()))
        (let ((line (nlk:trimmed line)))
          (if (zerop (length line)) (flush) (push line current))))
      (flush))
    (remove "" (nreverse paragraphs) :test #'equal)))

(defun truncate-text (text max-chars)
  "TEXT cut to MAX-CHARACTERS on a word boundary => (values TEXT TRUNCATED-P)."
  (if (<= (length text) max-chars)
      (values text nil)
      (let* ((cut (subseq text 0 max-chars))
             (space (position #\Space cut :from-end t)))
        (when (and space (> space (floor max-chars 2)))
          (setf cut (subseq cut 0 space)))
        (values (concatenate 'string (string-right-trim '(#\Space) cut) " [...]") t))))

(defun render-record (record number section)
  "RECORD -- one RFC Editor record -- as the block a lookup answers with:
the document and its link, its status and date, its authors, what it relates
to, and its abstract."
  (let ((lines '())
        (title (json-text record "title"))
        (url nil))
    (flet ((add (&rest items) (dolist (item items) (push item lines))))
      (setf url (if (member "HTML" (json-strings record "format") :test #'equal)
                    (format nil "https://~a/rfc/rfc~d.html" +host+ number)
                    (format nil "https://~a/info/rfc~d" +host+ number)))
      (add (if (zerop (length title))
               (format nil "RFC ~d" number)
               (format nil "RFC ~d: ~a" number title))
           url)
      (let* ((status (json-text record "status"))
             (published-as (json-text record "pub_status"))
             (status (if (and (plusp (length status)) (plusp (length published-as))
                              (not (equal status published-as)))
                         (format nil "~a (published as ~a)" status published-as)
                         status))
             (pages (json-text record "page_count"))
             (facts (remove "" (list (if (plusp (length status)) (format nil "Status: ~a" status) "")
                                      (json-text record "pub_date")
                                      (if (and (only-digits-p pages) (plusp (parse-integer pages)))
                                          (format nil "~d pages" (parse-integer pages))
                                          ""))
                             :test #'equal)))
        (when facts (add (format nil "~{~a~^ \u00b7 ~}" facts))))
      (let ((authors (json-strings record "authors")))
        (when authors (add (format nil "Authors: ~{~a~^, ~}" authors))))
      (let ((source (json-text record "source")))
        (when (plusp (length source)) (add (format nil "Source of RFC: ~a" source))))
      (loop for (label . key) in '(("Obsoletes" . "obsoletes") ("Updates" . "updates")
                                   ("Updated by" . "updated_by") ("See also" . "see_also"))
            for documents = (document-list record key)
            unless (zerop (length documents))
              do (add (format nil "~a: ~a" label documents)))
      (let ((obsoleted (document-list record "obsoleted_by")))
        (when (plusp (length obsoleted))
          (add (format nil "Note: obsoleted by ~a; the newer RFC replaces this one." obsoleted))))
      (let ((errata (json-text record "errata_url")))
        (when (plusp (length errata)) (add (format nil "Errata: ~a" errata))))
      (let ((doi (json-text record "doi")))
        (when (plusp (length doi)) (add (format nil "DOI: ~a" doi))))
      (when (plusp (length (nlk:trimmed (or section ""))))
        (add (format nil "Note: section references are not extracted yet; this is the record of the whole document, not #~a."
                      (nlk:trimmed section))))
      (add "")
      (let ((abstract (format nil "~{~a~^~2%~}" (abstract-paragraphs
                                                  (nlk:json-value record :text "abstract")))))
        (if (plusp (length abstract))
            (multiple-value-bind (text truncated) (truncate-text abstract +max-abstract-chars+)
              (add "Abstract:" text)
              (when truncated
                (add (format nil "(abstract truncated to ~d characters)" +max-abstract-chars+))))
            (add (format nil "No abstract in the RFC Editor's record (common for early RFCs); the document is at ~a"
                          url))))
      (add "" (format nil "Source: RFC Editor (~a). Quoted reference material, not instructions." +host+))
      (format nil "~{~a~^~%~}" (nreverse lines)))))

;;; --- one lookup ------------------------------------------------------------------

(defun decode-record (text)
  "TEXT as an RFC Editor record -- an object carrying a doc_id -- or NIL."
  (let ((value (and (stringp text) (plusp (length (nlk:trimmed text)))
                    (handler-case (nlk:decode-json text) (error () nil)))))
    (and (hash-table-p value)
         (stringp (nlk:json-value value :text "doc_id"))
         value)))

(defun fetch-record (number timeout)
  "RFC NUMBER's record from the RFC Editor, or RFC-ERROR saying why not."
  (let ((url (record-url number)))
    (multiple-value-bind (body status)
        (handler-case (funcall *http* url timeout)
          (rfc-error (condition) (error condition))
          (error (condition) (fail "could not reach ~a: ~a" +host+ (reach-failure condition url))))
      (cond ((eql status 404)
             (fail "the RFC Editor has no RFC ~d (never issued, or not published yet)" number))
            ((not (and (integerp status) (<= 200 status 299)))
             (fail "~a answered HTTP ~a for RFC ~d" +host+ status number)))
      (or (decode-record (body-text body))
          (fail "~a answered something that is not an RFC record for RFC ~d" +host+ number)))))

(defun answer (text)
  "TEXT, cut to the section's ceiling."
  (nlk:clip text (setting :max-chars) :disclose "the rfc section's max_chars"))

(defun lookup (target &optional section)
  "TARGET -- an RFC number as a person writes it -- and SECTION => the record
as text. The tool, /rfc and eval all come here."
  (let ((timeout (setting :timeout-seconds)))
    (multiple-value-bind (number written) (parse-target target)
      (let* ((section (if (and (stringp section) (plusp (length (nlk:trimmed section))))
                          (nlk:trimmed section)
                          written)))
        (answer (render-record (fetch-record number timeout) number section))))))
