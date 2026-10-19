;;;; mime.lisp --- one RFC 5322 message, read: its headers, the words, the files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure. The raw message is held as Latin-1, one character per octet, so
;;;; splitting it on boundaries never cuts a multibyte sequence; a part's own
;;;; charset decodes it only at the leaf.

(in-package #:nodecode-channel-email)

(defstruct (mail (:copier nil))
  "One parsed message. HEADERS is an alist of (downcased-name . unfolded raw
value), in the order the message carries them -- the topmost first, so the
last hop's stamps come before anything a sender wrote; TEXT the words a person
wrote, decoded (an HTML-only message's, as text); HTML its first HTML body
part as written, or NIL; ATTACHMENTS a list of (:filename :media-type
:octets)."
  (headers '() :type list)
  (text "" :type string)
  (html nil :type (or null string))
  (attachments '() :type list))

;;; --- octets and text ------------------------------------------------------------

(defun octets-latin1 (octets)
  (babel:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :encoding :latin-1))

(defun latin1-octets (string)
  (babel:string-to-octets string :encoding :latin-1))

(defun charset-encoding (charset)
  "The babel encoding a MIME CHARSET names: UTF-8 when it is unnamed, ASCII
or unknown -- ASCII is UTF-8's subset, and an unknown name decodes best as the
commonest charset."
  (let ((name (and charset (string-downcase (string-trim " \"'" charset)))))
    (cond ((or (null name) (string= name "")
               (member name '("us-ascii" "ascii" "utf8" "utf-8" "unknown-8bit") :test #'string=))
           :utf-8)
          ((member name '("iso-8859-1" "latin1" "latin-1") :test #'string=) :latin-1)
          ((member name '("windows-1252" "cp1252") :test #'string=) :cp1252)
          (t (let ((key (find-symbol (string-upcase name) :keyword)))
               (if (and key (ignore-errors (babel-encodings:get-character-encoding key)))
                   key
                   :utf-8))))))

(defun decode-octets (octets charset)
  "OCTETS as text in CHARSET, a bad sequence read as the replacement
character rather than refused: a person's words are never dropped for one
broken byte."
  (let ((babel-encodings:*suppress-character-coding-errors* t))
    (handler-case (babel:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                                          :encoding (charset-encoding charset))
      (error () (octets-latin1 octets)))))

(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Return #\Newline)))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Return #\Newline) string))

(defun split-lines (text)
  "TEXT's lines, LF or CRLF ended, without their ends."
  (let ((lines '()) (start 0))
    (loop for end = (position #\Newline text :start start)
          do (let ((line (subseq text start (or end (length text)))))
               (push (if (and (plusp (length line))
                              (char= (char line (1- (length line))) #\Return))
                         (subseq line 0 (1- (length line)))
                         line)
                     lines))
             (if end (setf start (1+ end)) (return)))
    (nreverse lines)))

(defun join-lines (lines)
  (format nil "~{~a~^~%~}" lines))

;;; --- transfer encodings ---------------------------------------------------------

(defun base64-octets (text)
  "TEXT's base64 as octets, whatever is not the alphabet skipped and a short
tail padded: mailers wrap, and some truncate."
  (let* ((clean (remove-if-not (lambda (c) (or (alphanumericp c) (find c "+/"))) text))
         (clean (subseq clean 0 (- (length clean) (if (= 1 (mod (length clean) 4)) 1 0))))
         (padded (concatenate 'string clean
                              (make-string (mod (- 4 (mod (length clean) 4)) 4) :initial-element #\=))))
    (handler-case (cl-base64:base64-string-to-usb8-array padded)
      (error () (make-array 0 :element-type '(unsigned-byte 8))))))

(defun quoted-printable-octets (text)
  "TEXT's quoted-printable as octets: =XX a byte, a trailing = a soft break."
  (let ((out (make-array (length text) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond ((char/= c #\=)
                      (vector-push (logand (char-code c) 255) out)
                      (incf i))
                     ((and (< (+ i 2) n) (digit-char-p (char text (1+ i)) 16)
                           (digit-char-p (char text (+ i 2)) 16))
                      (vector-push (parse-integer text :start (1+ i) :end (+ i 3) :radix 16) out)
                      (incf i 3))
                     ((and (< (1+ i) n) (char= (char text (1+ i)) #\Newline))
                      (incf i 2))
                     ((and (< (+ i 2) n) (char= (char text (1+ i)) #\Return)
                           (char= (char text (+ i 2)) #\Newline))
                      (incf i 3))
                     (t (vector-push (char-code #\=) out)
                        (incf i)))))
    (coerce out '(vector (unsigned-byte 8)))))

(defun transfer-decode (body encoding)
  (let ((encoding (string-downcase (trim (or encoding "")))))
    (cond ((string= encoding "base64") (base64-octets body))
          ((string= encoding "quoted-printable") (quoted-printable-octets body))
          (t (latin1-octets body)))))

;;; --- headers --------------------------------------------------------------------

(defun split-entity (raw)
  "RAW's head and body, split at the first empty line."
  (let ((crlf (search (coerce '(#\Return #\Newline #\Return #\Newline) 'string) raw))
        (lf (search (coerce '(#\Newline #\Newline) 'string) raw)))
    (cond ((and crlf (or (null lf) (< crlf lf)))
           (values (subseq raw 0 crlf) (subseq raw (+ crlf 4))))
          (lf (values (subseq raw 0 lf) (subseq raw (+ lf 2))))
          (t (values raw "")))))

(defun parse-headers (head)
  "HEAD's fields as (downcased-name . value), folded lines joined."
  (let ((fields '()))
    (dolist (line (split-lines head) (nreverse fields))
      (cond ((and fields (plusp (length line)) (whitespacep (char line 0)))
             (setf (cdar fields) (format nil "~a ~a" (cdar fields) (trim line))))
            (t (let ((colon (position #\: line)))
                 (when (and colon (plusp colon))
                   (push (cons (string-downcase (trim (subseq line 0 colon)))
                               (trim (subseq line (1+ colon))))
                         fields))))))))

(defun field (headers name)
  (cdr (assoc name headers :test #'string=)))

(defun header (mail name)
  "MAIL's first NAME field (downcased), raw, or NIL."
  (field (mail-headers mail) name))

(defun header-values (mail name)
  "Every NAME field of MAIL, topmost first."
  (loop for (key . value) in (mail-headers mail)
        when (string= key name) collect value))

(defun header-utf8 (value)
  "VALUE read back as UTF-8 when it carries raw 8-bit octets (RFC 6532)."
  (if (some (lambda (c) (> (char-code c) 127)) value)
      (decode-octets (latin1-octets value) "utf-8")
      value))

(defun decode-word (charset encoding data)
  (let ((charset (subseq charset 0 (position #\* charset))))
    (decode-octets (if (char-equal (char encoding 0) #\B)
                       (base64-octets data)
                       (quoted-printable-octets (substitute #\Space #\_ data)))
                   charset)))

(defun decode-words (text)
  "TEXT with its RFC 2047 encoded words decoded; the space between two
adjacent encoded words dropped, as the RFC says."
  (let ((out (make-string-output-stream)) (pos 0) (last-end nil))
    (ppcre:do-scans (start end starts ends "=\\?([^?\\s]+)\\?([BbQq])\\?([^?\\s]*)\\?=" text)
      (let ((gap (subseq text pos start)))
        (unless (and last-end (every #'whitespacep gap))
          (write-string gap out)))
      (write-string (decode-word (subseq text (aref starts 0) (aref ends 0))
                                 (subseq text (aref starts 1) (aref ends 1))
                                 (subseq text (aref starts 2) (aref ends 2)))
                    out)
      (setf pos end last-end end))
    (write-string (subseq text pos) out)
    (get-output-stream-string out)))

(defun header-text (value)
  "A header VALUE as a person reads it: 8-bit and encoded words decoded."
  (and value (decode-words (header-utf8 value))))

(defun split-outside (text separator)
  "TEXT split on SEPARATOR outside quotes, angle brackets and comments."
  (let ((parts '()) (out (make-string-output-stream)) (quoted nil) (angle 0) (paren 0) (escape nil))
    (loop for c across text
          do (cond (escape (write-char c out) (setf escape nil))
                   ((and quoted (char= c #\\)) (write-char c out) (setf escape t))
                   ((char= c #\") (setf quoted (not quoted)) (write-char c out))
                   (quoted (write-char c out))
                   ((char= c #\<) (incf angle) (write-char c out))
                   ((char= c #\>) (setf angle (max 0 (1- angle))) (write-char c out))
                   ((char= c #\() (incf paren) (write-char c out))
                   ((char= c #\)) (setf paren (max 0 (1- paren))) (write-char c out))
                   ((and (char= c separator) (zerop angle) (zerop paren))
                    (push (get-output-stream-string out) parts))
                   (t (write-char c out))))
    (push (get-output-stream-string out) parts)
    (nreverse parts)))

(defun unquote (text)
  (let ((text (trim text)))
    (if (and (>= (length text) 2) (char= (char text 0) #\") (char= (char text (1- (length text))) #\"))
        (with-output-to-string (out)
          (loop with escape = nil
                for c across (subseq text 1 (1- (length text)))
                do (cond (escape (write-char c out) (setf escape nil))
                         ((char= c #\\) (setf escape t))
                         (t (write-char c out)))))
        text)))

(defun parse-params (value)
  "VALUE's main token (downcased) and its parameters as (name . value)."
  (let ((parts (split-outside (or value "") #\;)))
    (values (string-downcase (trim (first parts)))
            (loop for part in (rest parts)
                  for eq = (position #\= part)
                  when eq
                    collect (cons (string-downcase (trim (subseq part 0 eq)))
                                  (unquote (subseq part (1+ eq))))))))

(defun percent-octets (text)
  (let ((out (make-array (length text) :element-type '(unsigned-byte 8) :fill-pointer 0)) (i 0))
    (loop while (< i (length text))
          do (let ((c (char text i)))
               (if (and (char= c #\%) (< (+ i 2) (length text))
                        (digit-char-p (char text (1+ i)) 16) (digit-char-p (char text (+ i 2)) 16))
                   (progn (vector-push (parse-integer text :start (1+ i) :end (+ i 3) :radix 16) out)
                          (incf i 3))
                   (progn (vector-push (logand (char-code c) 255) out) (incf i)))))
    (coerce out '(vector (unsigned-byte 8)))))

(defun param (params name)
  "Parameter NAME of PARAMS: plain, RFC 2231 extended (name*=charset''value)
or continued (name*0, name*1*...), decoded."
  (or (nlk:when-let (plain (cdr (assoc name params :test #'string=)))
        (header-text plain))
      (let* ((pattern (format nil "^~a\\*(?:(\\d+)\\*?|)$" (ppcre:quote-meta-chars name)))
             (pieces (loop for (key . value) in params
                           for index = (ppcre:register-groups-bind (n) (pattern key)
                                         (if n (parse-integer n) 0))
                           when index collect (list index (char= #\* (char key (1- (length key)))) value)))
             (pieces (sort pieces #'< :key #'first)))
        (when pieces
          (let* ((first-value (third (first pieces)))
                 (extended (second (first pieces)))
                 (quote-at (and extended (search "''" first-value)))
                 (charset (if quote-at (subseq first-value 0 (position #\' first-value)) "utf-8"))
                 (octets (apply #'concatenate '(vector (unsigned-byte 8))
                                (loop for (index ext value) in pieces
                                      for text = (if (and (eql index (first (first pieces))) quote-at)
                                                     (subseq value (+ quote-at 2))
                                                     value)
                                      collect (if ext (percent-octets text) (latin1-octets text))))))
            (decode-octets octets charset))))))

;;; --- addresses and ids ----------------------------------------------------------

(defun parse-address (text)
  "TEXT's address, downcased, and the display name beside it -- (values
ADDRESS NAME) -- or NIL when TEXT names no address."
  (let* ((text (header-text (or text "")))
         (lt (position #\< text :from-end t))
         (gt (and lt (position #\> text :start lt))))
    (if (and lt gt)
        (let ((address (string-downcase (trim (subseq text (1+ lt) gt))))
              (name (unquote (subseq text 0 lt))))
          (and (find #\@ address)
               (values address (and (plusp (length name)) name))))
        (let* ((comment (nth-value 1 (ppcre:scan-to-strings "\\(([^()]*)\\)" text)))
               (bare (ppcre:regex-replace-all "\\([^()]*\\)" text ""))
               (token (find-if (lambda (word) (find #\@ word)) (ppcre:split "[\\s,;]+" bare))))
          (and token
               (values (string-downcase (string-trim "<>\"' " token))
                       (and comment (plusp (length (trim (aref comment 0)))) (trim (aref comment 0)))))))))

(defun parse-address-list (text)
  "TEXT's addresses as (address . name), in order."
  (loop for part in (split-outside (or text "") #\,)
        for (address name) = (multiple-value-list (parse-address part))
        when address collect (cons address name)))

(defun message-ids (text)
  "The <...> message ids TEXT names, in order."
  (and text (ppcre:all-matches-as-strings "<[^<>\\s]+>" text)))

;;; --- the body -------------------------------------------------------------------

(defun multipart-parts (body boundary)
  "BODY's parts between BOUNDARY's delimiter lines."
  (if (or (null boundary) (string= boundary ""))
      (list body)
      (let ((open (concatenate 'string "--" boundary))
            (close (concatenate 'string "--" boundary "--"))
            (parts '()) (out nil) (started nil))
        (dolist (line (split-lines body))
          (let ((bare (string-right-trim '(#\Space #\Tab) line)))
            (cond ((string= bare close)
                   (when out (push (get-output-stream-string out) parts))
                   (setf out nil)
                   (return))
                  ((string= bare open)
                   (when out (push (get-output-stream-string out) parts))
                   (setf out (make-string-output-stream) started nil))
                  (out (when started (write-char #\Newline out))
                       (write-string line out)
                       (setf started t)))))
        (when out (push (get-output-stream-string out) parts))
        (nreverse parts))))

(defun walk-entity (headers body depth leaf)
  "Call LEAF on each leaf entity of HEADERS + BODY: (type params headers body)."
  (multiple-value-bind (type params) (parse-params (or (field headers "content-type") "text/plain"))
    (if (and (uiop:string-prefix-p "multipart/" type) (< depth 10))
        (dolist (part (multipart-parts body (cdr (assoc "boundary" params :test #'string=))))
          (multiple-value-bind (head part-body) (split-entity part)
            (walk-entity (parse-headers head) part-body (1+ depth) leaf)))
        (funcall leaf (if (string= type "") "text/plain" type) params headers body))))

(defun entity-filename (type)
  (let ((subtype (subseq type (1+ (or (position #\/ type) -1)))))
    (cond ((string= type "message/rfc822") "message.eml")
          ((member subtype '("jpeg" "pjpeg") :test #'string=) "image.jpg")
          ((or (string= subtype "") (string= subtype "octet-stream")) "attachment.bin")
          (t (format nil "~a.~a" (if (uiop:string-prefix-p "image/" type) "image" "attachment")
                     (remove-if-not (lambda (c) (or (alphanumericp c) (char= c #\-))) subtype))))))

(defun parse-message (raw)
  "RAW -- octets, or a string one character per octet -- as a MAIL: the plain
text parts joined (the HTML part, as text, when there is none), every other
leaf a file."
  (let ((raw (if (stringp raw) raw (octets-latin1 raw))))
    (multiple-value-bind (head body) (split-entity raw)
      (let ((headers (parse-headers head)) (plain '()) (html nil) (files '()))
        (walk-entity
         headers body 0
         (lambda (type params part-headers part-body)
           (multiple-value-bind (disposition dparams)
               (parse-params (or (field part-headers "content-disposition") ""))
             (let ((filename (or (param dparams "filename") (param params "name")))
                   (octets (transfer-decode part-body (field part-headers "content-transfer-encoding"))))
               (if (and (member type '("text/plain" "text/html") :test #'string=)
                        (not (string= disposition "attachment"))
                        (null filename))
                   (let ((text (decode-octets octets (cdr (assoc "charset" params :test #'string=)))))
                     (if (string= type "text/plain")
                         (push text plain)
                         (unless html (setf html text))))
                   (push (list :filename (or filename (entity-filename type))
                               :media-type type
                               :octets octets)
                         files))))))
        (make-mail :headers headers
                   :html (and html (remove #\Return html))
                   :text (remove #\Return
                          (cond (plain (trim (format nil "~{~a~^~%~%~}" (mapcar #'trim (reverse plain)))))
                               (html (html-text html))
                               (t "")))
                   :attachments (nreverse files))))))

;;; --- HTML and quoted replies ----------------------------------------------------

(defun decode-entities (text)
  (ppcre:regex-replace-all
   "&(#[0-9]+|#[xX][0-9a-fA-F]+|[a-zA-Z]+);" text
   (lambda (match name)
     (cond ((char= (char name 0) #\#)
            (let ((code (if (char-equal (char name 1) #\x)
                            (parse-integer name :start 2 :radix 16 :junk-allowed t)
                            (parse-integer name :start 1 :junk-allowed t))))
              (if (and code (< 0 code char-code-limit)) (string (code-char code)) match)))
           (t (let ((entry (assoc name '(("nbsp" . " ") ("amp" . "&") ("lt" . "<") ("gt" . ">")
                                         ("quot" . "\"") ("apos" . "'") ("hellip" . "…")
                                         ("mdash" . "—") ("ndash" . "–") ("rsquo" . "’")
                                         ("lsquo" . "‘") ("rdquo" . "”") ("ldquo" . "“"))
                                  :test #'string-equal)))
                (if entry (cdr entry) match)))))
   :simple-calls t))

(defun collapse-blank-lines (text)
  (trim (ppcre:regex-replace-all "\\n[ \\t]*(\\n[ \\t]*)+\\n" 
                                 (join-lines (mapcar (lambda (l) (string-right-trim '(#\Space #\Tab) l))
                                                     (split-lines text)))
                                 (format nil "~%~%"))))

(defun html-text (html)
  "HTML as the text a person reads: the quoted thread under a reply cut,
blocks as lines, tags dropped, entities decoded."
  (let* ((cut (ppcre:scan "(?i)<div[^>]*class=\"?[^\">]*gmail_quote|<blockquote|id=\"?(divRplyFwdMsg|appendonsend)" html))
         (s (if cut (subseq html 0 cut) html))
         (s (ppcre:regex-replace-all "(?is)<(script|style|head|title)\\b.*?</\\1\\s*>" s ""))
         (s (ppcre:regex-replace-all "(?s)\\s+" s " "))
         (s (ppcre:regex-replace-all "(?i)<br\\s*/?>" s (string #\Newline)))
         (s (ppcre:regex-replace-all "(?i)</?(p|div|li|tr|table|h[1-6]|ul|ol)\\b[^>]*>" s (string #\Newline)))
         (s (ppcre:regex-replace-all "(?s)<[^>]*>" s "")))
    (collapse-blank-lines (decode-entities s))))

(defparameter +attribution+
  "(?i)^\\s*(on|op|am|le|el|il|den|em|w dniu)\\s.+(wrote|schreef|schrieb|écrit|escribió|ha scritto|skrev|escreveu|napisał)\\b.*:\\s*$"
  "The line a mail client writes above the message it quotes, in the languages
most seen.")

(defparameter +divider+
  "(?i)^\\s*(-{2,}\\s*(original message|oorspronkelijk bericht|ursprüngliche nachricht|message d'origine|mensaje original)\\s*-{2,}|_{20,})\\s*$"
  "The line Outlook-style clients write above the message they quote.")

(defun quoted-line-p (line)
  (let ((bare (string-left-trim '(#\Space #\Tab) line)))
    (and (plusp (length bare)) (char= (char bare 0) #\>))))

(defun strip-quoted-reply (text)
  "TEXT without the earlier message a reply quotes under itself: cut at the
client's attribution or divider line, or at a quoted block that runs to the
end. A reply written between the quoted lines keeps them all, since its
words are among them; what is left is never empty while TEXT was not."
  (let* ((lines (coerce (split-lines text) 'vector))
         (n (length lines))
         (cut n))
    (flet ((interleaved-after (i)
             (let ((later (loop for j from (1+ i) below n
                                for line = (aref lines j)
                                unless (string= "" (trim line)) collect line)))
               (and (some #'quoted-line-p later)
                    (notevery #'quoted-line-p later)))))
      (loop for i from 0 below n
            for line = (aref lines i)
            for joined = (if (< (1+ i) n) (format nil "~a ~a" line (aref lines (1+ i))) line)
            for wrapped = (and (not (ppcre:scan +attribution+ line)) (ppcre:scan +attribution+ joined))
            when (and (or (ppcre:scan +attribution+ line) wrapped (ppcre:scan +divider+ line))
                      (not (interleaved-after (if wrapped (1+ i) i))))
              do (setf cut i) (return))
      (when (= cut n)
        (let ((k n))
          (loop for i from (1- n) downto 0
                for line = (aref lines i)
                while (or (quoted-line-p line) (string= "" (trim line)))
                do (setf k i))
          (when (and (< k n) (some #'quoted-line-p (subseq lines k)))
            (setf cut k))))
      (let ((kept (trim (join-lines (coerce (subseq lines 0 cut) 'list)))))
        (if (string= kept "") (trim text) kept)))))
