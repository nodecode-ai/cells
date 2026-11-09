;;;; mime.lisp --- what a message is made of, read without a mail library.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A message arrives as octets and is read into an ENTITY: its headers, its
;;;; type and parameters, its transfer encoding, and either its body or its
;;;; children (a multipart). Everything the tools answer is derived from that
;;;; one tree: the text a person reads, the attachment list the model picks a
;;;; part_id from, and the bytes a save writes.
;;;;
;;;; The parts are numbered the way IMAP numbers them (1, 2, 1.1, ...) even
;;;; though the numbering is this file's own: a part_id is only ever read
;;;; back by this file, from the same message.

(in-package #:nodecode-yandex-mail)

;;; --- octets ----------------------------------------------------------------------

(defun octets-index (needle haystack &key (start 0) (test #'=))
  "Where NEEDLE first sits in HAYSTACK at or after START, or NIL."
  (let ((n (length needle)) (h (length haystack)))
    (when (plusp n)
      (loop for i from start to (- h n)
            when (loop for j below n always (funcall test (aref needle j) (aref haystack (+ i j))))
              return i))))

(defun trim-crlf (octets start end)
  "START and END narrowed past a CRLF pair at either edge."
  (flet ((cr (i) (and (< i (length octets)) (= 13 (aref octets i))))
         (lf (i) (and (< i (length octets)) (= 10 (aref octets i)))))
    (loop while (and (< start end) (or (cr start) (lf start))) do (incf start))
    (loop while (and (< start end) (or (cr (1- end)) (lf (1- end)))) do (decf end))
    (values start end)))

;;; --- charsets ---------------------------------------------------------------------

(defparameter +cp1251+
  #(1026 1027 8218 1107 8222 8230 8224 8225 8364 8240 1033 8249 1034 1036 1035 1039
    176 177 8216 8217 8220 8221 8226 8211 8212 8482 1113 8250 1114 1116 1115 1119
    160 1038 1118 1032 164 1168 166 167 1025 1028 8364 171 172 173 174 1031
    176 177 1030 1110 1169 181 182 183 1105 8470 1108 187 1112 1029 1109 1111
    1040 1041 1042 1043 1044 1045 1046 1047 1048 1049 1050 1051 1052 1053 1054 1055
    1056 1057 1058 1059 1060 1061 1062 1063 1064 1065 1066 1067 1068 1069 1070 1071
    1072 1073 1074 1075 1076 1077 1078 1079 1080 1081 1082 1083 1084 1085 1086 1087
    1088 1089 1090 1091 1092 1093 1094 1095 1096 1097 1098 1099 1100 1101 1102 1103)
  "Windows-1251's upper half, the bytes 128-255.")

(defun utf8-decode (octets)
  "OCTETS as UTF-8, a malformed run read as its own bytes rather than refused."
  (with-output-to-string (out)
    (let ((i 0) (n (length octets)))
      (loop while (< i n)
            do (let* ((b (aref octets i))
                      (need (cond ((< b #x80) 0) ((= (logand b #xE0) #xC0) 1)
                                  ((= (logand b #xF0) #xE0) 2) ((= (logand b #xF8) #xF0) 3)
                                  (t nil))))
                 (cond ((null need) (write-char (code-char b) out) (incf i))
                       ((or (= need 0) (> (+ i need) (1- n)))
                        (write-char (code-char b) out) (incf i))
                       (t (let ((code (logand b (ash 1 (- 6 need)) -1))
                                (ok t))
                            (loop for k from 1 to need
                                  for next = (aref octets (+ i k))
                                  do (if (= (logand next #xC0) #x80)
                                         (setf code (logior (ash code 6) (logand next #x3F)))
                                         (setf ok nil)))
                            (cond (ok (write-char (code-char code) out) (incf i (1+ need)))
                                  (t (write-char (code-char b) out) (incf i)))))))))))

(defun decode-charset (octets charset)
  "OCTETS read in CHARSET: UTF-8, Windows-1251, KOI8-R and Latin-1 by name,
everything else tried as UTF-8 and read byte by byte when that is no text."
  (let ((name (string-downcase (or charset ""))))
    (cond ((member name '("" "utf-8" "utf8" "us-ascii" "ascii") :test #'string=)
           (utf8-decode octets))
          ((member name '("windows-1251" "cp1251" "win-1251" "x-cp1251") :test #'string=)
           (with-output-to-string (out)
             (loop for b across octets
                   do (write-char (if (< b 128) (code-char b) (code-char (aref +cp1251+ (- b 128))))
                                  out))))
          ((member name '("iso-8859-1" "latin1" "latin-1" "windows-1252") :test #'string=)
           (octets-line octets))
          (t (utf8-decode octets)))))

;;; --- transfer encodings -----------------------------------------------------------

(defun decode-quoted-printable (octets)
  "OCTETS with =XX and soft line breaks undone."
  (let ((out (make-array (length octets) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop with i = 0 with n = (length octets)
          while (< i n)
          do (let ((b (aref octets i)))
               (cond ((and (= b 61) (< (+ i 2) n)
                           (digit-char-p (code-char (aref octets (1+ i))) 16)
                           (digit-char-p (code-char (aref octets (+ i 2))) 16))
                      (vector-push (+ (* 16 (digit-char-p (code-char (aref octets (1+ i))) 16))
                                      (digit-char-p (code-char (aref octets (+ i 2))) 16))
                                   out)
                      (incf i 3))
                     ((and (= b 61) (< (1+ i) n)
                           (or (= (aref octets (1+ i)) 13) (= (aref octets (1+ i)) 10)))
                      (incf i (if (and (< (+ i 2) n) (= (aref octets (1+ i)) 13)
                                        (= (aref octets (+ i 2)) 10))
                                  3 2)))
                     (t (vector-push b out) (incf i)))))
    out))

(defun decode-body (octets encoding)
  "OCTETS with transfer ENCODING undone."
  (let ((name (string-downcase (or encoding ""))))
    (cond ((string= name "base64")
           (let ((text (remove-if (lambda (c) (member c '(#\Space #\Tab #\Return #\Linefeed)))
                                  (octets-line octets))))
             (handler-case (cl-base64:base64-string-to-usb8-array text)
               (error () octets))))
          ((string= name "quoted-printable") (decode-quoted-printable octets))
          (t octets))))

;;; --- headers ----------------------------------------------------------------------

(defun parse-header-block (octets)
  "OCTETS' header block as an alist of (lowercased-name . value), and the
position where the body starts."
  (let ((headers '()) (start 0) (n (length octets)))
    (flet ((next-line (from)
             (let ((end (or (octets-index (utf8 (string #\Newline)) octets :start from) n)))
               (values (subseq octets from end)
                       (if (< end n) (1+ end) n)))))
      (loop
        (multiple-value-bind (line next) (next-line start)
          (setf start next)
          (let ((text (string-right-trim '(#\Return)
                                         (decode-charset line "utf-8"))))
            (cond ((zerop (length text)) (return))
                  ((and (plusp (length text))
                        (or (char= (char text 0) #\Space) (char= (char text 0) #\Tab)))
                   (when headers
                     (setf (cdr (first headers))
                           (concatenate 'string (cdr (first headers)) " "
                                        (string-trim '(#\Space #\Tab) text)))))
                  (t (let ((colon (position #\: text)))
                       (when colon
                         (push (cons (string-downcase (string-trim '(#\Space #\Tab)
                                                                   (subseq text 0 colon)))
                                     (string-trim '(#\Space #\Tab) (subseq text (1+ colon))))
                               headers))))))
          (when (>= start n) (return)))))
    (values (nreverse headers) start)))

(defun header-raw (entity name)
  (cdr (assoc name (entity-headers entity) :test #'string=)))

(defun header (entity name)
  "HEADER NAME of ENTITY, RFC 2047 words decoded."
  (let ((value (header-raw entity name)))
    (and value (decode-words value))))

(defun decode-words (text)
  "TEXT with its =?charset?B?...?= words decoded, adjacent words joined."
  (let ((pieces '()) (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((start (search "=?" text :start2 i)))
               (if (null start)
                   (progn (push (cons :text (subseq text i)) pieces) (setf i n))
                   (progn
                     (when (> start i) (push (cons :text (subseq text i start)) pieces))
                     (let ((end (search (concatenate 'string "?" (string #\=)) text
                                        :start2 (+ start 2))))
                       (if (null end)
                           (progn (push (cons :text (subseq text start)) pieces) (setf i n))
                           (let* ((head (subseq text (+ start 2) end))
                                  (charset-end (position #\? head))
                                  (enc-end (and charset-end (position #\? head :start (1+ charset-end)))))
                             (if (null enc-end)
                                 (progn (push (cons :text (subseq text start (+ end 3))) pieces)
                                        (setf i (+ end 3)))
                                 (let* ((charset (subseq head 0 charset-end))
                                        (encoding (string-upcase (subseq head (1+ charset-end) enc-end)))
                                        (payload (subseq head (1+ enc-end)))
                                        (value (handler-case
                                                   (cond ((string= encoding "B")
                                                          (decode-charset
                                                           (cl-base64:base64-string-to-usb8-array
                                                            (remove-if (lambda (c) (find c " \t\r\n"))
                                                                       payload))
                                                           charset))
                                                         ((string= encoding "Q")
                                                          (decode-charset
                                                           (decode-quoted-printable
                                                            (utf8 (substitute #\Space #\_ payload)))
                                                           charset))
                                                         (t payload))
                                                 (error () payload))))
                                   (push (cons :word value) pieces)
                                   (setf i (+ end 3)))))))))))
    (with-output-to-string (out)
      (loop for (kind . value) in (nreverse pieces)
            do (cond ((eq kind :text)
                      (write-string (if (and (string= value "") nil) value value) out))
                     (t (write-string value out)))))))

;;; --- entities ---------------------------------------------------------------------

(defstruct (entity (:copier nil) (:constructor make-entity))
  "One MIME part: its headers, its type and parameters, its encoding, its
body octets when it is a leaf, its children when it is multipart, and its
part_id."
  headers type subtype params encoding id (body nil) (children nil))

(defun parse-parameters (value)
  "VALUE's ;-separated parameters as an alist of (lowercased-key . value)."
  (let ((params '()))
    (loop with parts = (split-semicolons value)
          for part in (cdr parts)
          do (let ((equals (position #\= part)))
               (when equals
                 (push (cons (string-downcase (string-trim '(#\Space #\Tab) (subseq part 0 equals)))
                             (string-trim '(#\" #\Space #\Tab) (subseq part (1+ equals))))
                       params))))
    (nreverse params)))

(defun split-semicolons (text)
  "TEXT split on semicolons that sit outside a quoted string."
  (let ((parts '()) (start 0) (quoted nil))
    (loop for i below (length text)
          for character = (char text i)
          do (cond ((char= character #\") (setf quoted (not quoted)))
                   ((and (char= character #\;) (not quoted))
                    (push (string-trim '(#\Space #\Tab) (subseq text start i)) parts)
                    (setf start (1+ i)))))
    (push (string-trim '(#\Space #\Tab) (subseq text start)) parts)
    (nreverse parts)))

(defun parse-entity (octets &optional (id "1") (depth 0))
  "OCTETS as an ENTITY numbered ID."
  (multiple-value-bind (headers body-start) (parse-header-block octets)
    (let* ((content-type (cdr (assoc "content-type" headers :test #'string=)))
           (pieces (and content-type (split-semicolons content-type)))
           (type-line (or (first pieces) "text/plain"))
           (slash (position #\/ type-line))
           (type (string-downcase (string-trim '(#\Space #\Tab)
                                               (if slash (subseq type-line 0 slash) type-line))))
           (subtype (string-downcase (string-trim '(#\Space #\Tab)
                                                   (if slash (subseq type-line (1+ slash)) "plain"))))
           (params (and content-type (parse-parameters content-type)))
           (disposition (cdr (assoc "content-disposition" headers :test #'string=)))
           (disposition-params (and disposition (parse-parameters disposition)))
           (encoding (cdr (assoc "content-transfer-encoding" headers :test #'string=)))
           (entity (make-entity :headers headers :type type :subtype subtype :params params
                                 :encoding encoding :id id))
           (boundary (cdr (assoc "boundary" (append disposition-params params) :test #'string=))))
      ;; A filename is either content-type's NAME or disposition's FILENAME.
      (let ((filename (or (cdr (assoc "filename" disposition-params :test #'string=))
                          (cdr (assoc "name" params :test #'string=)))))
        (when filename (push (cons "x-filename" filename) (entity-params entity)))
        (when disposition (push (cons "x-disposition" (first (split-semicolons disposition)))
                                (entity-params entity))))
      (if (and (string= type "multipart") boundary (plusp (length boundary)) (< depth 8))
          (setf (entity-children entity)
                (loop for child in (split-multipart (subseq octets body-start) boundary)
                      for index from 1
                      collect (parse-entity child (format nil "~a.~d" id index) (1+ depth))))
          (setf (entity-body entity) (subseq octets body-start)))
      entity)))

(defun split-multipart (body boundary)
  "BODY's parts, split on BOUNDARY."
  (let* ((delimiter (utf8 (concatenate 'string "--" boundary)))
         (crlf (utf8 +crlf+))
         (positions '())
         (start 0))
    (loop for at = (octets-index delimiter body :start start)
          while at
          do (push at positions)
             (setf start (+ at (length delimiter))))
    (setf positions (nreverse positions))
    (let ((parts '()))
      (loop for (at next) on positions
            while next
            do (let* ((after (+ at (length delimiter)))
                      (line-end (or (octets-index crlf body :start after) (length body)))
                      (content-start (min (length body) (+ line-end (length crlf)))))
                 ;; A delimiter followed by "--" closes the multipart.
                 (unless (and (>= (length body) (+ after 2))
                              (= 45 (aref body after)) (= 45 (aref body (1+ after))))
                   (multiple-value-bind (from to) (trim-crlf body content-start next)
                     (push (subseq body from to) parts)))))
      (nreverse parts))))

(defun entity-parameter (entity name)
  (cdr (assoc name (entity-params entity) :test #'string=)))

(defun entity-filename (entity)
  (let ((raw (entity-parameter entity "x-filename")))
    (and raw (plusp (length raw)) (decode-words raw))))

(defun entity-disposition (entity)
  (let ((raw (entity-parameter entity "x-disposition")))
    (and raw (string-downcase (string-trim '(#\Space #\Tab) raw)))))

(defun attachment-p (entity)
  "Is ENTITY a file a person would save: named, or marked attachment, or not
text at all."
  (and (null (entity-children entity))
       (or (entity-filename entity)
           (equal (entity-disposition entity) "attachment")
           (not (string= (entity-type entity) "text")))))

(defun entity-leaves (entity &optional (acc '()))
  (if (entity-children entity)
      (dolist (child (entity-children entity) (nreverse acc))
        (setf acc (entity-leaves child acc)))
      (cons entity acc)))

(defun entity-string (entity)
  "ENTITY's body as text: transfer encoding undone, then its charset."
  (let ((text (decode-charset (decode-body (entity-body entity) (entity-encoding entity))
                               (or (entity-parameter entity "charset") "utf-8"))))
    (string-right-trim '(#\Return #\Linefeed) text)))

(defun part-attachments (entity)
  "Every leaf that is a file, as (:part_id :filename :content_type :size)."
  (loop for leaf in (entity-leaves entity)
        when (attachment-p leaf)
          collect (list :part_id (entity-id leaf)
                        :filename (entity-filename leaf)
                        :content_type (format nil "~a/~a" (entity-type leaf) (entity-subtype leaf))
                        :size (if (entity-body leaf) (length (entity-body leaf)) 0))))

(defun part-by-id (entity id)
  (find id (entity-leaves entity) :key #'entity-id :test #'string=))

(defun part-text (entity)
  "The message's readable text: the text/plain parts, else the HTML drawn as
text, the parts of an attachment never among them."
  (let* ((leaves (remove-if #'attachment-p (entity-leaves entity)))
         (plain (remove-if-not (lambda (leaf)
                                  (and (string= (entity-type leaf) "text")
                                       (string= (entity-subtype leaf) "plain")))
                               leaves)))
    (cond (plain (format nil "~{~a~^~%~%~}" (mapcar #'entity-string plain)))
          (t (let ((html (remove-if-not (lambda (leaf)
                                           (and (string= (entity-type leaf) "text")
                                                (string= (entity-subtype leaf) "html")))
                                         leaves)))
               (if html
                   (format nil "~{~a~^~%~%~}" (mapcar (lambda (leaf) (html->text (entity-string leaf)))
                                                       html))
                   ""))))))

;;; --- HTML drawn as text ---------------------------------------------------------------

(defparameter +entities+
  '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'")
    ("&nbsp;" . " ") ("&mdash;" . "-") ("&ndash;" . "-") ("&hellip;" . "...")
    ("&laquo;" . "\"") ("&raquo;" . "\"") ("&#39;" . "'"))
  "The entities worth undoing; the rest are left as they were written.")

(defun html->text (html)
  "HTML as plain text: script and style dropped, block tags as line breaks,
the few entities undone."
  (let ((text (html-strip-elements (html-strip-elements html "script") "style")))
    (undo-entities
     (with-output-to-string (out)
       (let ((i 0) (n (length text)))
         (loop while (< i n)
               do (let ((character (char text i)))
                    (cond ((char= character #\<)
                           (let ((close (position #\> text :start i)))
                             (let ((tag (and close (string-downcase (subseq text (1+ i) close)))))
                               (when (and tag (some (lambda (prefix) (uiop:string-prefix-p prefix tag))
                                                    '("br" "p" "/p" "div" "/div" "li" "tr")))
                                 (write-char #\Newline out)))
                             (setf i (if close (1+ close) n))))
                          (t (write-char character out) (incf i))))))))))

(defun html-strip-elements (html name)
  "HTML with every <NAME>...</NAME> removed."
  (let ((open (search (concatenate 'string "<" name) (string-downcase html))))
    (if (null open)
        html
        (let ((close (search (concatenate 'string "</" name) (string-downcase html)
                             :start2 open)))
          (if (null close)
              html
              (html-strip-elements (concatenate 'string (subseq html 0 open)
                                                 (let ((end (or (search ">" html :start2 close)
                                                                (length html))))
                                                   (subseq html (1+ end))))
                                    name))))))

(defun replace-text (text old new)
  "TEXT with every OLD written as NEW."
  (with-output-to-string (out)
    (let ((start 0))
      (loop for at = (search old text :start2 start)
            while at
            do (write-string (subseq text start at) out)
               (write-string new out)
               (setf start (+ at (length old))))
      (write-string (subseq text start) out))))

(defun undo-entities (text)
  (dolist (pair +entities+ text)
    (setf text (replace-text text (first pair) (rest pair)))))

;;; --- what a tool answers ------------------------------------------------------------

(defun page-text (text offset max-chars)
  "TEXT from OFFSET, at most MAX-CHARS long: (values the page, the next
offset, whether that was the end)."
  (let* ((offset (max 0 (min offset (length text))))
         (end (min (length text) (+ offset max-chars))))
    (values (subseq text offset end) (if (>= end (length text)) nil end) (>= end (length text)))))

(defparameter +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defparameter +day-names+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defun rfc5322-date (&optional (universal (get-universal-time)))
  "UNIVERSAL as an RFC 5322 date, in UTC."
  (multiple-value-bind (second minute hour day month year weekday) (decode-universal-time universal 0)
    (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d +0000"
            (aref +day-names+ weekday) day (aref +month-names+ (1- month)) year
            hour minute second)))

(defun message-id (from)
  "A Message-ID for a message FROM this account."
  (let ((domain (or (and (position #\@ from) (subseq from (1+ (position #\@ from)))) "localhost")))
    (format nil "<~36r.~36r@~a>" (get-universal-time) (random 1000000000) domain)))

(defun base64-lines (text)
  "TEXT as base64 of its UTF-8, wrapped at 76 columns."
  (let ((encoded (cl-base64:usb8-array-to-base64-string (utf8 text))))
    (with-output-to-string (out)
      (loop for start from 0 below (length encoded) by 76
            do (write-string (subseq encoded start (min (length encoded) (+ start 76))) out)
               (write-string +crlf+ out)))))

(defun address-list (addresses)
  "ADDRESSES as one header value."
  (format nil "~{~a~^, ~}" addresses))

(defun compose-message (&key from to cc subject body in-reply-to references)
  "One message, as the text SMTP sends: UTF-8, base64 body, threading
headers when this answers another."
  (let ((id (message-id from)))
    (with-output-to-string (out)
      (macrolet ((line (text) `(progn (write-string ,text out) (write-string +crlf+ out))))
        (line (format nil "From: ~a" from))
        (line (format nil "To: ~a" (address-list to)))
        (when (and cc (plusp (length cc))) (line (format nil "Cc: ~a" (address-list cc))))
        (line (format nil "Subject: ~a" (if (every #'ascii-char-p subject)
                                             subject
                                             (format nil "=?UTF-8?B?~a?="
                                                     (cl-base64:usb8-array-to-base64-string
                                                      (utf8 subject))))))
        (line (format nil "Date: ~a" (rfc5322-date)))
        (line (format nil "Message-ID: ~a" id))
        (when in-reply-to (line (format nil "In-Reply-To: ~a" in-reply-to)))
        (when references (line (format nil "References: ~a" references)))
        (line "MIME-Version: 1.0")
        (line "Content-Type: text/plain; charset=utf-8")
        (line "Content-Transfer-Encoding: base64")
        (line "")
        (write-string (base64-lines (or body "")) out)
        id))))

(defun ascii-char-p (character)
  (< (char-code character) 128))

(defun safe-filename (name part-id)
  "NAME as one harmless path component, or a name made from PART-ID."
  (let* ((base (or (and name (car (last (uiop:split-string name :separator '(#\/ #\\))))) ""))
         (clean (string-trim '(#\Space #\Tab #\.)
                              (map 'string (lambda (character)
                                              (if (or (< (char-code character) 32)
                                                      (find character "<>:\"|?*$`';&(){}!#~"))
                                                  #\_
                                                  character))
                                   base)))
         (stem (if (> (length clean) 120) (subseq clean 0 120) clean)))
    (if (plusp (length stem)) stem (format nil "attachment-~a.bin" part-id))))
