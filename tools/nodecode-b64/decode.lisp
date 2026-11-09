;;;; decode.lisp --- the decoder: base64, base64url, hex, percent-encoding, a data: URI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The half of the cell that touches no seam: a value in, the description of
;;;; what it holds out, and B64-ERROR with the reason when it is none of them.
;;;; Nothing here runs, opens, saves or fetches what it decodes; gzip and zlib
;;;; data is inflated in memory, one layer deep, cut at the section's ceiling.
;;;; Every function takes its bounds as an argument, so a test reads the same
;;;; code the tool does without starting the cell.

(in-package #:nodecode-b64)

(nlk:define-peripheral b64 :not-running t)

;;; --- the bounds ------------------------------------------------------------------

(defparameter +max-input-chars+ 65536
  "The longest value decoded; a longer one is refused, not truncated.")

(defparameter +max-inflated+ 1048576
  "gzip and zlib output is cut at this many bytes.")

(defparameter +text-chars+ 3000
  "Characters of decoded text shown; the rest is counted.")

(defparameter +text-lines+ 60
  "Lines of decoded text shown; the rest is counted.")

(defparameter +dump-bytes+ 128
  "Bytes of a binary result shown as a hex dump.")

(defparameter +limits+
  (list :max-input +max-input-chars+ :max-inflated +max-inflated+
        :text-chars +text-chars+ :text-lines +text-lines+ :dump-bytes +dump-bytes+)
  "The bounds a reader takes when the caller names none: the original's own
constants, and the section's defaults.")

;;; --- the one record ---------------------------------------------------------------

(defstruct (decoded (:constructor make-decoded (label chars data notes)))
  "One decoded value: how it was read, what it held, and anything worth a note."
  (label "" :type string :read-only t)
  (chars 0 :type integer :read-only t)
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type vector :read-only t)
  (notes '() :read-only t))

;;; --- characters and octets -------------------------------------------------------

(defun octets (&rest bytes)
  "BYTES as a simple octet vector."
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun ascii (string)
  "STRING's characters as octets: for a signature written to be read."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun starts-with-p (data signature)
  "Whether DATA begins with the octets of SIGNATURE."
  (and (>= (length data) (length signature))
       (loop for index from 0 below (length signature)
             always (= (aref data index) (aref signature index)))))

(defparameter +spaces+ '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)
  "The whitespace a value may be wrapped in, or line-wrapped with.")

(defun whitespace-char-p (character)
  "Whether CHARACTER is whitespace: the ASCII set, and the Unicode spaces a
paste brings along."
  (let ((code (char-code character)))
    (or (member character +spaces+)
        (<= #x2000 code #x200A)
        (member code '(#x00A0 #x1680 #x2028 #x2029 #x202F #x205F #x3000 #xFEFF)))))

(defun trimmed (text)
  "TEXT without the whitespace around it."
  (string-trim +spaces+ (or text "")))

(defun clean-value (text)
  "The value of a reference: surrounding space and one pair of quotes removed."
  (let ((clean (trimmed text)))
    (when (and (>= (length clean) 2)
               (char= (char clean 0) (char clean (1- (length clean))))
               (find (char clean 0) "\"'`"))
      (setf clean (trimmed (subseq clean 1 (1- (length clean))))))
    clean))

(defun notes-vector (&optional initial)
  (let ((notes (make-array (length initial) :adjustable t :fill-pointer 0)))
    (loop for note in initial do (vector-push-extend note notes))
    notes))

(defun make-notes (&optional initial)
  "A growable, ordered list of notes, as the description answers them."
  (notes-vector initial))

(defun note-push (notes text)
  "Add TEXT to NOTES, a growable vector of strings, at the end."
  (vector-push-extend text notes)
  text)

(defun utf-8-octets (string)
  "STRING as UTF-8 octets."
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8)
                                              :adjustable t :fill-pointer 0)))
    (flet ((put (byte) (vector-push-extend byte out)))
      (loop for character across string
            for code = (char-code character)
            do (cond ((<= code #x7F) (put code))
                     ((<= code #x7FF)
                      (put (logior #xC0 (ash code -6)))
                      (put (logior #x80 (logand code #x3F))))
                     ((<= code #xFFFF)
                      (put (logior #xE0 (ash code -12)))
                      (put (logior #x80 (logand (ash code -6) #x3F)))
                      (put (logior #x80 (logand code #x3F))))
                     ((<= code #x10FFFF)
                      (put (logior #xF0 (ash code -18)))
                      (put (logior #x80 (logand (ash code -12) #x3F)))
                      (put (logior #x80 (logand (ash code -6) #x3F)))
                      (put (logior #x80 (logand code #x3F))))
                     (t (put #xFF) (put #xFD)))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun continuation (byte)
  "BYTE's six bits when it is a UTF-8 continuation byte, else NIL."
  (when (<= #x80 byte #xBF) (logand byte #x3F)))

(defun utf-8-code-point (data index end)
  "The code point DATA's byte at INDEX begins, and its width; NIL when the
bytes there are not well-formed UTF-8 (an overlong form, a surrogate, past
U+10FFFF, or cut short)."
  (let* ((first-byte (aref data index))
         (width (cond ((<= first-byte #x7F) 1)
                      ((<= #xC2 first-byte #xDF) 2)
                      ((<= #xE0 first-byte #xEF) 3)
                      ((<= #xF0 first-byte #xF4) 4)
                      (t 0))))
    (when (and (plusp width) (<= (+ index width) end))
      (let ((code (utf-8-assemble data index width first-byte)))
        (when (and code
                   (case width (1 t) (2 (>= code #x80)) (3 (>= code #x800)) (t (>= code #x10000)))
                   (not (<= #xD800 code #xDFFF))
                   (<= code #x10FFFF))
          (values code width))))))

(defun utf-8-assemble (data index width first-byte)
  "The code point WIDTH bytes of DATA from INDEX spell, or NIL when one of them
is not a continuation byte."
  (macrolet ((tail (offset)
               `(continuation (aref data (+ index ,offset)))))
    (case width
      (1 first-byte)
      (2 (let ((b (tail 1))) (and b (logior (ash (logand first-byte #x1F) 6) b))))
      (3 (let ((b (tail 1)))
           (and b (let ((c (tail 2)))
                    (and c (logior (ash (logand first-byte #x0F) 12) (ash b 6) c))))))
      (4 (let ((b (tail 1)))
           (and b (let ((c (tail 2)))
                    (and c (let ((d (tail 3)))
                             (and d (logior (ash (logand first-byte #x07) 18)
                                            (ash b 12) (ash c 6) d))))))))
      (t nil))))

(defun utf-8-decoded (data &key (start 0) (end (length data)))
  "DATA from START to END as a string, or NIL when it is not well-formed UTF-8."
  (let ((characters '())
        (index start))
    (loop while (< index end)
          do (multiple-value-bind (code width) (utf-8-code-point data index end)
               (unless code (return-from utf-8-decoded nil))
               (push (code-char code) characters)
               (incf index width)))
    (coerce (nreverse characters) 'string)))

(defun utf-16-decoded (data start little-endian)
  "DATA from START as UTF-16 text, or NIL when its units are not well formed."
  (let ((characters '())
        (index start)
        (end (length data)))
    (when (oddp (- end start)) (return-from utf-16-decoded nil))
    (labels ((unit (at)
               (if little-endian
                   (logior (aref data at) (ash (aref data (1+ at)) 8))
                   (logior (ash (aref data at) 8) (aref data (1+ at))))))
      (loop while (< index end)
            do (let ((code (unit index)))
                 (cond
                   ((<= #xD800 code #xDBFF)
                    (when (> (+ index 4) end) (return-from utf-16-decoded nil))
                    (let ((low (unit (+ index 2))))
                      (unless (<= #xDC00 low #xDFFF) (return-from utf-16-decoded nil))
                      (push (code-char (+ #x10000 (ash (- code #xD800) 10) (- low #xDC00)))
                            characters)
                      (incf index 4)))
                   ((<= #xDC00 code #xDFFF) (return-from utf-16-decoded nil))
                   (t (push (code-char code) characters) (incf index 2))))))
    (coerce (nreverse characters) 'string)))

;;; --- base64 ----------------------------------------------------------------------

(defparameter +base64-standard+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The standard alphabet, as RFC 4648 writes it.")

(defparameter +base64-url-safe+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "The URL-safe alphabet: the same order, '-' and '_' for '+' and '/'.")

(defun alphabet-map (alphabet)
  "A 128-entry vector: a character's six-bit value, NIL for everything else."
  (let ((map (make-array 128 :initial-element nil)))
    (loop for index from 0 below 64
          do (setf (aref map (char-code (char alphabet index))) index))
    map))

(defparameter +standard-map+ (alphabet-map +base64-standard+))
(defparameter +url-map+ (alphabet-map +base64-url-safe+))

(defun base64-value (character map)
  "CHARACTER's six bits in the alphabet MAP, or NIL when it is not one."
  (let ((code (char-code character)))
    (when (< code 128) (aref map code))))

(defun base64-bytes (stripped map)
  "STRIPPED, a validated base64 body with no padding, as its octets."
  (let ((out (make-array (* 3 (ceiling (length stripped) 4))
                          :element-type '(unsigned-byte 8) :fill-pointer 0))
        (length-of (length stripped)))
    (loop for index from 0 below length-of by 4
          for size = (min 4 (- length-of index))
          for a = (base64-value (char stripped index) map)
          for b = (and (> size 1) (base64-value (char stripped (1+ index)) map))
          for c = (and (> size 2) (base64-value (char stripped (+ index 2)) map))
          for d = (and (> size 3) (base64-value (char stripped (+ index 3)) map))
          do (vector-push (logior (ash a 2) (ash b -4)) out)
             (when c
               (vector-push (logior (ash (logand b #x0F) 4) (ash c -2)) out))
             (when d
               (vector-push (logior (ash (logand c #x03) 6) d) out)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun base64-string (data alphabet)
  "DATA as base64 in ALPHABET, padded: what a re-encoding compares against."
  (let ((out (make-string-output-stream))
        (length-of (length data)))
    (loop for index from 0 below length-of by 3
          for left = (- length-of index)
          for b0 = (aref data index)
          for b1 = (if (> left 1) (aref data (1+ index)) 0)
          for b2 = (if (> left 2) (aref data (+ index 2)) 0)
          for bits = (logior (ash b0 16) (ash b1 8) b2)
          do (write-char (char alphabet (ldb (byte 6 18) bits)) out)
             (write-char (char alphabet (ldb (byte 6 12) bits)) out)
             (write-char (if (> left 1) (char alphabet (ldb (byte 6 6) bits)) #\=) out)
             (write-char (if (> left 2) (char alphabet (ldb (byte 6 0) bits)) #\=) out))
    (get-output-stream-string out)))

(defun decode-base64 (text notes &key forced)
  "TEXT as base64 or base64url: the alphabet its characters name, or FORCED."
  (let* ((body (remove-if #'whitespace-char-p text))
         (padding (loop with count = 0
                        for index downfrom (1- (length body)) to 0
                        while (char= (char body index) #\=)
                        do (incf count)
                        finally (return count)))
         (stripped (subseq body 0 (- (length body) padding))))
    (when (zerop (length stripped)) (fail "nothing to decode"))
    (let ((has-standard (find-if (lambda (character) (member character '(#\+ #\/))) stripped))
          (has-url (find-if (lambda (character) (member character '(#\- #\_))) stripped)))
      (when (and has-standard has-url)
        (fail "this mixes the standard base64 alphabet ('+', '/') with the URL-safe one ('-', '_')"))
      (let* ((url (or has-url (and (equal forced "base64url") (not has-standard))))
             (map (if url +url-map+ +standard-map+))
             (bad (find-if-not (lambda (character) (base64-value character map)) stripped)))
        (when (find #\= stripped)
          (fail "'=' may only appear at the end of base64, as padding"))
        (when bad (fail "~s is not a base64 character" (string bad)))
        (when (> padding 2)
          (fail "base64 ends in at most two '='; this ends in ~d" padding))
        (when (= 1 (mod (length stripped) 4))
          (fail "base64 cannot be ~:d characters long (one character is left over); it was probably cut short"
                (length stripped)))
        (when (and (plusp padding) (plusp (mod (length body) 4)))
          (note-push notes (format nil "the padding is off: ~d '=' for ~:d characters (decoded anyway)"
                                   padding (length stripped))))
        (let ((data (base64-bytes stripped map)))
          (unless (string= (string-right-trim "=" (base64-string data (if url +base64-url-safe+ +base64-standard+)))
                           stripped)
            (note-push notes "the last character has bits set that decoding drops: the text was probably cut or edited"))
          (make-decoded (concatenate 'string (if url "base64url" "base64")
                                      (if (and (plusp (mod (length stripped) 4)) (zerop padding))
                                          ", unpadded" ""))
                        (length stripped) data notes))))))

;;; --- hex and percent-encoding -----------------------------------------------------

(defun hex-digits (text)
  "TEXT without a 0x prefix, its \\x markers, its colons and its whitespace:
the digits a value written for hex holds."
  (let ((body (if (and (>= (length text) 2) (string-equal (subseq text 0 2) "0x"))
                  (subseq text 2)
                  text)))
    (with-output-to-string (out)
      (loop with index = 0
            with length-of = (length body)
            while (< index length-of)
            for character = (char body index)
            do (cond
                 ((and (char= character #\\) (< (1+ index) length-of)
                       (char-equal (char body (1+ index)) #\x))
                  (incf index 2))
                 ((or (whitespace-char-p character) (char= character #\:)) (incf index))
                 (t (write-char character out) (incf index)))))))

(defun hex-pairs-p (text)
  "Whether TEXT is hex digits in pairs: a value's shape says hex."
  (and (plusp (length text))
       (evenp (length text))
       (every (lambda (character) (digit-char-p character 16)) text)))

(defun decode-hex (text notes)
  "TEXT, hex digits in any of the ways hex gets written, as its octets."
  (let ((digits (hex-digits text)))
    (unless (hex-pairs-p digits)
      (cond
        ((every (lambda (character) (digit-char-p character 16)) digits)
         (fail "hex needs two digits per byte; this has ~:d digits" (length digits)))
        (t (fail "~s is not a hex digit"
                 (string (find-if-not (lambda (character) (digit-char-p character 16)) digits))))))
    (make-decoded "hex" (length digits)
                  (loop with out = (make-array (/ (length digits) 2)
                                                :element-type '(unsigned-byte 8))
                        for index from 0 below (length digits) by 2
                        for at from 0
                        do (setf (aref out at) (parse-integer digits :start index :end (+ index 2) :radix 16))
                        finally (return out))
                  notes)))

(defun percent-sequence-p (text index)
  "Whether TEXT has %XX at INDEX: a percent escape, two hex digits long."
  (and (< (+ index 2) (length text))
       (char= (char text index) #\%)
       (digit-char-p (char text (1+ index)) 16)
       (digit-char-p (char text (+ index 2)) 16)))

(defun percent-present-p (text)
  "Whether TEXT holds a percent escape at all."
  (loop for index from 0 below (length text) thereis (percent-sequence-p text index)))

(defun decode-percent (text notes)
  "TEXT percent-decoded: a %XX an octet, any other character its UTF-8 bytes."
  (when (find #\+ text)
    (note-push notes "'+' is kept as '+'; in a form-encoded query string it stands for a space"))
  (loop for index from 0 below (length text)
        when (and (char= (char text index) #\%) (not (percent-sequence-p text index)))
          do (note-push notes (format nil "a '%' at position ~:d is not followed by two hex digits and is kept as is"
                                      (1+ index)))
             (return))
  (let ((out (make-array (length text) :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0)))
    (loop with index = 0
          with length-of = (length text)
          while (< index length-of)
          do (if (percent-sequence-p text index)
                 (progn
                   (vector-push-extend (parse-integer text :start (1+ index) :end (+ index 3) :radix 16) out)
                   (incf index 3))
                 (progn
                   (loop for byte across (utf-8-octets (string (char text index)))
                         do (vector-push-extend byte out))
                   (incf index))))
    (make-decoded "percent-encoding" (length text)
                  (coerce out '(simple-array (unsigned-byte 8) (*))) notes)))

;;; --- a data: URI ----------------------------------------------------------------

(defun data-uri-parts (text)
  "TEXT's data: URI as its meta and its payload, or NIL when it is not one."
  (when (and (>= (length text) 5) (string-equal (subseq text 0 5) "data:"))
    (let ((comma (position #\, text :start 5)))
      (when comma (values (subseq text 5 comma) (subseq text (1+ comma)))))))

(defun latin-1-string (data)
  "DATA as one character an octet: what a data: URI's payload is read as."
  (map 'string #'code-char data))

(defun decode-data-uri (meta payload notes)
  "A data: URI's META and PAYLOAD decoded: base64 or percent-encoded, as its
meta says."
  (let* ((params (loop for part in (uiop:split-string meta :separator ";")
                        collect (trimmed part)))
         (base64 (and params (string-equal (car (last params)) "base64")))
         (mime (or (and params (plusp (length (car params))) (car params)) "text/plain"))
         (inner (if base64
                    (decode-base64 (latin-1-string (decoded-data (decode-percent payload (make-notes))))
                                   notes)
                    (decode-percent payload notes))))
    (make-decoded (format nil "data: URI (~a, ~a)" mime (if base64 "base64" "percent-encoded"))
                  (decoded-chars inner) (decoded-data inner) notes)))

;;; --- reading the value ------------------------------------------------------------

(defparameter +forced-kinds+ '("base64url" "base64" "hex" "url")
  "The encodings a value may name in front of itself: base64:, hex:, url:.")

(defun forced-prefix (text)
  "TEXT's own encoding prefix, as (values KIND BODY), or NIL."
  (loop for kind in +forced-kinds+
        for prefix = (concatenate 'string kind ":")
        when (and (>= (length text) (length prefix))
                  (string-equal (subseq text 0 (length prefix)) prefix))
          do (return (values kind (subseq text (length prefix))))))

(defun forced-prefix-p (text)
  "Whether TEXT names its encoding in front of itself."
  (multiple-value-bind (kind) (forced-prefix text) (and kind t)))

(defun ascii-alphanumeric-p (character)
  (let ((code (char-code character)))
    (or (<= 65 code 90) (<= 97 code 122) (<= 48 code 57))))

(defun base64-shaped-p (text)
  "Whether every character of TEXT belongs to one of the base64 alphabets."
  (and (plusp (length text))
       (every (lambda (character)
                (or (ascii-alphanumeric-p character)
                    (find character "+/_-=" :test #'char=)))
              text)))

(defun decode-value (value &key forced (limits +limits+))
  "Decode VALUE, telling the encoding by its shape. Signals B64-ERROR with the
reason when it is none of them, or longer than the section allows."
  (let ((text (clean-value value)))
    (when (and forced (plusp (length text)) (not (forced-prefix-p text)))
      (setf text (concatenate 'string forced ":" text)))
    (when (zerop (length text))
      (fail "nothing to decode: give it base64, base64url, hex or percent-encoded text"))
    (when (> (length text) (getf limits :max-input))
      (fail "the value is ~:d characters long; b64 reads at most ~:d"
            (length text) (getf limits :max-input)))
    (let ((notes (make-notes)))
      (multiple-value-bind (kind body) (forced-prefix text)
        (cond
          (kind (cond ((string= kind "hex") (decode-hex body notes))
                      ((string= kind "url") (decode-percent body notes))
                      (t (decode-base64 body notes :forced kind))))
          (t (multiple-value-bind (meta payload) (data-uri-parts text)
               (cond
                 (meta (decode-data-uri meta payload notes))
                 ((percent-present-p text) (decode-percent text notes))
                 (t (decode-by-shape text notes))))))))))

(defun decode-by-shape (text notes)
  "TEXT read the way its shape says: hex pairs as hex, the base64 alphabets as
base64; anything else refuses."
  (let ((compact (remove-if (lambda (character) (or (whitespace-char-p character)
                                                    (char= character #\:)))
                             text)))
    (cond
      ((or (and (>= (length text) 2) (string-equal (subseq text 0 2) "0x"))
           (search "\\x" text)
           (and (>= (length compact) 2) (hex-pairs-p compact)))
       (let ((decoded (decode-hex text notes)))
         (unless (or (and (>= (length text) 2) (string-equal (subseq text 0 2) "0x"))
                     (find #\: text)
                     (= 1 (mod (length compact) 4)))
           (note-push notes "read as hex; it is also valid base64, which base64:<value> reads instead"))
         decoded))
      ((base64-shaped-p (remove-if #'whitespace-char-p text)) (decode-base64 text notes))
      (t (let ((bad (find-if-not #'base64-character-p text)))
           (fail "this is not base64, base64url, hex or percent-encoding~@[: ~s belongs to none of them~]"
                 (and bad (string bad))))))))

(defun base64-character-p (character)
  (or (whitespace-char-p character)
      (ascii-alphanumeric-p character)
      (find character "+/_=-" :test #'char=)))

;;; --- what it decoded to ---------------------------------------------------------

(defun png-kind (data)
  (if (and (>= (length data) 24) (equalp (subseq data 12 16) (ascii "IHDR")))
      (format nil "PNG image, ~d x ~d pixels"
              (logior (ash (aref data 16) 24) (ash (aref data 17) 16) (ash (aref data 18) 8) (aref data 19))
              (logior (ash (aref data 20) 24) (ash (aref data 21) 16) (ash (aref data 22) 8) (aref data 23)))
      "PNG image"))

(defun gif-kind (data)
  (if (>= (length data) 10)
      (format nil "GIF image, ~d x ~d pixels"
              (logior (aref data 6) (ash (aref data 7) 8))
              (logior (aref data 8) (ash (aref data 9) 8)))
      "GIF image"))

(defun pdf-kind (data)
  (declare (ignore data))
  "PDF document")

(defparameter +signatures+
  (list (cons (octets #x89 80 78 71 13 10 26 10) #'png-kind)
        (cons (ascii "GIF87a") #'gif-kind)
        (cons (ascii "GIF89a") #'gif-kind)
        (cons (octets #xFF 216 255) "JPEG image")
        (cons (ascii "%PDF-") #'pdf-kind)
        (cons (octets 80 75 3 4) "ZIP archive (also .docx, .xlsx, .jar, .apk)")
        (cons (octets 80 75 5 6) "ZIP archive, empty")
        (cons (octets 31 139) "gzip data")
        (cons (ascii "BZh") "bzip2 data")
        (cons (octets #xFD 55 122 88 90 0) "xz data")
        (cons (octets 40 #xB5 47 #xFD) "zstd data")
        (cons (octets 55 122 #xBC #xAF 39 28) "7-Zip archive")
        (cons (octets #x7F 69 76 70) "ELF executable or library")
        (cons (octets #xCA #xFE #xBA #xBE) "Java class file or macOS universal binary")
        (cons (octets #xCF #xFA #xED #xFE) "macOS executable (Mach-O, 64-bit)")
        (cons (octets #xCE #xFA #xED #xFE) "macOS executable (Mach-O, 32-bit)")
        (cons (octets 0 97 115 109) "WebAssembly module")
        (cons (ascii "SQLite format 3") "SQLite database")
        (cons (ascii "OggS") "Ogg audio or video")
        (cons (ascii "ID3") "MP3 audio"))
  "The signatures text never starts with, and what each names, checked before
the text test.")

(defun riff-kind (data)
  "The RIFF and ISO base media formats, whose signature needs a look at the
bytes after it."
  (cond
    ((and (>= (length data) 12) (starts-with-p data (ascii "RIFF")))
     (let ((form (subseq data 8 12)))
       (cond ((equalp form (ascii "WEBP")) "WebP image")
             ((equalp form (ascii "WAVE")) "WAV audio")
             ((equalp form (ascii "AVI ")) "AVI video")
             (t "RIFF data"))))
    ((and (>= (length data) 8) (equalp (subseq data 4 8) (ascii "ftyp")))
     "MP4 / QuickTime media")
    (t nil)))

(defun kind-of (data)
  "A known format named by DATA's first bytes, or NIL."
  (loop for (signature . what) in +signatures+
        when (starts-with-p data signature)
          do (return-from kind-of (if (functionp what) (funcall what data) what)))
  (riff-kind data))

(defun weak-kind (data)
  "Formats whose signature is short enough for text to start with it, checked
after the text test."
  (cond
    ((and (>= (length data) 2) (= (aref data 0) #x78)
          (zerop (mod (+ (* (aref data 0) 256) (aref data 1)) 31)))
     "zlib data")
    ((and (>= (length data) 2) (= (aref data 0) 77) (= (aref data 1) 90))
     "Windows executable (PE)")
    ((and (>= (length data) 2) (= (aref data 0) #x30)
          (member (aref data 1) '(#x81 #x82 #x83)))
     "DER (ASN.1): a certificate, key or signature in binary form")
    (t nil)))

(defun control-p (character)
  (let ((code (char-code character)))
    (or (< code 32) (<= #x7F code #x9F))))

(defun text-of (data)
  "DATA as text and the name of its encoding when it is not plain UTF-8, as a
cons; NIL when the bytes are binary."
  (cond
    ((starts-with-p data (octets #xEF #xBB #xBF))
     (let ((text (utf-8-decoded data :start 3)))
       (and text (cons text "UTF-8 with a byte order mark"))))
    ((starts-with-p data (octets #xFF #xFE))
     (let ((text (utf-16-decoded data 2 t)))
       (and text (cons text "UTF-16 (little-endian)"))))
    ((starts-with-p data (octets #xFE #xFF))
     (let ((text (utf-16-decoded data 2 nil)))
       (and text (cons text "UTF-16 (big-endian)"))))
    (t (let ((text (utf-8-decoded data)))
         (when text
           (let ((controls (count-if (lambda (character)
                                       (and (control-p character)
                                            (not (member character '(#\Tab #\Newline #\Return)))))
                                     text)))
             (unless (or (find (code-char 0) text)
                         (> controls (max 1 (floor (length text) 20))))
               (cons text nil))))))))

;;; --- showing what it holds ---------------------------------------------------------

(defun json-shape (value)
  "The description of a parsed JSON object or array."
  (cond
    ((hash-table-p value) (format nil "JSON object, ~d key~:p" (hash-table-count value)))
    ((vectorp value) (format nil "JSON array, ~d item~:p" (length value)))
    ((listp value) (format nil "JSON array, ~d item~:p" (length value)))
    (t "JSON value")))

(defun json-text (value &optional (indent 0))
  "VALUE as pretty JSON, two spaces a level, an object's keys in order."
  (let ((pad (make-string (* 2 (1+ indent)) :initial-element #\Space))
        (dedent (make-string (* 2 indent) :initial-element #\Space)))
    (cond
      ((eq value t) "true")
      ((null value) "false")
      ((eq value :null) "null")
      ((stringp value) (shasht:write-json value nil))
      ((numberp value) (shasht:write-json value nil))
      ((hash-table-p value)
       (if (zerop (hash-table-count value))
           "{}"
           (let ((members (sort (loop for key being the hash-keys of value using (hash-value item)
                                      collect (cons key item))
                                #'string< :key (lambda (member) (princ-to-string (car member))))))
             (with-output-to-string (out)
               (write-string "{" out)
               (loop for (key . item) in members
                     for at from 0
                     do (when (plusp at) (write-string "," out))
                        (format out "~%~a~s: ~a" pad (princ-to-string key) (json-text item (1+ indent))))
               (format out "~%~a}" dedent)))))
      ((vectorp value)
       (if (zerop (length value))
           "[]"
           (with-output-to-string (out)
             (write-string "[" out)
             (loop for item across value
                   for at from 0
                   do (when (plusp at) (write-string "," out))
                      (format out "~%~a~a" pad (json-text item (1+ indent))))
             (format out "~%~a]" dedent))))
      (t (shasht:write-json (princ-to-string value) nil)))))

(defun json-value (text)
  "TEXT parsed as JSON when it is one object or array, else NIL."
  (let ((stripped (trimmed text)))
    (when (and (plusp (length stripped)) (find (char stripped 0) "{["))
      (handler-case
          (let ((value (shasht:read-json stripped nil nil t)))
            (and (or (hash-table-p value) (vectorp value)) value))
        (error () nil)))))

(defun split-lines (text)
  "TEXT's lines, an empty last line kept: a final line break is not an empty line."
  (loop with lines = '()
        with start = 0
        for position = (position #\Newline text :start start)
        do (if position
               (progn (push (subseq text start position) lines) (setf start (1+ position)))
               (progn (push (subseq text start) lines) (return (nreverse lines))))))

(defun join-length (lines)
  "How many characters LINES hold, with the line breaks between them."
  (if (null lines)
      0
      (+ (1+ (loop for line in lines sum (length line))) (1- (length lines)))))

(defun replace-crlf (text)
  "TEXT with \\r\\n brought to \\n: a lone return stays, and is escaped."
  (with-output-to-string (out)
    (loop with index = 0
          with length-of = (length text)
          while (< index length-of)
          for character = (char text index)
          do (cond
               ((and (char= character #\Return) (< (1+ index) length-of)
                     (char= (char text (1+ index)) #\Newline))
                (write-char #\Newline out)
                (incf index 2))
               (t (write-char character out) (incf index))))))

(defun invisible-p (character)
  "Whether CHARACTER is a control or an invisible one: it must not hide in
plain sight."
  (let ((code (char-code character)))
    (or (and (control-p character) (not (char= character #\Tab)))
        (member code '(#x00AD #x200B #x200C #x200D #x200E #x200F #x2028 #x2029
                       #x202A #x202B #x202C #x202D #x202E #x2060 #xFEFF)))))

(defun escape-line (line)
  "LINE with its control and invisible characters written out, and how many."
  (let ((hidden 0))
    (values
     (with-output-to-string (out)
       (loop for character across line
             do (if (invisible-p character)
                    (progn
                      (incf hidden)
                      (format out (if (< (char-code character) 256) "\\x~2,'0x" "\\u~4,'0x")
                              (char-code character)))
                    (write-char character out))))
     hidden)))

(defun text-lines (text limits)
  "TEXT as the description's indented lines, the characters left out, and how
many were escaped."
  (let* ((normalized (replace-crlf text))
         (cap (getf limits :text-chars))
         (shown (subseq normalized 0 (min (length normalized) cap)))
         (left (- (length normalized) (length shown)))
         (lines (split-lines shown)))
    (when (and (> (length lines) 1) (string= (car (last lines)) "") (zerop left))
      (setf lines (butlast lines)))
    (when (> (length lines) (getf limits :text-lines))
      (incf left (join-length (nthcdr (getf limits :text-lines) lines)))
      (setf lines (subseq lines 0 (getf limits :text-lines))))
    (let ((hidden 0))
      (values
       (loop for line in lines
             collect (multiple-value-bind (escaped count) (escape-line line)
                       (incf hidden count)
                       (concatenate 'string "  " escaped)))
       left hidden))))

(defun dump-lines (data limits)
  "DATA's first bytes as a hex dump: offset, hex, and what could be read."
  (let* ((cap (getf limits :dump-bytes))
         (shown (min (length data) cap))
         (lines '()))
    (loop for offset from 0 below shown by 16
          for chunk = (subseq data offset (min (length data) (+ offset 16)))
          for hexes = (string-downcase (format nil "~{~2,'0x~^ ~}" (coerce chunk 'list)))
          do (push (format nil "  ~8,'0x  ~a~a  ~a" offset hexes
                           (make-string (max 0 (- 47 (length hexes))) :initial-element #\Space)
                           (map 'string (lambda (byte) (if (<= 32 byte 126) (code-char byte) #\.)) chunk))
                   lines))
    (when (> (length data) shown)
      (push (format nil "  ... ~:d more bytes" (- (length data) shown)) lines))
    (nreverse lines)))

(defun inflate (data format)
  "DATA inflated as FORMAT, one layer deep: (values OCTETS NIL) or (values NIL
THE-REASON)."
  (handler-case (values (chipz:decompress nil format data) nil)
    (error (failure) (values nil failure))))

(defun inflated-lines (data notes limits depth what)
  "The description's lines for gzip or zlib DATA, whose INFLATED content is
described the same way one level down."
  (multiple-value-bind (inflated failure)
      (inflate data (if (string= what "gzip data") 'chipz:gzip 'chipz:zlib))
    (if failure
        (progn
          (note-push notes (format nil "~a that does not inflate: ~a" what failure))
          (append (list (format nil "Content: ~a, ~:d bytes" what (length data))
                        "First bytes (hex)")
                  (dump-lines data limits)))
        (let* ((cap (getf limits :max-inflated))
               (cut (> (length inflated) cap))
               (kept (if cut (subseq inflated 0 cap) inflated)))
          (when cut
            (note-push notes (format nil "inflated output is cut at ~:d bytes" cap)))
          (let ((head (format nil "Content: ~a, inflates to ~:d bytes" what (length kept))))
            (if (plusp depth)
                (list head)
                (cons head
                      (loop for line in (content-lines kept notes limits (1+ depth))
                            collect (if (and (>= (length line) 9) (string= (subseq line 0 9) "Content: "))
                                        (concatenate 'string "Inflated: " (subseq line 9))
                                        line)))))))))

(defun other-layer (text)
  "What TEXT itself decodes to when it is base64 of something readable, NIL
when it is not: the second layer of encoding worth a note."
  (let ((body (string-right-trim "=" (trimmed text))))
    (when (and (>= (length body) 8)
               (not (find #\Newline body))
               (base64-shaped-p body))
      (handler-case
          (let* ((inner (decoded-data (decode-base64 body (make-notes))))
                 (known (kind-of inner)))
            (cond
              (known known)
              (t (let ((as-text (text-of inner)))
                   (when (and as-text
                              (>= (length (trimmed (car as-text))) 4)
                              (every (lambda (character)
                                       (or (graphic-char-p character) (char= character #\Space)))
                                     (car as-text)))
                     "text")))))
        (b64-error () nil)))))

(defun jwt-shaped-p (text)
  "Whether TEXT has a JWT's three dot-separated base64url parts."
  (let ((parts (uiop:split-string text :separator ".")))
    (and (= 3 (length parts))
         (>= (length (first parts)) 3)
         (string= (subseq (first parts) 0 3) "eyJ")
         (every (lambda (part)
                  (every (lambda (character)
                           (or (ascii-alphanumeric-p character)
                               (find character "-_" :test #'char=)))
                         part))
                parts))))

(defun content-lines (data notes limits &optional (depth 0))
  "The description's Content lines for DATA, with a note for anything worth
saying."
  (if (zerop (length data))
      (list "Content: empty (0 bytes)")
      (let* ((known (kind-of data))
             (as-text (and (null known) (text-of data))))
        (cond
          ((or (equal known "gzip data")
               (and (null known) (null as-text) (equal (weak-kind data) "zlib data")))
           (inflated-lines data notes limits depth (or known "zlib data")))
          (known (append (list (format nil "Content: ~a, ~:d bytes" known (length data))
                               "First bytes (hex)")
                         (dump-lines data limits)))
          ((null as-text)
           (let ((weak (weak-kind data)))
             (append (list (if weak
                               (format nil "Content: ~a, ~:d bytes" weak (length data))
                               (format nil "Content: binary, ~:d bytes (no known format)" (length data)))
                           "First bytes (hex)")
                     (dump-lines data limits))))
          (t (text-content-lines (car as-text) (cdr as-text) notes limits depth))))))

(defun text-content-lines (text encoding notes limits depth)
  "The description's lines for TEXT, a decoded text, as text or as JSON."
  (when encoding (note-push notes (format nil "the text is ~a" encoding)))
  (let* ((stripped (trimmed text))
         (parsed (json-value text))
         (body (if parsed (json-text parsed) text))
         (count (max 1 (length (split-lines text)))))
    (multiple-value-bind (lines left hidden) (text-lines body limits)
      (let ((head (if parsed
                      (format nil "Content: ~a" (json-shape parsed))
                      (format nil "Content: text, ~:d characters, ~:d line~:p"
                              (length text) count))))
        (let ((out (append (list head (if parsed "JSON" "Text")) lines)))
          (when (plusp left)
            (setf out (append out (list (format nil "  ... ~:d more characters" left)))))
          (when (plusp hidden)
            (note-push notes (format nil "~:d control or invisible character~:p shown as \\x.. / \\u...." hidden)))
          (cond
            ((jwt-shaped-p stripped)
             (note-push notes "the text is a JWT: b64_decode on one of its parts decodes that part's JSON"))
            ((zerop depth)
             (let ((layer (other-layer stripped)))
               (cond
                 (layer (note-push notes (format nil "the text is base64 again (~a inside): b64_decode on the text decodes the next layer" layer)))
                 ((and (percent-present-p stripped)
                       (not (string= (percent-decoded-text stripped) stripped)))
                  (note-push notes "the text is still percent-encoded (encoded twice): b64_decode on the text decodes the next layer"))))))
          out)))))

(defun percent-decoded-text (text)
  "TEXT percent-decoded as text, for saying whether a value is encoded twice."
  (let ((bytes (decoded-data (decode-percent text (make-notes)))))
    (or (utf-8-decoded bytes) (latin-1-string bytes))))

(defun describe-value (decoded limits)
  "What the description of DECODED is: how it was read, what it holds, and
anything worth a note."
  (let* ((notes (make-notes (coerce (decoded-notes decoded) 'list)))
         (lines (list (format nil "~a: ~:d characters -> ~:d bytes"
                               (decoded-label decoded) (decoded-chars decoded)
                               (length (decoded-data decoded))))))
    (setf lines (append lines (content-lines (decoded-data decoded) notes limits)))
    (when (plusp (fill-pointer notes))
      (setf lines (append lines (list "Notes")
                          (loop for note across notes
                                collect (concatenate 'string "  - " note)))))
    (format nil "~{~a~^~%~}" lines)))

(defun expand-value (value &key forced (limits +limits+))
  "What a value puts into the answer: its description, decoded here."
  (describe-value (decode-value value :forced forced :limits limits) limits))
