;;;; proto.lisp --- protobuf by hand, the Connect envelope, and the byte helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): catalog/src/discovery/protobuf.ts (the
;;;; wire codec omp generates its Cursor messages over, and its
;;;; google.protobuf.Value writer and reader) and ai/src/providers/connect-
;;;; frame.ts (the Connect streaming envelope). Pure transforms, no I/O.
;;;;
;;;; No schema compiler: a message is written as the concatenation of its
;;;; fields in the order the .proto declares them, each field writer
;;;; answering NIL when proto3 leaves the field out, which is exactly what
;;;; omp's encoder does:
;;;;
;;;;   - a plain scalar (string, bytes, a number, a bool) is left out at its
;;;;     default ("", no bytes, 0, false); an `optional' one is written
;;;;     whenever it is set, its default included (the * writers)
;;;;   - a message field is written whenever it is set, empty included
;;;;   - repeated strings and messages repeat the field (no message here
;;;;     carries a repeated number, which omp would pack)
;;;;
;;;; A message read back is its list of fields, (NUMBER WIRE-TYPE VALUE) in
;;;; wire order: VALUE an integer for a varint, octets for everything else.
;;;; Fields this cell does not name ride along untouched, so a message the
;;;; server sent (a checkpoint) goes back with what this cell never read.

(in-package #:nodecode-cursor)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(define-condition malformed-proto (error)
  ((text :initarg :text :reader malformed-proto-text))
  (:report (lambda (condition stream) (write-string (malformed-proto-text condition) stream)))
  (:documentation "Bytes that are not the protobuf or Connect frame they claim to be."))

(defun malformed (control &rest arguments)
  (error 'malformed-proto :text (apply #'format nil control arguments)))

;;; --- octets ----------------------------------------------------------------------

(defun pb (&rest parts)
  "PARTS joined into one octet vector: each part octets, NIL (left out), or a
list of parts."
  (let ((flat '()))
    (labels ((walk (part)
               (cond ((null part))
                     ((consp part) (mapc #'walk part))
                     (t (push part flat)))))
      (mapc #'walk parts))
    (setf flat (nreverse flat))
    (let ((out (make-array (reduce #'+ flat :key #'length) :element-type '(unsigned-byte 8)))
          (at 0))
      (dolist (part flat out)
        (replace out part :start1 at)
        (incf at (length part))))))

(defun utf8 (string)
  "STRING's UTF-8 octets."
  (coerce (sb-ext:string-to-octets string :external-format :utf-8) 'octets))

(defun text-of (octets)
  "The text OCTETS spell as UTF-8, an invalid sequence read as U+FFFD."
  (sb-ext:octets-to-string (coerce octets 'octets)
                           :external-format (list :utf-8 :replacement (code-char #xfffd))))

(defun hex (octets)
  "OCTETS as lowercase hex."
  (with-output-to-string (out)
    (loop for byte across octets do (format out "~(~2,'0x~)" byte))))

(defun unhex (text)
  "The octets the hex TEXT spells."
  (let ((out (make-array (floor (length text) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length out) out)
      (setf (aref out i) (parse-integer text :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

;;; --- writing -----------------------------------------------------------------------

(defun varint (value)
  "VALUE as a protobuf varint; a negative VALUE as its 64-bit two's
complement, ten octets, as int32 and int64 write it."
  (let ((v (ldb (byte 64 0) value)) (bytes '()))
    (loop (if (< v #x80)
              (return (push v bytes))
              (progn (push (logior #x80 (logand v #x7f)) bytes)
                     (setf v (ash v -7)))))
    (coerce (nreverse bytes) 'octets)))

(defun field-key (number wire-type)
  "The tag of field NUMBER of WIRE-TYPE."
  (varint (logior (ash number 3) wire-type)))

(defun len-field (number octets)
  "Field NUMBER as a length-delimited OCTETS."
  (pb (field-key number 2) (varint (length octets)) octets))

(defun varint-field (number value)
  "Field NUMBER as the varint VALUE."
  (pb (field-key number 0) (varint value)))

(defun double-octets (value)
  "VALUE as an IEEE 754 double, little-endian."
  (let* ((d (coerce value 'double-float))
         (bits (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits d)) 32)
                       (sb-kernel:double-float-low-bits d)))
         (out (make-array 8 :element-type '(unsigned-byte 8))))
    (dotimes (i 8 out)
      (setf (aref out i) (ldb (byte 8 (* 8 i)) bits)))))

(defun octets-double (octets)
  "The double the eight little-endian OCTETS hold."
  (let ((bits (loop for i from 0 below 8 sum (ash (aref octets i) (* 8 i)))))
    (sb-kernel:make-double-float (let ((high (ldb (byte 32 32) bits)))
                                   (if (logbitp 31 high) (- high (ash 1 32)) high))
                                 (ldb (byte 32 0) bits))))

;;; The proto3 field writers: NIL when the field is left out.

(defun pb-string (number text)
  "A plain string field: left out when empty."
  (and text (plusp (length text)) (len-field number (utf8 text))))

(defun pb-string* (number text)
  "An optional string field: written whenever TEXT is set."
  (and text (len-field number (utf8 text))))

(defun pb-bytes (number octets)
  "A plain bytes field: left out when empty."
  (and octets (plusp (length octets)) (len-field number octets)))

(defun pb-bytes* (number octets)
  "An optional bytes field: written whenever OCTETS is set."
  (and octets (len-field number octets)))

(defun pb-int (number value)
  "A plain integer field (int32, int64, uint32, uint64, an enum): left out at 0."
  (and value (/= value 0) (varint-field number value)))

(defun pb-int* (number value)
  "An optional integer field: written whenever VALUE is set."
  (and value (varint-field number value)))

(defun pb-bool (number value)
  "A plain bool field: left out when false."
  (and value (varint-field number 1)))

(defun pb-bool* (number value)
  "An optional bool field: NIL leaves it out, :FALSE writes false, anything
else true."
  (cond ((null value) nil)
        ((eq value :false) (varint-field number 0))
        (t (varint-field number 1))))

(defun pb-message (number octets)
  "A message field: written whenever OCTETS is set, an empty message included."
  (and octets (len-field number octets)))

(defun pb-messages (number list)
  "A repeated message (or string) field, LIST already encoded."
  (mapcar (lambda (octets) (len-field number octets)) list))

(defun pb-strings (number list)
  "A repeated string field."
  (mapcar (lambda (text) (len-field number (utf8 text))) list))

(defun pb-bytes-map (number alist)
  "A map<string, bytes> field from ALIST of (KEY . OCTETS), in ALIST's order;
each entry leaves out an empty key or value, as omp's encoder does."
  (mapcar (lambda (entry)
            (len-field number (pb (pb-string 1 (car entry)) (pb-bytes 2 (cdr entry)))))
          alist))

;;; --- reading -----------------------------------------------------------------------

(defmacro ecase* (key &body clauses)
  "ECASE whose miss is MALFORMED-PROTO."
  (let ((value (gensym "KEY")))
    `(let ((,value ,key))
       (case ,value ,@clauses (t (malformed "unsupported protobuf wire type ~a" ,value))))))

(defun pb-decode (octets)
  "The fields of the message OCTETS hold, in wire order: (NUMBER WIRE-TYPE
VALUE), VALUE an integer for a varint, else the field's octets."
  (let ((pos 0) (end (length octets)) (fields '()))
    (labels ((byte* ()
               (when (>= pos end) (malformed "protobuf ends inside a field"))
               (prog1 (aref octets pos) (incf pos)))
             (varint* ()
               (loop for shift from 0 by 7
                     for byte = (byte*)
                     sum (ash (logand byte #x7f) shift) into value
                     when (> shift 63) do (malformed "protobuf varint exceeds 64 bits")
                     unless (logbitp 7 byte) return (ldb (byte 64 0) value)))
             (take (count)
               (when (> (+ pos count) end) (malformed "protobuf ends inside a field"))
               (prog1 (subseq octets pos (+ pos count)) (incf pos count))))
      (loop while (< pos end)
            do (let* ((tag (varint*)) (number (ash tag -3)) (wire-type (logand tag 7)))
                 (when (zerop number) (malformed "protobuf field number 0"))
                 (push (list number wire-type
                             (ecase* wire-type
                               (0 (varint*))
                               (1 (take 8))
                               (2 (take (varint*)))
                               (5 (take 4))))
                       fields))))
    (nreverse fields)))

(defun pb-get (fields number)
  "The value of the last field NUMBER in FIELDS (proto3: the last one wins), or NIL."
  (third (find number fields :key #'first :from-end t)))

(defun pb-all (fields number)
  "Every value of field NUMBER in FIELDS, in order."
  (loop for (n nil value) in fields when (= n number) collect value))

(defun pb-text (fields number)
  "The string field NUMBER of FIELDS, or NIL when absent."
  (let ((value (pb-get fields number)))
    (and (vectorp value) (text-of value))))

(defun pb-text* (fields number)
  "The string field NUMBER of FIELDS, \"\" when absent (proto3's default)."
  (or (pb-text fields number) ""))

(defun pb-sub (fields number)
  "The message field NUMBER of FIELDS decoded, or NIL when absent."
  (let ((value (pb-get fields number)))
    (and (vectorp value) (pb-decode value))))

(defun pb-has (fields number)
  "Whether FIELDS carries field NUMBER."
  (and (find number fields :key #'first) t))

(defun pb-signed (value)
  "VALUE, a 64-bit varint, as the signed integer int64 means."
  (and value (if (logbitp 63 value) (- value (ash 1 64)) value)))

(defun pb-oneof (fields numbers)
  "(values NUMBER VALUE) of the oneof member of FIELDS among NUMBERS: the
last one on the wire, as a decoder settles it; NIL when none is set."
  (let ((field (find-if (lambda (number) (member number numbers)) fields :key #'first :from-end t)))
    (and field (values (first field) (third field)))))

(defun pb-raw (field)
  "FIELD, (NUMBER WIRE-TYPE VALUE), written back as it arrived."
  (destructuring-bind (number wire-type value) field
    (ecase wire-type
      (0 (varint-field number value))
      ((1 5) (pb (field-key number wire-type) value))
      (2 (len-field number value)))))

;;; --- google.protobuf.Value --------------------------------------------------------
;;; MCP tool arguments and input schemas cross the wire as Value messages.
;;; JSON here is what NLK:DECODE-JSON makes of it: objects hash tables,
;;; arrays vectors, :NULL null, T and NIL the booleans.

(defun json-value-octets (value)
  "VALUE as a google.protobuf.Value message (writeJsonValue)."
  (cond ((eq value :null) (pb (field-key 1 0) (varint 0)))
        ((eq value t) (pb (field-key 4 0) (varint 1)))
        ((null value) (pb (field-key 4 0) (varint 0)))
        ((stringp value) (len-field 3 (utf8 value)))
        ((realp value) (pb (field-key 2 1) (double-octets value)))
        ((hash-table-p value)
         (len-field 5 (pb (let ((entries '()))
                            (maphash (lambda (key item)
                                       (push (len-field 1 (pb (len-field 1 (utf8 (string key)))
                                                              (len-field 2 (json-value-octets item))))
                                             entries))
                                     value)
                            (nreverse entries)))))
        ((vectorp value)
         (len-field 6 (pb (map 'list (lambda (item) (len-field 1 (json-value-octets item))) value))))
        (t (len-field 3 (utf8 (princ-to-string value))))))

(defun json-number (double)
  "DOUBLE as JSON text would carry it: an integral value as an integer."
  (if (and (= double (ffloor double)) (< (abs double) 1d15))
      (truncate double)
      double))

(defun octets-json-value (octets)
  "The JSON value a google.protobuf.Value message OCTETS holds (readJsonValue):
the last member wins, a message with none is null."
  (let ((value :null))
    (loop for (number wire-type item) in (pb-decode octets)
          do (case number
               (1 (setf value :null))
               (2 (when (= wire-type 1) (setf value (json-number (octets-double item)))))
               (3 (when (= wire-type 2) (setf value (text-of item))))
               (4 (when (= wire-type 0) (setf value (/= item 0))))
               (5 (when (= wire-type 2)
                    (setf value (let ((object (make-hash-table :test #'equal)))
                                  (loop for (n wt entry) in (pb-decode item)
                                        when (and (= n 1) (= wt 2))
                                          do (let ((fields (pb-decode entry)))
                                               (setf (gethash (pb-text* fields 1) object)
                                                     (let ((inner (pb-get fields 2)))
                                                       (if inner (octets-json-value inner) :null)))))
                                  object))))
               (6 (when (= wire-type 2)
                    (setf value (coerce (loop for (n wt entry) in (pb-decode item)
                                              when (and (= n 1) (= wt 2))
                                                collect (octets-json-value entry))
                                        'vector))))))
    value))

;;; --- the Connect envelope ----------------------------------------------------------
;;; One message on a Connect stream: a flag octet, the payload's length as four
;;; big-endian octets, the payload (connect-frame.ts).

(defconstant +connect-compressed+ #x01
  "Flag bit: the payload is compressed with the negotiated encoding.")

(defconstant +connect-end-stream+ #x02
  "Flag bit: the payload is the end-of-stream JSON trailer.")

(defun connect-frame (payload &optional (flags 0))
  "PAYLOAD wrapped as one Connect message."
  (let ((length (length payload)))
    (pb (coerce (list flags (ldb (byte 8 24) length) (ldb (byte 8 16) length)
                      (ldb (byte 8 8) length) (ldb (byte 8 0) length))
                'octets)
        payload)))

(defun connect-frames (octets)
  "The Connect messages OCTETS holds whole: a list of (FLAGS . PAYLOAD), and
as a second value the octets after the last whole one."
  (let ((pos 0) (frames '()))
    (loop while (>= (- (length octets) pos) 5)
          do (let* ((length (loop for i from 1 to 4 sum (ash (aref octets (+ pos i)) (* 8 (- 4 i)))))
                    (end (+ pos 5 length)))
               (when (> end (length octets)) (return))
               (push (cons (aref octets pos) (subseq octets (+ pos 5) end)) frames)
               (setf pos end)))
    (values (nreverse frames) (subseq octets pos))))

(defun connect-unary-body (octets)
  "The message of a unary answer that came Connect-framed, or NIL when
OCTETS is not that (decodeConnectUnaryBody): the first frame that is not the
end-of-stream trailer, unless a frame is cut short or compressed."
  (when (>= (length octets) 5)
    (let ((pos 0))
      (loop while (<= (+ pos 5) (length octets))
            do (let* ((flags (aref octets pos))
                      (length (loop for i from 1 to 4 sum (ash (aref octets (+ pos i)) (* 8 (- 4 i)))))
                      (end (+ pos 5 length)))
                 (when (> end (length octets)) (return nil))
                 (when (logtest flags +connect-compressed+) (return nil))
                 (unless (logtest flags +connect-end-stream+)
                   (return (subseq octets (+ pos 5) end)))
                 (setf pos end))))))

;;; --- identities ---------------------------------------------------------------------

(defun sha256-octets (octets)
  "The 32-octet SHA-256 digest of OCTETS."
  (unhex (subseq (nlk::sha256-text (coerce octets 'octets)) 7)))

(defun uuid ()
  "A random version-4 UUID (crypto.randomUUID)."
  (let ((bytes (nlk:random-bytes 16)))
    (setf (aref bytes 6) (logior #x40 (logand #x0f (aref bytes 6)))
          (aref bytes 8) (logior #x80 (logand #x3f (aref bytes 8))))
    (uuid-text (hex bytes))))

(defun uuid-text (hex)
  "The first 32 HEX digits in the 8-4-4-4-12 layout."
  (format nil "~a-~a-~a-~a-~a" (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
          (subseq hex 16 20) (subseq hex 20 32)))

(defun deterministic-uuid (seed)
  "The leading 128 bits of SEED's SHA-256 as a UUID-shaped string
(utils/deterministic-id.ts): the same seed always names the same id."
  (uuid-text (subseq (nlk::sha256-text seed) 7)))

(defun base64url (octets)
  "OCTETS in unpadded base64url, as Node's Buffer#toString(\"base64url\") writes it."
  (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ (cl-base64:usb8-array-to-base64-string
                                                                    (coerce octets 'octets))))))

(defun unbase64 (text)
  "The octets the base64 or base64url TEXT spells, padding optional, or NIL."
  (ignore-errors
   (let ((plain (substitute #\/ #\_ (substitute #\+ #\- (remove-if (lambda (c) (member c '(#\Newline #\Return #\Space)))
                                                                  text)))))
     (cl-base64:base64-string-to-usb8-array
      (concatenate 'string plain (make-string (mod (- (length plain)) 4) :initial-element #\=))))))
