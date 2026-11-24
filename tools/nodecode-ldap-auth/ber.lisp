;;;; ber.lisp --- the bytes of LDAPv3: BER, the requests, the replies, escaping.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; LDAP is BER-encoded ASN.1. The subset a password bind needs is small and
;;;; fixed: a BindRequest and its response, a SearchRequest, the entries and
;;;; the result that end it, an ExtendedRequest for StartTLS, and Unbind. Each
;;;; is written here once, and read back by a decoder that answers whatever the
;;;; directory sent rather than what we hoped for.

(in-package #:nodecode-ldap-auth)

;;; --- octets and text --------------------------------------------------------------

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun utf8 (string)
  "STRING as UTF-8 octets: what a DN, a filter value and a password are on the wire."
  (sb-ext:string-to-octets (or string "") :external-format :utf-8))

(defun text-of (octets)
  "OCTETS read back as text, never a failure: a directory value that is not
UTF-8 is shown byte for byte instead of costing a login."
  (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
    (error () (map 'string #'code-char octets))))

(defun octet-vector (bytes)
  "BYTES, a list of integers below 256, as an octet vector."
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun concat (&rest parts)
  "PARTS, each an octet vector, as one octet vector."
  (let* ((size (reduce #'+ parts :key #'length :initial-value 0))
         (out (make-array size :element-type '(unsigned-byte 8)))
         (at 0))
    (dolist (part parts out)
      (replace out part :start1 at)
      (incf at (length part)))))

(defun starts-with-p (prefix string &key (test #'char=))
  "Whether STRING begins with PREFIX."
  (let ((n (length prefix)))
    (and (<= n (length string)) (every test prefix string))))

;;; --- BER, written ----------------------------------------------------------------

(defun ber-length (n)
  "The BER length octets of N, as a list: short form under 128, else long form."
  (if (< n 128)
      (list n)
      (let ((bytes '()))
        (loop with x = n
              while (plusp x)
              do (push (logand x #xff) bytes)
                 (setf x (ash x -8)))
        (cons (logior #x80 (length bytes)) bytes))))

(defun tlv (tag content)
  "One BER element: TAG (a single octet), its length, CONTENT's octets."
  (octet-vector (append (list tag)
                         (ber-length (length content))
                         (coerce content 'list))))

(defun ber-integer (n &optional (tag #x02))
  "BER INTEGER (or ENUMERATED with TAG) of the non-negative N."
  (let ((bytes '()))
    (loop with x = n
          do (push (logand x #xff) bytes)
             (setf x (ash x -8))
          while (plusp x))
    (when (or (null bytes) (>= (first bytes) #x80))
      (push 0 bytes))
    (tlv tag (octet-vector bytes))))

(defun ber-string (string &optional (tag #x04))
  "BER OCTET STRING (or another primitive TAG) of STRING's UTF-8 octets."
  (tlv tag (utf8 string)))

(defun ber-boolean (value)
  "BER BOOLEAN."
  (tlv #x01 (octet-vector (list (if value #xff 0)))))

(defun ber-enumerated (n) (ber-integer n #x0a))
(defun ber-sequence (&rest parts) (tlv #x30 (apply #'concat parts)))
(defun ber-set (&rest parts) (tlv #x31 (apply #'concat parts)))

;;; --- BER, read -------------------------------------------------------------------

(defun read-length (octets pos)
  "=> (values LENGTH NEXT-INDEX) for the length octets at POS."
  (let ((first (aref octets pos)))
    (if (< first #x80)
        (values first (1+ pos))
        (let ((count (logand first #x7f))
              (n 0))
          (loop for i from 0 below count
                do (setf n (logior (ash n 8) (aref octets (+ pos 1 i)))))
          (values n (+ pos 1 count))))))

(defun read-tlv (octets pos)
  "=> (values TAG CONTENT NEXT-INDEX): one BER element read at POS."
  (when (>= pos (length octets))
    (fail "the directory's answer ended in the middle of an element"))
  (let ((tag (aref octets pos)))
    (multiple-value-bind (length body) (read-length octets (1+ pos))
      (let ((end (+ body length)))
        (when (> end (length octets))
          (fail "a directory answer is cut short: ~d of ~d octets" (length octets) end))
        (values tag (subseq octets body end) end)))))

(defun integer-of (octets)
  "OCTETS read as a non-negative BER INTEGER's value."
  (let ((n 0))
    (loop for byte across octets
          do (setf n (logior (ash n 8) byte)))
    n))

;;; --- escaping, so a username cannot be read as syntax ------------------------------

(defun escape-dn (username)
  "USERNAME escaped for a DN template (RFC 4514): the separators, the quote,
both ends of a value, and a leading # or space, so a username reads as one RDN
value and never as the DN's own syntax."
  (with-output-to-string (out)
    (loop for char across (or username "")
          for at from 0
          do (cond ((member char (list #\\ #\, #\+ #\" #\< #\> #\; #\=))
                    (write-char #\\ out) (write-char char out))
                   ((char= char #\Null)
                    (write-string "\\00" out))
                   ((and (zerop at) (char= char #\Space))
                    (write-string "\\ " out))
                   (t (write-char char out))))))

(defun escape-filter (username)
  "USERNAME escaped for a search filter (RFC 4515): backslash, the two
parentheses, the asterisk and NUL, each as its hex pair."
  (with-output-to-string (out)
    (loop for char across (or username "")
          do (case char
               (#\\ (write-string "\\5c" out))
               (#\* (write-string "\\2a" out))
               (#\( (write-string "\\28" out))
               (#\) (write-string "\\29" out))
               (#\Null (write-string "\\00" out))
               (t (write-char char out))))))

;;; --- the requests ----------------------------------------------------------------

(defparameter +starttls-oid+ "1.3.6.1.4.1.1466.20037"
  "The extended operation that upgrades a plain connection to TLS in place.")

;;; --- the requests ----------------------------------------------------------------

(defparameter +starttls-oid+ "1.3.6.1.4.1.1466.20037"
  "The extended operation that upgrades a plain connection to TLS in place.")

(defun bind-request (id dn password)
  "A BindRequest: LDAPv3, the simple authentication choice."
  (ber-sequence (ber-integer id)
                (tlv #x60 (concat (ber-integer 3)
                                   (ber-string dn)
                                   (tlv #x80 (utf8 password))))))

(defun encode-filter (filter)
  "FILTER as BER: (:and F...), (:or F...), (:not F), (:equal ATTRIBUTE VALUE),
(:present ATTRIBUTE). A value is escaped by the caller, once."
  (ecase (first filter)
    (:and (tlv #xa0 (apply #'concat (mapcar #'encode-filter (rest filter)))))
    (:or (tlv #xa1 (apply #'concat (mapcar #'encode-filter (rest filter)))))
    (:not (tlv #xa2 (encode-filter (second filter))))
    (:equal (tlv #xa3 (concat (ber-string (second filter))
                               (ber-string (third filter)))))
    (:present (tlv #x87 (utf8 (second filter))))))

(defparameter +subtree+ 2 "The search scope that walks everything under the base.")
(defparameter +base-object+ 0 "The search scope that reads the base entry alone.")

(defun search-request (id &key base scope filter attributes (time-limit 10))
  "A SearchRequest: never chase referrals, never ask for more than one entry."
  (ber-sequence (ber-integer id)
                (tlv #x63 (concat (ber-string base)
                                   (ber-enumerated (or scope +subtree+))
                                   (ber-enumerated 0)      ; derefAliases: never
                                   (ber-integer 0)         ; sizeLimit: the server's
                                   (ber-integer (max 0 (or time-limit 0)))
                                   (ber-boolean nil)       ; typesOnly
                                   (encode-filter filter)
                                   (ber-sequence
                                    (apply #'concat
                                           (mapcar #'ber-string attributes)))))))

(defun unbind-request (id)
  "An UnbindRequest, whose answer is the closed connection."
  (ber-sequence (ber-integer id) (tlv #x42 (octet-vector '()))))

(defun starttls-request (id)
  "An ExtendedRequest asking for StartTLS."
  (ber-sequence (ber-integer id)
                (tlv #x77 (ber-string +starttls-oid+ #x80))))

;;; --- the replies ----------------------------------------------------------------

(defparameter +result-success+ 0)
(defparameter +result-no-such-object+ 32)
(defparameter +result-invalid-credentials+ 49)

(defun result-message (op-tag id code &key (diagnostic ""))
  "A result-shaped reply (BindResponse, SearchResultDone, ExtendedResponse):
what a scripted directory answers, and the shape the reader expects."
  (ber-sequence (ber-integer id)
                (tlv op-tag (concat (ber-enumerated code)
                                     (ber-string "")         ; matchedDN
                                     (ber-string diagnostic)))))

(defun bind-response (id code &key (diagnostic ""))
  "A BindResponse carrying CODE."
  (result-message #x61 id code :diagnostic diagnostic))

(defun search-done (id code &key (diagnostic ""))
  "The SearchResultDone that ends a search."
  (result-message #x65 id code :diagnostic diagnostic))

(defun extended-response (id code &key (diagnostic ""))
  "An ExtendedResponse: what StartTLS is answered with."
  (result-message #x78 id code :diagnostic diagnostic))

(defun search-entry (id dn attributes)
  "A SearchResultEntry: DN and an alist of (name value) or (name value...)."
  (flet ((one (value) (if (listp value) (ber-set (apply #'concat (mapcar #'ber-string value)))
                           (ber-set (ber-string value)))))
    (ber-sequence (ber-integer id)
                  (tlv #x64 (concat (ber-string dn)
                                    (ber-sequence
                                     (apply #'concat
                                            (mapcar (lambda (attribute)
                                                      (ber-sequence (ber-string (first attribute))
                                                                    (one (second attribute))))
                                                    attributes))))))))

(defun parse-message (octets)
  "One LDAPMessage as a plist.

  (:id N :kind :result :code C :diagnostic TEXT)
  (:id N :kind :entry :dn DN :attributes a list of NAME VALUE pairs)

Anything else is :kind :other, which a reader can skip."
  (multiple-value-bind (tag body) (read-tlv octets 0)
    (unless (= tag #x30)
      (fail "the directory answered something that is not an LDAP message"))
    (multiple-value-bind (id-tag id-bytes pos) (read-tlv body 0)
      (declare (ignore id-tag))
      (multiple-value-bind (op-tag op-body) (read-tlv body pos)
        (list :id (integer-of id-bytes) :op op-tag
              :body (parse-op op-tag op-body))))))

(defun parse-op (tag body)
  "What an operation TAG's BODY holds, as a plist."
  (cond
    ((member tag '(#x61 #x65 #x67 #x78) :test #'=)
     (list :kind :result :code (read-code body) :diagnostic (read-diagnostic body)))
    ((= tag #x64)
     (multiple-value-bind (name-tag dn-bytes pos) (read-tlv body 0)
       (declare (ignore name-tag))
       (let ((attributes '()))
         (when (< pos (length body))
           (multiple-value-bind (attrs-tag attrs pos-2) (read-tlv body pos)
             (declare (ignore attrs-tag pos-2))
             (loop with at = 0
                   while (< at (length attrs))
                   do (multiple-value-bind (a-tag a-body next) (read-tlv attrs at)
                        (declare (ignore a-tag))
                        (multiple-value-bind (n-tag name n-body) (read-tlv a-body 0)
                          (declare (ignore n-tag))
                          (let ((values '()))
                            (when (< n-body (length a-body))
                              (multiple-value-bind (v-tag v-body next-v) (read-tlv a-body n-body)
                                (declare (ignore v-tag next-v))
                                (loop with v-at = 0
                                      while (< v-at (length v-body))
                                      do (multiple-value-bind (s-tag s-body next-s)
                                             (read-tlv v-body v-at)
                                           (declare (ignore s-tag))
                                           (push (text-of s-body) values)
                                           (setf v-at next-s)))))
                            (push (cons (text-of name) (nreverse values)) attributes))
                        (setf at next)))))
         (list :kind :entry :dn (text-of dn-bytes) :attributes (nreverse attributes))))))
    (t (list :kind :other))))

(defun read-code (body)
  "The resultCode of a result-shaped reply."
  (multiple-value-bind (tag code) (read-tlv body 0)
    (declare (ignore tag))
    (integer-of code)))

(defun read-diagnostic (body)
  "The diagnosticMessage of a result-shaped reply, or NIL."
  (multiple-value-bind (tag code pos) (read-tlv body 0)
    (declare (ignore tag code))
    (if (< pos (length body))
        (multiple-value-bind (m-tag matched m-pos) (read-tlv body pos)
          (declare (ignore m-tag matched))
          (if (< m-pos (length body))
              (multiple-value-bind (d-tag diagnostic) (read-tlv body m-pos)
                (declare (ignore d-tag))
                (text-of diagnostic))
              ""))
        "")))

(defun attribute-value (entry name)
  "ENTRY's first value for NAME, matched case-insensitively, or \"\"."
  (let ((found (assoc name (getf entry :attributes) :test #'string-equal)))
    (or (first (rest found)) "")))