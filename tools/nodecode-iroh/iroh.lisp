;;;; iroh.lisp --- pairing state, tickets, and the sidecar's two seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps this in Python around a Rust sidecar; here it is the
;;;; same state in the same shapes, so a peer running either one interoperates:
;;;; a ticket is `hermes-iroh://pair?peer=..&secret=..&ts=..&nonce=..&addr=..`,
;;;; fifteen minutes old, single use; a peer record carries the endpoint id,
;;;; its direct addresses and the pairing secret; a call is one JSON-RPC
;;;; round trip to `hermes-iroh-sidecar serve` on stdio.
;;;;
;;;; Files under the home, one directory (0700, files 0600): peers.json,
;;;; nonces.json, pairing.secret, endpoint.key.

(in-package #:nodecode-iroh)

(defparameter *ticket-lifetime* (* 15 60)
  "How long a pairing ticket may be presented, in seconds (the original's).

Two implementations must agree or a ticket one issues the other refuses.")

(defparameter *max-task-chars* 64000
  "The longest task text one call carries.")

(defvar *state* nil
  "The directory this cell keeps its state in; NIL is NLK:HOME's iroh/.")

(defvar *run-program* #'nlk:run-bounded
  "The one process seam: NLK:RUN-BOUNDED's shape, (ARGV &KEY SECONDS INPUT)
=> (VALUES OUTPUT ERROR STATUS). A test binds its own stand-in.")

(defun state-directory ()
  "Where peers, nonces and the pairing secret live."
  (or *state* (nlk:home "iroh/")))

(defun state-file (name)
  (merge-pathnames name (state-directory)))

;;; --- the small seams ---------------------------------------------------------

(defun current-seconds ()
  "Unix time: the unit a ticket's ts is in."
  (nlk:unix-now))

(defun fresh-token (&optional (bytes 24))
  "BYTES of randomness as lowercase hex."
  (string-downcase
   (format nil "~{~2,'0x~}" (coerce (nlk:random-bytes bytes) 'list))))

(defun read-json-object (path)
  "PATH as a decoded JSON object; an absent, empty or unreadable file is an empty one."
  (let ((text (ignore-errors (nlk:read-text path))))
    (or (and text
             (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
               (and (plusp (length trimmed))
                    (let ((object (ignore-errors (nlk:decode-json trimmed))))
                      (and (hash-table-p object) object)))))
        (nlk:json-object))))

(defun write-json-object (path object)
  "PATH := OBJECT, written whole and atomically, 0600."
  (nlk:write-file-atomically path (nlk:encode-json-object object)
                             :mode #o600 :directory-mode #o700)
  object)

;;; --- the peer registry -------------------------------------------------------

(defun peers-file () (state-file "peers.json"))

(defun all-peers ()
  "Every paired peer, as the JSON object the file holds."
  (read-json-object (peers-file)))

(defun find-peer (peer)
  "PEER's record, or NIL."
  (and (stringp peer) (plusp (length peer)) (gethash peer (all-peers))))

(defun put-peer (peer record)
  (with-iroh-lock
    (let ((peers (all-peers)))
      (setf (gethash peer peers) record)
      (write-json-object (peers-file) peers))))

(defun touch-peer (peer)
  "Record that PEER answered just now."
  (with-iroh-lock
    (let* ((peers (all-peers))
           (record (gethash peer peers)))
      (when record
        (setf (gethash "last_called" record) (nlk:iso-now))
        (write-json-object (peers-file) peers)))))

;;; --- nonces: one ticket, one pairing -----------------------------------------

(defun nonces-file () (state-file "nonces.json"))

(defun claim-nonce (nonce &key (now (current-seconds)))
  "Record NONCE as spent. => T the first time, NIL on a replay."
  (with-iroh-lock
    (let* ((spent (read-json-object (nonces-file)))
           (already (gethash nonce spent)))
      (if already
          nil
          (progn
            (let ((stale '()))
              (maphash (lambda (key value)
                         (when (and (numberp value)
                                    (< value (- now (* 2 *ticket-lifetime*))))
                           (push key stale)))
                       spent)
              (dolist (key stale) (remhash key spent)))
            (setf (gethash nonce spent) now)
            (write-json-object (nonces-file) spent)
            t)))))

;;; --- the pairing secret -------------------------------------------------------

(defun pairing-secret ()
  "This profile's pairing secret: a stable random token, created on first use."
  (let* ((path (state-file "pairing.secret"))
         (existing (ignore-errors (nlk:read-text path)))
         (token (and existing
                     (string-trim '(#\Space #\Tab #\Newline #\Return) existing))))
    (if (and token (plusp (length token)))
        token
        (let ((token (fresh-token)))
          (nlk:write-file-atomically path token :mode #o600 :directory-mode #o700)
          token))))

;;; --- tickets ------------------------------------------------------------------

(defun percent-encode (text)
  "TEXT with everything but an unreserved character percent-escaped."
  (with-output-to-string (out)
    (loop for char across text
          for code = (char-code char)
          do (if (or (alphanumericp char) (find char "-._~"))
                 (write-char char out)
                 (format out "%~2,'0X" (if (< code 128) code (char-code #\?)))))))

(defun percent-decode (text)
  "TEXT's percent escapes read back: %XX and + as a space."
  (with-output-to-string (out)
    (loop with length = (length text)
          for index = 0 then (1+ index)
          while (< index length)
          for char = (char text index)
          do (cond ((char= char #\+) (write-char #\Space out))
                   ((and (char= char #\%) (< (+ index 2) length)
                         (digit-char-p (char text (1+ index)) 16)
                         (digit-char-p (char text (+ index 2)) 16))
                    (write-char (code-char (parse-integer text :start (1+ index) :end (+ index 3)
                                                                :radix 16))
                                out)
                    (incf index 2))
                   (t (write-char char out))))))

(defun query-parameters (query)
  "QUERY as a plist of (:key \"value\" ...), a repeated key collected into a list."
  (let ((parameters '()))
    (dolist (pair (uiop:split-string query :separator '(#\&)))
      (let* ((equals (position #\= pair))
             (key (percent-decode (subseq pair 0 equals)))
             (value (percent-decode (if equals (subseq pair (1+ equals)) "")))
             (member (assoc key parameters :test #'string=)))
        (if member
            (setf (cdr member) (append (cdr member) (list value)))
            (push (cons key (list value)) parameters))))
    (loop for (key . values) in parameters
          collect (intern (string-upcase key) :keyword)
          collect (if (cdr values) values (car values)))))

(defun endpoint-id-p (text)
  "Whether TEXT is an Iroh endpoint id as a ticket carries one."
  (and (stringp text)
       (<= 4 (length text) 128)
       (every (lambda (char) (or (alphanumericp char) (find char "-_"))) text)))

(defun ticket-problem (raw &key (now (current-seconds)))
  "Why RAW is no valid pairing ticket: NIL when it is one, its parsed
plist (:peer :secret :ts :nonce :addrs) when it is. Fail-closed, offline:
scheme, peer id, secret length, expiry and the nonce's shape."
  (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or raw "")))
         (prefix "hermes-iroh://pair?"))
    (unless (uiop:string-prefix-p prefix text)
      (fail "~s is no pairing ticket: one reads hermes-iroh://pair?peer=...&secret=...&ts=...&nonce=..."
            (nlk:clip text 60)))
    (let* ((parameters (query-parameters (subseq text (length prefix))))
           (peer (getf parameters :peer))
           (secret (getf parameters :secret))
           (ts (getf parameters :ts))
           (nonce (getf parameters :nonce))
           (addrs (let ((value (getf parameters :addr)))
                    (cond ((null value) '())
                          ((listp value) value)
                          (t (list value))))))
      (unless (and (stringp peer) (endpoint-id-p peer))
        (fail "the ticket names no valid peer endpoint id"))
      (unless (and (stringp secret) (<= 16 (length secret)))
        (fail "the ticket carries no pairing secret (16 characters or more)"))
      (unless (and (stringp ts) (every #'digit-char-p ts) (plusp (length ts)))
        (fail "the ticket carries no timestamp ts"))
      (unless (and (stringp nonce)
                   (<= 16 (length nonce) 128)
                   (every (lambda (char) (find char "0123456789abcdef")) nonce))
        (fail "the ticket's nonce is malformed"))
      (let ((age (- now (parse-integer ts))))
        (when (< age -60)
          (fail "the ticket's timestamp is in the future"))
        (when (> age *ticket-lifetime*)
          (fail "the ticket expired ~a minutes ago: a ticket is good for ~a minutes"
                (floor age 60) (floor *ticket-lifetime* 60))))
      (list :peer peer :secret secret :ts (parse-integer ts) :nonce nonce :addrs addrs))))

(defun compose-ticket (&key peer secret ts nonce addrs)
  "The ticket a peer presents to pair with this agent."
  (format nil "hermes-iroh://pair?~{~a~^&~}"
          (append (list (format nil "peer=~a" (percent-encode peer))
                        (format nil "secret=~a" (percent-encode secret))
                        (format nil "ts=~a" ts)
                        (format nil "nonce=~a" (percent-encode nonce)))
                  (mapcar (lambda (addr) (format nil "addr=~a" (percent-encode addr))) addrs))))

;;; --- what leaves the machine ----------------------------------------------------

(defparameter +secret-words+ '("api_key" "api-key" "apikey" "secret" "token" "password" "passwd" "pwd")
  "The names a credential-shaped assignment starts with.")

(defun split-assignment (word)
  "WORD as (VALUES NAME VALUE) when it reads NAME=VALUE or NAME:VALUE, else NIL."
  (let ((at (or (position #\= word) (position #\: word))))
    (when (and at (plusp at) (< (1+ at) (length word)))
      (values (subseq word 0 at) (subseq word (1+ at))))))

(defun redact (text)
  "TEXT with credential-shaped tokens replaced, on its way to a peer.
A word after `bearer', and the value of an assignment whose name is a
secret word, keep only their first four characters."
  (let ((previous "")
        (words (uiop:split-string (or text "") :separator '(#\Space #\Newline #\Tab))))
    (format nil "~{~a~^ ~}"
            (mapcar (lambda (word)
                      (prog1
                          (cond ((and (string-equal previous "bearer") (<= 8 (length word)))
                                 (mask word))
                                (t (multiple-value-bind (name value) (split-assignment word)
                                     (if (and name (<= 8 (length value))
                                              (member (string-downcase name) +secret-words+
                                                      :test #'string=))
                                         (concatenate 'string name "=" (mask value))
                                         word))))
                        (setf previous word)))
                    words))))

(defun mask (value)
  (if (<= (length value) 4)
      "[REDACTED]"
      (format nil "~a[REDACTED]" (subseq value 0 4))))

;;; --- the sidecar ---------------------------------------------------------------

(defun sidecar-binary ()
  "The sidecar program the section names, or NIL."
  (let ((path (ignore-errors (setting :sidecar))))
    (and (stringp path)
         (plusp (length (string-trim '(#\Space) path)))
         (string-trim '(#\Space) path))))

(defun relay-arguments ()
  "The --relay flag, when the section names a policy other than the default."
  (let ((relay (ignore-errors (setting :relay))))
    (if (and (stringp relay) (plusp (length relay))
             (not (member relay '("default" "n0") :test #'string-equal)))
        (list "--relay" relay)
        '())))

(defun sidecar (arguments &key input seconds)
  "Run the sidecar with ARGUMENTS and INPUT on its stdin. => its stdout."
  (let ((binary (sidecar-binary)))
    (unless binary
      (fail "the iroh section names no sidecar: build the program in the original repository's
sidecar/ directory (cargo build --release) and put its path in iroh.sidecar"))
    (multiple-value-bind (output error status)
        (funcall *run-program* (cons binary arguments)
                 :seconds (or seconds (setting :timeout-seconds))
                 :input (or input ""))
      (unless (eql status 0)
        (fail "the sidecar exited ~a~@[: ~a~]" status
              (let ((text (nlk:one-line (or error ""))))
                (and (plusp (length text)) (nlk:clip text 300)))))
      (or output ""))))

(defun endpoint-info ()
  "This agent's endpoint id and direct addresses, or NIL without a sidecar."
  (when (sidecar-binary)
    (let* ((output (sidecar (append (list "addr" "--state-dir" (namestring (state-directory)))
                                    (relay-arguments))
                             :seconds 120))
           (object (ignore-errors (nlk:decode-json output)))
           (id (and (hash-table-p object) (nlk:json-value object :string "endpointId"))))
      (when id
        (list :id id
              :addrs (loop for addr across (nlk:json-array object "addrs")
                            for text = (nlk:json-value addr :string)
                            when text collect text))))))

(defun rpc (method &optional parameters)
  "One JSON-RPC round trip to `sidecar serve`. Answers its result object.
The process answers the one request and exits on the end of its stdin, so a
call leaves no daemon behind."
  (let* ((id 1)
         (request (nlk:encode-json-object
                   (nlk:json-object "jsonrpc" "2.0" "id" id "method" method
                                     "params" (or parameters (nlk:json-object)))))
         (output (sidecar (append (list "serve" "--state-dir" (namestring (state-directory)))
                                  (relay-arguments))
                          :input (format nil "~a~%" request)))
         (reply nil))
    (dolist (line (nlk:lines output))
      (let ((object (ignore-errors (nlk:decode-json line))))
        (when (and (hash-table-p object) (nlk:json-value object :integer "id"))
          (setf reply object))))
    (unless reply
      (fail "the sidecar answered ~a with no JSON-RPC reply" method))
    (let ((error (nlk:json-value reply :object "error")))
      (when error
        (fail "the sidecar refused ~a: ~a" method
              (or (nlk:json-value error :string "message") "no reason given"))))
    (or (nlk:json-value reply :object "result")
        (fail "the sidecar's ~a answer carried no result" method))))
