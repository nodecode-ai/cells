;;;; wire.lisp --- the one seam, the address guard, and the shape of an answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original skill's curl, as a call: two GETs on the Grey Ridge x402 API,
;;;; every byte through NLK:HTTP, which a test stubs. Nothing here touches a
;;;; file, the environment or a shell, and the only thing sent is what the two
;;;; tools name: a token address, and for the gate a side and a dollar size.
;;;;
;;;; The API answers camelCase JSON with an `upgrade' object carrying the paid
;;;; route's own URL. An answer here is that report with snake_case keys, the
;;;; upgrade lifted into one line, and a note saying what the free tier is.

(in-package #:nodecode-token-safety)

;;; --- what the API is ------------------------------------------------------------

(defparameter +default-base-url+ "https://x402-data-api.sigrunner.workers.dev"
  "The Grey Ridge host the free preview is served from, named here so the
section's guide can spell it out; the section's api_base is what is used.")

(defparameter +token-report-path+ "/chain/token-report/preview"
  "The full report on one Base ERC-20: the trap checks, the measured transfer
tax, the liquidity and one verdict.")

(defparameter +swap-gate-path+ "/gate/swap/preview"
  "The sized go/hold/block decision for one swap.")

(defparameter +free-tier-note+
  "free preview of the Grey Ridge x402 API: three real-token checks a day per address, no key and no signup; the paid route is 0.01 USDC a call through an x402 client"
  "What every answer says about what it cost.")

(defparameter +address-length+ 42
  "An ERC-20 address is 0x and 40 hexadecimal characters.")

(defparameter +detail-chars+ 300
  "How much of a failed answer's body a refusal quotes.")

;;; --- the seam -------------------------------------------------------------------

(defun http (url &key (timeout 20))
  "The cell's one network call; a test stubs NLK:HTTP and never dials.
=> (values BODY STATUS)."
  (nlk:http :get url
            :headers (list (cons "accept" "application/json"))
            :timeout timeout
            :connect-timeout (min 10 timeout)))

(defun base-url ()
  "The section's api_base without a trailing slash."
  (string-right-trim "/" (or (setting :api-base) +default-base-url+)))

(defun query-string (params)
  "PARAMS -- an alist of (NAME . VALUE) with string names -- as a query
string. An address, a side and a number need no encoding beyond this."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in params
                when (and value (plusp (length (princ-to-string value))))
                  collect (format nil "~a=~a" name value))))

(defun api-url (path params)
  "PATH on the section's root, with PARAMS, absolutely."
  (format nil "~a~a~@[?~a~]" (base-url) path
          (let ((query (query-string params)))
            (and (plusp (length query)) query))))

(defun fetch (url)
  "URL's answer => (values OBJECT-OR-NIL STATUS BODY). An object only when the
body decoded into one; a dial that fails signals, and the verb's caller turns
that into the call's text."
  (multiple-value-bind (body status) (http url)
    (values (let ((object (ignore-errors (nlk:decode-json (or body "")))))
              (and (hash-table-p object) object))
            status
            body)))

;;; --- the guards -----------------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun address-p (text)
  "Whether TEXT is a 0x-prefixed, 40-digit hex address, as the original's
schema checked."
  (and (stringp text)
       (= (length text) +address-length+)
       (string-equal (subseq text 0 2) "0x")
       (every (lambda (character) (digit-char-p character 16)) (subseq text 2))))

(defun checked-address (text)
  "TEXT as a lowercase 0x address, or a refusal naming the shape it wants."
  (let ((address (nlk:trimmed (or text ""))))
    (unless (address-p address)
      (fail "~:[no token address was given~;~:*~s is not a token address~]: a Base ERC-20 contract is 0x followed by 40 hexadecimal characters, e.g. 0x4200000000000000000000000000000000000006"
            (and (plusp (length address)) address)))
    (string-downcase address)))

(defun checked-side (text)
  "TEXT as \"buy\" or \"sell\", the two sides the gate sizes, else a refusal."
  (let ((side (string-downcase (nlk:trimmed (or text "buy")))))
    (unless (member side '("buy" "sell") :test #'string=)
      (fail "~s is not a trade side: the gate sizes a \"buy\" or a \"sell\"" side))
    side))

(defun checked-notional (number)
  "NUMBER as a positive whole dollar size, else a refusal."
  (let ((size (or number 0)))
    (unless (and (integerp size) (plusp size))
      (fail "~s is not a trade size: give notional_usd as a positive whole number of dollars, e.g. 500" size))
    size))

;;; --- the answer's shape ---------------------------------------------------------

(defun snake-name (key)
  "KEY in snake_case: the API answers camelCase, an answer here reads
snake_case, as the rest of this image's JSON does."
  (let ((text (princ-to-string key)))
    (with-output-to-string (out)
      (loop for index from 0 below (length text)
            for character = (char text index)
            do (when (and (upper-case-p character) (plusp index))
                 (write-char #\_ out))
               (write-char (char-downcase character) out)))))

(defun relabel (value)
  "VALUE with every object key in snake_case, recursively; anything else as it
came."
  (cond ((hash-table-p value)
         (let ((out (nlk:json-object)))
           (maphash (lambda (key item)
                      (setf (gethash (snake-name key) out) (relabel item)))
                    value)
           out))
        ((and (vectorp value) (not (stringp value)))
         (map 'vector #'relabel value))
        (t value)))

(defun paid-route (raw)
  "The paid call the preview hands back in its `upgrade' object, as one line,
or NIL when the answer carries none."
  (let* ((upgrade (nlk:json-value raw :object "upgrade"))
         (url (nlk:json-value upgrade :string "paid_url"))
         (price (nlk:json-value upgrade :string "price_usdc")))
    (when url
      (format nil "~a~@[ (~a USDC a call, settled inline by an x402 client on Base)~]"
              url price))))

(defun report-object (raw token extra)
  "RAW -- the API's report -- as the answer a model reads: snake_case keys, the
upgrade lifted into paid_route, and a note saying what the free tier is.
EXTRA is a list of (KEY . VALUE) this cell adds, ahead of the report's own."
  (let ((answer (nlk:json-object))
        (body (relabel raw)))
    (remhash "upgrade" body)
    (setf (gethash "token" answer) token)
    (loop for (key . value) in extra
          do (setf (gethash key answer) value))
    (loop for key being the hash-keys of body using (hash-value value)
          do (setf (gethash key answer) value))
    (setf (gethash "tier" answer) +free-tier-note+)
    (let ((paid (paid-route raw)))
      (when paid (setf (gethash "paid_route" answer) paid)))
    answer))

(defun detail-of (object body)
  "The API's own sentence for a refused call: its error, detail or message
member, else the body's first clipped line."
  (or (nlk:json-value object :string "error")
      (nlk:json-value object :string "detail")
      (nlk:json-value object :string "message")
      (let ((text (nlk:one-line (or body ""))))
        (if (plusp (length text)) (nlk:clip text +detail-chars+) "no detail"))))

(defun reply (object)
  "OBJECT as the answer's text, fitted to the section's max_result_chars."
  (let* ((text (nlk:encode-json-object object))
         (cap (setting :max-result-chars)))
    (if (and (integerp cap) (> (length text) cap))
        (concatenate 'string (nlk:clip text (max 20 (- cap 80)))
                     " (clipped: raise max_result_chars for the whole report)")
        text)))

;;; --- the two requests ------------------------------------------------------------

(defun report-answer (token)
  "TOKEN's report as an answer object, or a refusal saying what the API said."
  (multiple-value-bind (object status body)
      (fetch (api-url +token-report-path+ (list (cons "token" token))))
    (cond ((and (integerp status) (<= 200 status 299) object)
           (report-object object token '()))
          ((eql status 429)
           (fail "the free preview is rate limited (HTTP 429): ~a. Try again tomorrow, or use the paid route. The API said: ~a" +free-tier-note+ (detail-of object body)))
          ((and object (member status '(400 422)))
           (fail "the API refused ~a (HTTP ~a): ~a" token status (detail-of object body)))
          (t
           (fail "the token-safety API answered HTTP ~a: ~a" status (detail-of object body))))))

(defun gate-answer (token side notional)
  "TOKEN's sized decision for a SIDE trade of NOTIONAL dollars, as an answer
object, or a refusal saying what the API said."
  (multiple-value-bind (object status body)
      (fetch (api-url +swap-gate-path+
                      (list (cons "token" token)
                            (cons "side" side)
                            (cons "notional_usd" notional))))
    (cond ((and (integerp status) (<= 200 status 299) object)
           (report-object object token
                          (list (cons "side" side)
                                (cons "notional_usd" notional))))
          ((eql status 429)
           (fail "the free swap gate is rate limited (HTTP 429): ~a. Try again tomorrow, or use the paid route. The API said: ~a" +free-tier-note+ (detail-of object body)))
          (t
           (fail "the token-safety API answered HTTP ~a: ~a" status (detail-of object body))))))
