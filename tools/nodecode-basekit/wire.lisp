;;;; wire.lisp --- hex and ABI reading, the one HTTP seam, the JSON-RPC client, prices.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through NLK:HTTP, from RPC-BATCH or
;;;; HTTP-JSON, so a test stubs the one name and never touches the network.
;;;; Base's public endpoints are keyless and rate-limited, so RPC-BATCH
;;;; batches a tool's reads into one POST and walks a short list of public
;;;; endpoints when one refuses; the endpoint that answered is remembered for
;;;; `full' status. Nothing here writes, signs or needs a key.
;;;;
;;;; The plugin's engine (basekit_core.py) is the source of the hex layout:
;;;; quantities are 0x-prefixed hex strings, addresses are one right-padded
;;;; 32-byte word, and a call's return data is a hex string whose words this
;;;; file indexes by 64 characters.

(in-package #:nodecode-basekit)

;;; --- where Base is read ---------------------------------------------------------

(defparameter +default-rpc+ "https://base-rpc.publicnode.com"
  "The keyless public Base endpoint every read uses unless the section names one.")

(defparameter +fallback-rpcs+ '("https://mainnet.base.org"
                                 "https://base.drpc.org"
                                 "https://base.meowrpc.com")
  "Tried in order when the preferred endpoint fails or rate-limits.")

(defparameter +chain-id+ 8453 "Base, the chain every read here states.")
(defparameter +chain-name+ "Base")
(defparameter +weth+ "0x4200000000000000000000000000000000000006"
  "WETH on Base: DefiLlama prices it, so it is the native ETH price too.")
(defparameter +defillama-url+ "https://coins.llama.fi/prices/current/")
(defparameter +coinbase-spot-url+ "https://api.coinbase.com/v2/prices/ETH-USD/spot")

(defvar *last-endpoint* nil
  "The endpoint the newest answered call went to, for `full' status.")

;;; --- hex and ABI -----------------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun strip-0x (text)
  "TEXT without a leading 0x."
  (if (and (stringp text) (>= (length text) 2)
           (char= (char text 0) #\0) (member (char text 1) '(#\x #\X)))
      (subseq text 2)
      (or text "")))

(defun hex-body-p (text)
  "TEXT is one or more hex digits."
  (and (stringp text) (plusp (length text)) (every (lambda (c) (digit-char-p c 16)) text)))

(defun address-p (value)
  "VALUE is a 0x-prefixed 20-byte EVM address."
  (and (stringp value) (let ((body (strip-0x value))) (and (= 40 (length body)) (hex-body-p body)))))

(defun tx-hash-p (value)
  "VALUE is a 0x-prefixed 32-byte transaction hash."
  (and (stringp value) (let ((body (strip-0x value))) (and (= 64 (length body)) (hex-body-p body)))))

(defun pad-word (hex)
  "HEX left-padded with zeros to one 32-byte word of 64 characters."
  (let ((body (string-downcase (strip-0x hex))))
    (if (>= (length body) 64) body (concatenate 'string (make-string (- 64 (length body)) :initial-element #\0) body))))

(defun number-word (number)
  "NUMBER as one 32-byte ABI word."
  (pad-word (format nil "~x" (mod number (ash 1 256)))))

(defun address-word (address)
  "ADDRESS as one 32-byte ABI word, the form a topic or an argument takes."
  (pad-word (strip-0x address)))

(defun hex-int (value &optional (default 0))
  "VALUE - a 0x quantity, a decimal string or an integer - as an integer."
  (cond ((integerp value) value)
        ((stringp value) (let ((body (strip-0x value)))
                           (or (and (plusp (length body)) (ignore-errors (parse-integer body :radix 16)))
                               default)))
        (t default)))

(defun word-at (body index)
  "Word INDEX of the return data BODY (a hex string), or NIL when it is short."
  (let ((start (* 64 index)))
    (when (and (stringp body) (<= (+ start 64) (length body)))
      (subseq body start (+ start 64)))))

(defun uint-at (body &optional (index 0))
  "Word INDEX of BODY as an unsigned integer."
  (let ((word (word-at body index)))
    (if word (parse-integer word :radix 16) 0)))

(defun address-at (body &optional (index 0))
  "Word INDEX of BODY as an address, or NIL."
  (let ((word (word-at body index)))
    (and word (concatenate 'string "0x" (subseq word 24)))))

(defun octets-of (hex)
  "HEX (no 0x needed) as an octet vector."
  (let* ((body (strip-0x hex))
         (count (floor (length body) 2))
         (octets (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (index count octets)
      (setf (aref octets index)
            (or (ignore-errors (parse-integer body :start (* 2 index) :end (+ 2 (* 2 index)) :radix 16))
                0)))))

(defun utf8 (octets)
  "OCTETS as a string; a malformed sequence reads with replacement characters."
  (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
    (error () (map 'string (lambda (octet) (if (< octet 128) (code-char octet) #\?)) octets))))

(defun text-of-hex (hex)
  "HEX as text, trimmed of the padding a bytes32 string carries."
  (string-trim '(#\Null #\Space #\Tab #\Newline #\Return) (utf8 (octets-of hex))))

(defun abi-string (body)
  "The ABI string BODY carries - dynamic (offset, length) or bytes32 padding -
or NIL when BODY is not a string at all."
  (when (and body (>= (length body) 64))
    (let* ((first (parse-integer (subseq body 0 64) :radix 16))
           (total (length body)))
      (cond
        ;; dynamic string: word 0 is an offset into the payload, word 1 its length
        ((and (plusp first) (<= (+ (* 2 first) 64) total))
         (let* ((start (* 2 first))
                (length (parse-integer (subseq body start (+ start 64)) :radix 16)))
           (when (<= length (- total start 64))
             (text-of-hex (subseq body (+ start 64) (+ start 64 (* 2 length)))))))
        ;; bytes32 string: NUL-padded, the way older tokens answer symbol()
        (t (let* ((raw (subseq body 0 64))
                  (end (or (search "00" raw) (length raw)))
                  (end (if (evenp end) end (1- end))))
             (text-of-hex (subseq raw 0 (max 0 end)))))))))

(defun round-to (value places)
  "VALUE rounded to PLACES decimals."
  (let* ((factor (expt 10 places))
         (scaled (* (coerce value 'double-float) (coerce factor 'double-float))))
    (/ (float (round scaled) 1d0) (coerce factor 'double-float))))

(defun gwei (wei)
  "WEI as gwei, rounded to six decimals."
  (round-to (/ (coerce wei 'double-float) 1d9) 6))

(defun eth-of-wei (wei)
  "WEI as ETH, rounded to nine decimals."
  (round-to (/ (coerce wei 'double-float) 1d18) 9))

(defun usd-of-wei (wei price)
  "The dollar value of WEI at PRICE, or NIL when there is no price."
  (and price (round-to (* (/ (coerce wei 'double-float) 1d18) price) 6)))

(defun units (raw decimals)
  "RAW token units as a decimal amount."
  (if (and (integerp decimals) (plusp decimals))
      (round-to (/ (coerce raw 'double-float) (coerce (expt 10 decimals) 'double-float)) 6)
      (coerce raw 'double-float)))

(defun clip-text (text limit)
  "TEXT cut to LIMIT characters."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (concatenate 'string (subseq text 0 limit) "..."))))

;;; --- HTTP ------------------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER - an HTTP body - as a decoded JSON value; anything else is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (nlk:trimmed answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((hash-table-p answer) answer)
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun http-json (url &key content (timeout (setting :timeout-seconds)))
  "One JSON exchange with URL => the decoded answer. A transport failure or a
refused status signals BASEKIT-ERROR, which the model repairs from."
  (let ((answer nil) (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http (if content :post :get) url
                    :headers (if content
                                 '(("content-type" . "application/json"))
                                 '(("accept" . "application/json")))
                    :content content
                    :timeout timeout
                    :connect-timeout (min 10 timeout)))
      (error (condition)
        (fail "basekit: ~a: ~a" url (nle:transport-failure-label condition url))))
    (cond ((and (integerp status) (< status 300)) (decode-body answer))
          (t (fail "basekit: ~a answered HTTP ~a~@[: ~a~]"
                   url status (clip-text (princ-to-string (or answer "")) 160))))))

;;; --- JSON-RPC --------------------------------------------------------------------

(defun endpoint-name (url)
  "URL without its scheme, as a status line shows it."
  (let ((position (search "//" url)))
    (if position (subseq url (+ position 2)) url)))

(defun rpc-endpoints ()
  "The configured endpoint first, then the public ones, without duplicates."
  (remove-duplicates (cons (or (nonempty (setting :rpc-url)) +default-rpc+) +fallback-rpcs+)
                     :test #'string=))

(defstruct (reply (:constructor %reply (value error)))
  "One JSON-RPC call's answer: its VALUE, or the ERROR text the endpoint sent."
  (value nil)
  (error nil))

(defun rpc-payload (calls)
  "CALLS - a list of (METHOD . PARAMS-JSON) - as one JSON-RPC batch body."
  (format nil "[~{~a~^,~}]"
          (loop for (method . params) in calls
                for id from 0
                collect (format nil "{\"jsonrpc\":\"2.0\",\"id\":~d,\"method\":~s,\"params\":~a}"
                                id method params))))

(defun rpc-error-text (item)
  "ITEM's JSON-RPC error as one line."
  (let ((error (nlk:json-value item :object "error")))
    (or (nlk:json-value error :text "message")
        (princ-to-string (or error "error")))))

(defun unwrap-answers (answer expected)
  "ANSWER - one decoded JSON-RPC response - as a list of REPLY, one per call id,
or NIL when the shape is not a response at all."
  (let ((by-id (make-hash-table)) (replies '()))
    (cond ((hash-table-p answer) (setf (gethash 0 by-id) answer))
          ((and (vectorp answer) (not (stringp answer)))
           (loop for item across answer
                 when (hash-table-p item)
                   do (setf (gethash (or (nlk:json-value item :integer "id") 0) by-id) item)))
          (t (return-from unwrap-answers nil)))
    (dotimes (index expected (nreverse replies))
      (let ((item (gethash index by-id)))
        (push (cond ((null item) (%reply nil "the endpoint answered no result for this call"))
                    ((nlk:json-value item :object "error") (%reply nil (rpc-error-text item)))
                    (t (%reply (nlk:json-value item :any "result") nil)))
              replies)))))

(defun rpc-batch (calls &key (timeout (setting :timeout-seconds)))
  "CALLS - a list of (METHOD . PARAMS-JSON) - as one POST => a list of REPLY in order.
The configured endpoint goes first; a transport failure or a refused status on
one walks to the next public endpoint, and only every endpoint failing is an
error the tool reports."
  (let ((payload (rpc-payload calls))
        (failures '()))
    (dolist (url (rpc-endpoints)
                  (fail "basekit: no Base RPC endpoint answered (~{~a~^; ~})" (nreverse failures)))
      (let ((answer nil) (note nil))
        (handler-case
            (multiple-value-bind (body status)
                (nlk:http :post url
                          :headers '(("content-type" . "application/json"))
                          :content payload
                          :timeout timeout
                          :connect-timeout (min 10 timeout))
              (if (and (integerp status) (< status 300))
                  (setf answer (decode-body body))
                  (setf note (format nil "HTTP ~a" status))))
          (error (condition)
            (setf note (nle:transport-failure-label condition url))))
        (cond
          (answer
           (let ((replies (unwrap-answers answer (length calls))))
             (when replies
               (setf *last-endpoint* url)
               (return replies))))
          (t (push (format nil "~a: ~a" (endpoint-name url) (or note "no answer")) failures)))))))

(defun rpc (method params)
  "One JSON-RPC call => its result; an endpoint error signals BASEKIT-ERROR."
  (let ((reply (first (rpc-batch (list (cons method params))))))
    (when (reply-error reply)
      (fail "basekit: ~a: ~a" method (reply-error reply)))
    (reply-value reply)))

(defun head-block ()
  "The chain's newest block number."
  (hex-int (rpc "eth_blockNumber" "[]")))

(defun call-params (to data)
  "eth_call's params for TO with calldata DATA."
  (format nil "[{\"to\":~s,\"data\":~s},\"latest\"]" to data))

(defun safe-call-value (reply)
  "A replied eth_call's return data as a hex body, or NIL when it reverted or was empty."
  (let ((value (reply-value reply)))
    (when (and (stringp value) (plusp (length (strip-0x value))))
      (string-downcase (strip-0x value)))))

(defun safe-eth-call (to data)
  "eth_call TO with DATA => its return data as a hex body, or NIL when it reverted."
  (safe-call-value (first (rpc-batch (list (cons "eth_call" (call-params to data)))))))

(defun eth-calls (requests)
  "REQUESTS - a list of (TO . DATA) - in one batch => their return data, NIL where a call reverted."
  (loop for reply in (rpc-batch (loop for (to . data) in requests
                                      collect (cons "eth_call" (call-params to data))))
        collect (safe-call-value reply)))

(defun topic-text (topic)
  "One eth_getLogs topic as JSON: a 32-byte word as 0x hex, NIL as a wildcard."
  (if (stringp topic)
      (format nil "~s" (concatenate 'string "0x" (pad-word topic)))
      "null"))

(defun log-position (log)
  "LOG's place in the chain, for ordering a window newest first."
  (+ (* 1000000 (hex-int (nlk:json-value log :string "blockNumber"))) (hex-int (nlk:json-value log :string "logIndex"))))

(defun get-logs (address topics from to &key (limit 400))
  "ADDRESS-filtered logs for TOPICS between block FROM and TO, oldest first.
Public endpoints require the address filter and cap the range, so a rejected
window is an error the tool states rather than an empty list, which would be a
claim about the chain."
  (let* ((params (format nil "[{\"address\":~s,\"topics\":[~{~a~^,~}],\"fromBlock\":\"0x~x\",\"toBlock\":\"0x~x\"}]"
                          address (mapcar #'topic-text topics) from to))
         (reply (first (rpc-batch (list (cons "eth_getLogs" params))))))
    (if (reply-error reply)
        (fail "basekit: eth_getLogs for ~a over ~d blocks was rejected (~a); public Base RPCs cap the range, so name a smaller blocks value or your own rpc_url"
              address (- to from) (reply-error reply))
        (let ((logs (sort (loop for log across (nlk:json-value (reply-value reply) :array)
                                when (hash-table-p log) collect log)
                          #'< :key #'log-position)))
          (if (> (length logs) limit) (subseq logs 0 limit) logs)))))

;;; --- US dollar prices ---------------------------------------------------------------

(defun coinbase-eth-price ()
  "ETH/USD from Coinbase's public spot endpoint, or NIL."
  (handler-case
      (let* ((body (http-json +coinbase-spot-url+ :timeout 10))
             (amount (nlk:json-value body :string "data" "amount"))
             (*read-eval* nil))
        (and amount (let ((value (ignore-errors (read-from-string amount))))
                      (and (realp value) (float value)))))
    (error () nil)))

(defun prices (addresses)
  "USD prices for ADDRESSES - a list of token addresses - and for native ETH,
from DefiLlama (keyless), with Coinbase's spot as the ETH fallback.
=> (values TABLE SOURCES): TABLE maps a lowercase address and \"eth\" to a price."
  (let ((table (make-hash-table :test #'equal))
        (sources '()))
    (let ((wanted (remove-duplicates (cons +weth+ (remove-if-not #'address-p addresses))
                                     :test #'string-equal)))
      (handler-case
          (let* ((query (format nil "~{~a~^,~}"
                                 (mapcar (lambda (address) (format nil "base:~a" (string-downcase address)))
                                         wanted)))
                 (body (http-json (concatenate 'string +defillama-url+ query)
                                  :timeout (min 12 (setting :timeout-seconds))))
                 (coins (nlk:json-value body :object "coins")))
            (when coins
              (maphash (lambda (key info)
                         (let* ((address (string-downcase (car (last (uiop:split-string key :separator ":")))))
                                (price (nlk:json-value info :number "price")))
                           (when (and price (plusp price))
                             (setf (gethash address table) (float price)))))
                       coins))
            (when (plusp (hash-table-count table)) (push "defillama" sources)))
        (error () nil)))
    (let ((eth (gethash (string-downcase +weth+) table)))
      (cond (eth (setf (gethash "eth" table) eth))
            (t (let ((spot (coinbase-eth-price)))
                 (when spot
                   (setf (gethash "eth" table) spot)
                   (push "coinbase" sources))))))
    (values table (nreverse sources))))

(defun eth-price-usd ()
  "ETH in dollars, or NIL when no source answered."
  (multiple-value-bind (table sources) (prices '())
    (declare (ignore sources))
    (let ((price (gethash "eth" table)))
      (and price (round-to price 2)))))
