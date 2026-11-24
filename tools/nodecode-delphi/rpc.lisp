;;;; rpc.lisp --- the node side: keccak-256, ABI words, eth_call, gateway routing.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original got quotes and live prices from Gensyn's Node SDK, which
;;;; signed nothing but did call the chain. Here the same calls are made
;;;; directly as JSON-RPC eth_call over https: a 4-byte selector hashed with
;;;; keccak-256, the words Ethereum pads arguments into, and the result read
;;;; back as an integer. A quote reads the gateway the market belongs to, asks
;;;; that gateway what the trade costs, and reads the collateral's decimals.
;;;; Nothing is signed and nothing is sent.

(in-package #:nodecode-delphi)

;;; --- keccak-256 ----------------------------------------------------------------

(defconstant +word-mask+ #xFFFFFFFFFFFFFFFF
  "One 64-bit lane, kept inside its 64 bits.")

(declaim (inline rotate-lane))
(defun rotate-lane (lane count)
  "LANE turned left by COUNT bits."
  (if (zerop count) lane
      (logand +word-mask+ (logior (ash lane (- count 64)) (ash lane count)))))

(defparameter +round-constants+
  (vector #x0000000000000001 #x0000000000008082 #x800000000000808a #x8000000080008000
          #x000000000000808b #x0000000080000001 #x8000000080008081 #x8000000000008009
          #x000000000000008a #x0000000000000088 #x0000000080008009 #x000000008000000a
          #x000000008000808b #x800000000000008b #x8000000000008089 #x8000000000008003
          #x8000000000008002 #x8000000000000080 #x000000000000800a #x800000008000000a
          #x8000000080008081 #x8000000000008080 #x0000000080000001 #x8000000080008008)
  "Keccak's iota constants, one a round.")

(defparameter +rho+
  (vector 0 1 62 28 27  36 44 6 55 20  3 10 43 25 39  41 45 15 21 8  18 2 61 56 14)
  "Keccak's rho offsets, lane x + 5y.")

(defun keccak-permute (state)
  "STATE, 25 lanes, through one Keccak-f[1600] permutation."
  (declare (type (simple-array (unsigned-byte 64) (25)) state))
  (let ((column (make-array 5 :element-type '(unsigned-byte 64)))
        (delta (make-array 5 :element-type '(unsigned-byte 64)))
        (moved (make-array 25 :element-type '(unsigned-byte 64))))
    (dotimes (round 24)
      (dotimes (x 5)
        (setf (aref column x) (logxor (aref state x) (aref state (+ x 5)) (aref state (+ x 10))
                                       (aref state (+ x 15)) (aref state (+ x 20)))))
      (dotimes (x 5)
        (setf (aref delta x) (logxor (aref column (mod (+ x 4) 5))
                                      (rotate-lane (aref column (mod (+ x 1) 5)) 1))))
      (dotimes (lane 25) (setf (aref state lane) (logxor (aref state lane) (aref delta (mod lane 5)))))
      (dotimes (y 5)
        (dotimes (x 5)
          (setf (aref moved (+ y (* 5 (mod (+ (* 2 x) (* 3 y)) 5))))
                (rotate-lane (aref state (+ x (* 5 y))) (aref +rho+ (+ x (* 5 y)))))))
      (dotimes (y 5)
        (dotimes (x 5)
          (setf (aref state (+ x (* 5 y)))
                (logxor (aref moved (+ x (* 5 y)))
                        (logand (lognot (aref moved (+ (mod (+ x 1) 5) (* 5 y))))
                                (aref moved (+ (mod (+ x 2) 5) (* 5 y))))))))
      (setf (aref state 0) (logxor (aref state 0) (aref +round-constants+ round))))
    state))

(defun keccak-256 (octets)
  "OCTETS, a vector of bytes, as its Keccak-256 digest: 32 bytes."
  (let* ((rate 136)
         (length (length octets))
         (padded (make-array (* rate (1+ (floor length rate)))
                              :element-type '(unsigned-byte 8) :initial-element 0))
         (state (make-array 25 :element-type '(unsigned-byte 64) :initial-element 0)))
    (replace padded octets)
    (setf (aref padded length) #x01)
    (setf (aref padded (1- (length padded)))
          (logior (aref padded (1- (length padded))) #x80))
    (loop for offset from 0 below (length padded) by rate
          do (loop for lane from 0 below (floor rate 8)
                   do (let ((word 0))
                        (dotimes (byte 8)
                          (setf word (logior word (ash (aref padded (+ offset (* 8 lane) byte)) (* 8 byte)))))
                        (setf (aref state lane) (logxor (aref state lane) word))))
             (keccak-permute state))
    (let ((digest (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (i 32)
        (setf (aref digest i) (ldb (byte 8 (* 8 (mod i 8))) (aref state (floor i 8)))))
      digest)))

(defun octets-of (text)
  "TEXT's characters as bytes; every signature here is ASCII."
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun hex-of (octets)
  "OCTETS as lowercase hex, no 0x."
  (format nil "~(~{~2,'0x~}~)" (coerce octets 'list)))

(defun selector (signature)
  "SIGNATURE, such as \"quoteBuyExactOut(address,uint256,uint256)\", as its
4-byte selector in hex: the first four bytes of its keccak-256 digest."
  (subseq (hex-of (keccak-256 (octets-of signature))) 0 8))

;;; --- the words Ethereum pads arguments into ---------------------------------------

(defun word-of (number)
  "NUMBER, a non-negative integer, as one 32-byte word of hex."
  (unless (and (integerp number) (not (minusp number)) (< number (ash 1 256)))
    (fail "~s is not a 256-bit unsigned integer" number))
  (format nil "~64,'0x" number))

(defun word-of-address (address)
  "ADDRESS, 0x and 40 hex characters, as the word an argument takes."
  (concatenate 'string (make-string 24 :initial-element #\0)
               (string-downcase (subseq (check-address address "an address") 2))))

(defun word-array-of-addresses (addresses)
  "ADDRESSES as the tail of an address[] argument: the length, then each word."
  (with-output-to-string (out)
    (format out "~a" (word-of (length addresses)))
    (dolist (address addresses) (format out "~a" (word-of-address address)))))

(defun call-data (signature &rest words)
  "The 0x data of a call to SIGNATURE with WORDS, each already a word of hex."
  (format nil "0x~a~{~a~}" (selector signature) words))

(defun call-data-with-addresses (signature addresses)
  "The 0x data of a call to SIGNATURE taking one address[] argument: the offset
of the array, then the array itself."
  (format nil "0x~a~a~a" (selector signature) (word-of 32)
          (word-array-of-addresses addresses)))

(defun hex-digits (text)
  "TEXT's hex digits, without a leading 0x; NIL when it is not hex."
  (if (and (stringp text) (>= (length text) 3) (char= #\0 (char text 0)) (char= #\x (char text 1)))
      (subseq text 2)
      (and (stringp text) text)))

(defun unhex-number (text)
  "TEXT, 0x-prefixed hex, as an integer; NIL for anything else."
  (let ((digits (hex-digits text)))
    (and digits (plusp (length digits))
         (every (lambda (c) (digit-char-p c 16)) digits)
         (parse-integer digits :radix 16))))

(defun word-at (data index)
  "The 32-byte word of DATA at INDEX, counting from the first, as an integer."
  (let ((digits (hex-digits data)))
    (and digits (>= (length digits) (* 64 (1+ index)))
         (unhex-number (subseq digits (* 64 index) (* 64 (1+ index)))))))

(defun first-word (data)
  "The first 32-byte word of DATA as an integer."
  (word-at data 0))

(defun bool-array-at (data)
  "DATA read as the bool[] answer to an address[] argument: the offset of the
array, its length, then one flag a word."
  (let* ((digits (hex-digits data))
         (offset (and digits (word-at data 0)))
         (start (and offset (floor offset 32)))
         (count (and start (word-at data start))))
    (when (and digits start count (<= (* 64 (+ 1 start count)) (length digits)))
      (loop for i from 0 below count
            collect (let ((word (word-at data (+ start 1 i)))) (and word (plusp word)))))))

;;; --- JSON-RPC ------------------------------------------------------------------

(defun rpc-url ()
  "The node this network's reads use: the section's rpc_url, else the network's."
  (let ((given (setting :rpc-url)))
    (if (and (stringp given) (plusp (length given))) (string-right-trim "/" given)
        (getf (rest (network-entry)) :rpc))))

(defun rpc-call (to data &key what)
  "eth_call TO with DATA on the running network's node => the 0x result.
A node that answers with an error is a DELPHI-ERROR naming WHAT."
  (let* ((body (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"eth_call\",\"params\":[{\"to\":\"~a\",\"data\":\"~a\"},\"latest\"]}"
                       (string-downcase to) data))
         (headers (list (cons "content-type" "application/json")
                        (cons "accept" "application/json")
                        (cons "user-agent" +user-agent+))))
    (multiple-value-bind (text status)
        (handler-case (http :post (rpc-url) :headers headers :content body)
          (error (condition)
            (declare (ignore condition))
            (fail "~a: the node at ~a did not answer (no network, DNS or TLS); try again" what (rpc-url))))
      (let ((answer (and (stringp text) (plusp (length text)) (ignore-errors (nlk:decode-json text)))))
        (unless (= status 200)
          (fail "~a: the node answered HTTP ~d~@[ ~a~]" what status
                (and (hash-table-p answer) (jstr (jobj answer "error") "message"))))
        (unless (hash-table-p answer)
          (fail "~a: the node's answer was not JSON" what))
        (let ((error (jobj answer "error")))
          (when error
            (fail "~a: the node refused the call~@[: ~a~]" what (clip (jstr error "message") 160))))
        (let ((result (jstr answer "result")))
          (or (and result (plusp (length result)) result)
              (fail "~a: the node answered with no result" what)))))))

;;; --- Delphi's own calls ----------------------------------------------------------

(defparameter +market-proxies-exist+ "marketProxiesExist(address[])"
  "The factory's question: did this factory deploy these markets?")

(defparameter +quote-buy+ "quoteBuyExactOut(address,uint256,uint256)"
  "The gateway's price of a buy: market, outcome, shares out; what it costs.")

(defparameter +quote-sell+ "quoteSellExactIn(address,uint256,uint256)"
  "The gateway's price of a sell: market, outcome, shares in; what it pays.")

(defparameter +spot-probabilities+ "spotImpliedProbabilities(address,uint256[])"
  "The gateway's implied probabilities for some outcomes of one market.")
(defparameter +decimals+ "decimals()"
  "The collateral token's own decimals.")

(defun gateway-for (market)
  "The gateway that owns MARKET: the legacy one when only the legacy factory
deployed it, else the network's own."
  (let* ((entry (rest (network-entry)))
         (default (getf entry :gateway))
         (legacy (getf entry :legacy-gateway))
         (factory (getf entry :factory))
         (legacy-factory (getf entry :legacy-factory)))
    (if (or (null legacy) (null factory) (null legacy-factory))
        default
        (labels ((claims (contract)
                   (let ((answer (rpc-call contract (call-data-with-addresses +market-proxies-exist+ (list market))
                                           :what "delphi_quote")))
                     (let ((flags (bool-array-at answer)))
                       (and flags (first flags))))))
          (cond ((ignore-errors (claims factory)) default)
                ((ignore-errors (claims legacy-factory)) legacy)
                (t default))))))

(defun quote-raw (side market index shares-raw)
  "SIDE (:buy or :sell) of SHARES-RAW in outcome INDEX of MARKET => the raw
collateral amount, from the gateway that owns the market."
  (let* ((gateway (gateway-for market))
         (data (call-data (if (eq side :buy) +quote-buy+ +quote-sell+)
                          (word-of-address market) (word-of index) (word-of shares-raw)))
         (answer (rpc-call gateway data :what (if (eq side :buy) "delphi_quote_buy" "delphi_quote_sell")))
         (amount (first-word answer)))
    (unless amount
      (fail "~a: the gateway answered ~s, which is not an amount"
            (if (eq side :buy) "delphi_quote_buy" "delphi_quote_sell") (clip answer 80)))
    amount))

(defun collateral-decimals ()
  "The collateral token's decimals, or NIL when the node does not say."
  (let ((token (getf (rest (network-entry)) :token)))
    (and token
         (ignore-errors
          (first-word (rpc-call token (call-data +decimals+) :what "delphi_quote"))))))

(defun call-data-with-address-and-array (signature address numbers)
  "The 0x data of a call to SIGNATURE taking an address and a uint256[]: the
address word, the offset of the array, then the array itself."
  (with-output-to-string (out)
    (format out "0x~a~a~a~a~{~a~}" (selector signature) (word-of-address address)
            (word-of 64) (word-of (length numbers)) (mapcar #'word-of numbers))))

(defun number-array-at (data)
  "DATA read as the uint256[] a call answered: the offset, the length, then the
numbers."
  (let* ((digits (hex-digits data))
         (offset (and digits (word-at data 0)))
         (start (and offset (floor offset 32)))
         (count (and start (word-at data start))))
    (when (and digits start count (<= (* 64 (+ 1 start count)) (length digits)))
      (loop for i from 0 below count collect (word-at data (+ start 1 i))))))

(defun market-probabilities (market count)
  "The implied probabilities the gateway that owns MARKET gives its COUNT
outcomes, each as an exact fraction of 10^18, or NIL when the node does not
answer."
  (let* ((gateway (gateway-for market))
         (data (call-data-with-address-and-array +spot-probabilities+ market
                                                 (loop for i from 0 below count collect i)))
         (answer (rpc-call gateway data :what "delphi_compute_edge"))
         (numbers (number-array-at answer)))
    (and numbers (mapcar (lambda (n) (/ n (expt 10 18))) numbers))))
