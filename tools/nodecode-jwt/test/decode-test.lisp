;;;; decode-test.lisp --- the decoder's half: a token in, the description out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the header's fields and what their names mean, the
;;;; claims, a NumericDate shown as its UTC date and against a pinned clock,
;;;; the status lines, the notes, a payload that is not a JSON object, a JWE,
;;;; and a token that is not one answering with the reason.

(in-package #:nodecode.test)

(deftest jwt-cell-the-classic-token-reads-back ()
  (let ((answer (jwt-expand +classic+)))
    (is (search "JWT, signed (JWS), 3 parts; the signature is NOT verified" answer) "how many parts, and that nothing was verified")
    (is (search "Header" answer) "the header is listed")
    (is (search "alg (algorithm)" answer) "a known name says what it means")
    (is (search "HMAC with SHA-256" answer) "and the algorithm is spelled out")
    (is (search "(subject)" answer) "a claim's name is explained too")
    (is (search (jwt-clock 1516239022) answer) "iat is shown as its UTC date")
    (is (search (jwt-clock +now+) answer) "the status is said at the clock the test pins")
    (is (search "no exp claim: the token does not expire on its own" answer) "a token with no exp says so")
    (is (search "signature: 11 bytes, not checked" answer) "the signature is counted, never checked")
    (is (search "Claims" answer) "the claims are listed")))

(deftest jwt-cell-an-expiry-is-said-against-now ()
  (let ((valid (jwt-expand (jwt-token "{\"alg\":\"HS256\"}"
                                       (format nil "{\"exp\":~d,\"iat\":~d}"
                                               (+ +now+ 300) (- +now+ 700))))))
    (is (search "valid for 5 minutes more" valid) "an unexpired token says how long it has left")
    (is (search "lifetime 16 minutes 40 seconds (exp - iat)" valid) "and how long it was made for"))
  (let ((stale (jwt-expand (jwt-token "{\"alg\":\"HS256\"}"
                                       (format nil "{\"exp\":~d}" (- +now+ 3600))))))
    (is (search "EXPIRED 1 hour ago" stale) "an expired token says how long ago"))
  (let ((early (jwt-expand (jwt-token "{\"alg\":\"HS256\"}"
                                       (format nil "{\"nbf\":~d,\"exp\":~d}"
                                               (+ +now+ 600) (+ +now+ 9000))))))
    (is (search "NOT VALID YET: usable in 10 minutes" early) "a future nbf is called out"))
  (let ((future (jwt-expand (jwt-token "{\"alg\":\"HS256\"}"
                                        (format nil "{\"iat\":~d,\"exp\":~d}"
                                                (+ +now+ 600) (+ +now+ 9000))))))
    (is (search "iat is in the future" future) "an iat in the future is called out")))

(deftest jwt-cell-a-millisecond-time-is-called-out ()
  (let ((answer (jwt-expand (jwt-token "{\"alg\":\"HS256\"}"
                                        (format nil "{\"exp\":~d}" 1700000000000)))))
    (is (search "exp looks like milliseconds" answer) "the status says what a server would see")
    (is (search "(looks like milliseconds; JWT times are seconds) = " answer) "and the row says what the number would be as seconds")
    (is (search (jwt-clock 1700000000) answer) "which is the date that number names")))

(deftest jwt-cell-the-notes-name-what-is-wrong ()
  (let ((two (jwt-expand (format nil "~a.~a"
                                 (jwt-encode "{\"alg\":\"HS256\"}")
                                 (jwt-encode "{\"sub\":\"u1\"}")))))
    (is (search "only 2 parts" two) "a token with its final dot taken for punctuation is noted"))
  (is (search "the header has no alg" (jwt-expand (jwt-token "{\"typ\":\"JWT\"}" "{}"))) "a header with no alg is noted")
  (let ((unsigned (jwt-expand (jwt-token "{\"alg\":\"none\"}" "{\"sub\":\"u1\"}"))))
    (is (search "alg is \"none\"" unsigned) "alg none is the loudest note"))
  (let ((lying (jwt-expand (jwt-token "{\"alg\":\"none\"}" "{\"sub\":\"u1\"}" "still-here"))))
    (is (search "yet part 3 is not empty" lying) "and a signature that alg none cannot have"))
  (is (search "signature (part 3) is empty although alg is \"HS256\""
              (jwt-expand (jwt-token "{\"alg\":\"HS256\"}" "{\"sub\":\"u1\"}" nil))) "an empty signature under a real alg is noted")
  (is (search "the header points to its own key (jku)"
              (jwt-expand (jwt-token "{\"alg\":\"HS256\",\"jku\":\"https://example.test/keys\"}" "{}"))) "a key the token names itself")
  (is (search "crit lists header fields a server must understand or reject the token: [\"exp\"]"
              (jwt-expand (jwt-token "{\"alg\":\"HS256\",\"crit\":[\"exp\"]}" "{\"exp\":1}"))) "a crit list is shown whole"))

(deftest jwt-cell-a-name-twice-is-shown-as-the-last-one ()
  (let ((answer (jwt-expand (jwt-token "{\"alg\":\"HS256\",\"alg\":\"RS256\"}"
                                        "{\"exp\":1,\"exp\":2}"))))
    (is (search "\"RS256\" = RSA PKCS#1 v1.5 with SHA-256" answer) "the last value is the one a parser keeps")
    (is (search "names that appear twice (JSON keeps the last one, other parsers may not): header alg, exp"
                answer) "and both places say a name was written twice")))

(deftest jwt-cell-an-encrypted-token-shows-its-header-only ()
  (let ((answer (jwt-expand (format nil "~a.~a.~a.~a.~a"
                                    (jwt-encode "{\"alg\":\"RSA-OAEP\",\"enc\":\"A256GCM\"}")
                                    (jwt-encode "ciphertext")
                                    (jwt-encode "key") (jwt-encode "iv") (jwt-encode "tag")))))
    (is (search "JWT, encrypted (JWE), 5 parts" answer) "a JWE is named as one")
    (is (search "enc (encryption)" answer) "its header is listed")
    (is (search "the claims cannot be read without the recipient's key" answer) "and its payload is not guessed at")
    (is (not (search "Claims" answer)) "no claims section")))

(deftest jwt-cell-a-payload-that-is-not-an-object-is-shown-as-text ()
  (let ((answer (jwt-expand (jwt-token "{\"alg\":\"HS256\",\"cty\":\"JWT\"}" "not json at all"))))
    (is (search "Payload (not a JSON object, 15 bytes)" answer) "the payload's size is said")
    (is (search "\"not json at all\"" answer) "and its text is shown as a JSON string")
    (is (search "cty is JWT: the payload is itself a token" answer) "a nested token is pointed at")))

(deftest jwt-cell-what-is-not-a-token-says-why ()
  (is (search "no token given" (or (jwt-refusal "") "")) "an empty value says what a token is")
  (is (search "a JWT has 3 parts (signed) or 5 parts (encrypted) separated by dots; this has 1"
              (or (jwt-refusal "not-a-token") "")) "a value with no dots says how many parts it has")
  (is (search "the header (part 1) is not base64url" (or (jwt-refusal "***.###.sig") "")) "a part outside the alphabet says which part")
  (is (search "the header (part 1) decodes, but not to JSON, so this is not a JWT"
              (or (jwt-refusal (format nil "~a.~a.~a" (jwt-encode "hello") (jwt-encode "{}") "sig")) "")) "a header that is not JSON says so")
  (is (search "the header (part 1) is JSON but not an object"
              (or (jwt-refusal (jwt-token "[]" "{}")) "")) "and a header that is not an object says so")
  (is (search "reads at most 16,384"
              (or (jwt-refusal (make-string 20000 :initial-element #\a)) "")) "a token past the ceiling is refused, not cut"))

(deftest jwt-cell-a-bearer-token-and-padding-are-taken ()
  (let ((padded (format nil "~a.~a.~a=="
                         (jwt-encode "{\"alg\":\"HS256\"}")
                         (jwt-encode "{\"sub\":\"u1\"}")
                         (jwt-encode "sig"))))
    (is (search "HS256" (jwt-expand padded)) "padding on a part is optional and taken")
    (is (search "HS256" (jwt-expand (format nil "Bearer ~a" padded))) "a Bearer prefix is stripped")
    (is (search "HS256" (jwt-expand (format nil "  \"~a\"  " padded))) "and surrounding quotes")))
