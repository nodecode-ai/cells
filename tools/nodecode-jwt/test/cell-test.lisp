;;;; cell-test.lisp --- the cell's half: the tool, the command, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the declaration installs the tool, /jwt and
;;;; (help :jwt) and takes all three back on stop; jwt_decode answers the
;;;; description the original's @jwt: reference put in the message, called
;;;; through the wire a provider's call comes through; a value that is not a
;;;; token comes back as a line the model can act on rather than a failed call;
;;;; /jwt answers the same for the operator; a call with no token is refused at
;;;; the boundary; and the section's bounds are the ones the reads keep to.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "jwt"
  (:config)
  (:help :jwt)
  (:command "jwt")
  (:running
   (is (= 16384 (nodecode-jwt::setting :max-token-chars)) "the original's own ceiling on a token")
   (is (= 60 (nodecode-jwt::setting :max-claims)) "and on how many claims are listed")
   (is (= 200 (nodecode-jwt::setting :max-value-chars)) "and on how much of one value is shown"))
  (:refused ("max_claims" 0)))

(deftest jwt-cell-the-tool-answers-the-description ()
  (with-cell-stop ((jwt-start))
    (let ((answer (jwt-tool "jwt-call-1" (jwt-arguments +classic+))))
      (is (search "JWT, signed (JWS), 3 parts" answer) "the token is decoded")
      (is (search "HMAC with SHA-256" answer) "the algorithm is spelled out")
      (is (search "1234567890" answer) "and the claims are listed"))
    (let ((answer (jwt-tool "jwt-call-2" (jwt-arguments "not-a-token"))))
      (is (search "jwt: a JWT has 3 parts" answer) "a refusal is a line, not a failed call"))))

(deftest jwt-cell-the-command-answers-the-operator ()
  (with-cell-stop ((jwt-start))
    (is (search "HMAC with SHA-256" (nodecode-jwt:run-slash +classic+ nil)) "/jwt decodes what is typed after it")
    (is (search "/jwt <token>" (nodecode-jwt:run-slash "" nil)) "bare, it says how to use it")
    (is (search "jwt: a JWT has 3 parts" (nodecode-jwt:run-slash "not-a-token" nil)) "and a value it cannot read comes back with the reason")))

(deftest jwt-cell-a-token-is-required-at-the-boundary ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-jwt::jwt-arguments)
                             (nlk:make-json-object "token" ""))) "an empty token is refused by the declaration")
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-jwt::jwt-arguments)
                             (nlk:make-json-object "other" "a.b.c"))) "and so is a call that names no token"))

(deftest jwt-cell-the-section-bounds-the-reads ()
  (with-cell-stop ((jwt-start "max_token_chars" 40))
    (is (search (format nil "jwt: the token is ~:d characters long; jwt reads at most 40"
                        (length +classic+))
                (jwt-tool "jwt-call-3" (jwt-arguments +classic+))) "a token longer than the section allows is refused, not cut"))
  (with-cell-stop ((jwt-start "max_claims" 1))
    (let ((answer (jwt-tool "jwt-call-4"
                             (jwt-arguments (jwt-token "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
                                                        "{\"a\":1,\"b\":2,\"c\":3}")))))
      (is (search "... and 1 more header fields" answer) "fields past the bound are counted")
      (is (search "... and 2 more claims" answer) "and so are claims")))
  (with-cell-stop ((jwt-start "max_value_chars" 20))
    (let ((answer (jwt-tool "jwt-call-5"
                             (jwt-arguments (jwt-token "{\"alg\":\"HS256\"}"
                                                        "{\"scope\":\"read write admin everything else\"}")))))
      (is (search "characters)" answer) "a long value is cut, and the rest counted"))))
