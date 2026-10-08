;;;; cell-test.lisp --- the devin cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every exchange with Devin or Cascade a
;;;; stubbed dex:post answering bytes written out here by hand from the
;;;; .proto field numbers (the DV- helpers below spell a field as its number,
;;;; its wire type and its value, never through the cell's own encoder), every
;;;; stream an in-memory octet stream. The one socket a test opens is the
;;;; sign-in's own loopback callback, dialled on 127.0.0.1: nothing touches
;;;; the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "devin" "DEVIN-CELL-" :start nodecode-devin:start-cell)

(define-cell-lifecycle-tests "devin"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models)
  (:command "devin")
  (:running (is (nle::find-lane-by-name "devin" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "devin" nil)) "and taken back out"))
  (:refused ("base_url" 5)))

;;; --- bytes by hand ------------------------------------------------------------------

(defun dv-bytes (&rest parts)
  "Octets from PARTS: an integer is one byte, a string its UTF-8 bytes, a
vector its bytes."
  (coerce (loop for part in parts
                append (etypecase part
                         (integer (list part))
                         (string (coerce (sb-ext:string-to-octets part :external-format :utf-8) 'list))
                         (vector (coerce part 'list))))
          '(simple-array (unsigned-byte 8) (*))))

(defun dv-varint-bytes (value)
  "VALUE as a base-128 varint, a list of bytes."
  (loop for rest = value then (ash rest -7)
        collect (if (< rest 128) rest (logior #x80 (logand rest #x7f)))
        while (>= rest 128)))

(defun dv-len (field &rest parts)
  "Field FIELD, wire type 2, holding the bytes of PARTS (as DV-BYTES reads them)."
  (let ((body (apply #'dv-bytes parts)))
    (apply #'dv-bytes (append (dv-varint-bytes (logior (ash field 3) 2)) (dv-varint-bytes (length body))
                              (list body)))))

(defun dv-varint (field value)
  "Field FIELD, wire type 0, holding VALUE."
  (apply #'dv-bytes (append (dv-varint-bytes (ash field 3)) (dv-varint-bytes value))))

(defun dv-float (field value)
  "Field FIELD, wire type 5, holding the float VALUE little-endian."
  (let ((bits (ldb (byte 32 0) (sb-kernel:single-float-bits (coerce value 'single-float)))))
    (apply #'dv-bytes (append (dv-varint-bytes (logior (ash field 3) 5))
                              (loop for index below 4 collect (ldb (byte 8 (* 8 index)) bits))))))

(defun dv-frame (flags payload)
  "One Connect envelope by hand: FLAGS, the big-endian length, PAYLOAD."
  (let ((length (length payload)))
    (dv-bytes flags (ldb (byte 8 24) length) (ldb (byte 8 16) length) (ldb (byte 8 8) length)
              (ldb (byte 8 0) length) payload)))

(defparameter +dv-gzipped-hello+
  (dv-bytes #x1f #x8b #x08 #x00 #x00 #x00 #x00 #x00 #x02 #xff #x93 #x62 #xf5 #x48 #xcd #xc9
            #xc9 #x07 #x00 #xd2 #xf3 #xfc #x3f #x07 #x00 #x00 #x00)
  "gzip -9 of the GetChatMessageResponse {delta_text: \"Hello\"} (1a 05 48 65 6c 6c 6f),
deflate-compressed by zlib: a real compressed frame, not the cell's stored one.")

(defparameter +dv-gzipped-unavailable+
  (dv-bytes #x1f #x8b #x08 #x00 #x00 #x00 #x00 #x00 #x02 #xff #xab #x56 #x4a #x2d #x2a #xca
            #x2f #x52 #xb2 #xaa #x56 #x4a #xce #x4f #x49 #x55 #xb2 #x52 #x2a #xcd #x4b #x2c
            #x4b #xcc #xcc #x49 #x4c #xca #x49 #x55 #xd2 #x51 #xca #x4d #x2d #x2e #x4e #x4c
            #x07 #x89 #x97 #x14 #x55 #x2a #xe4 #x24 #x96 #xa4 #x16 #x29 #xd5 #xd6 #x02 #x00
            #x1f #x88 #x9e #x85 #x36 #x00 #x00 #x00)
  "zlib's gzip of {\"error\":{\"code\":\"unavailable\",\"message\":\"try later\"}}.")

(defun dv-response-frames ()
  "A scripted GetChatMessage answer: a thought and its signature, text (in a
gzipped frame), a tool call whose arguments arrive in two deltas, the stop
reason and usage, and the end-of-stream trailer."
  (dv-bytes
   ;; message_id 1, delta_thinking 9, delta_signature 10
   (dv-frame 0 (dv-bytes #x0a 2 "m1" #x4a 5 "think" #x52 3 "sig"))
   (dv-frame 1 +dv-gzipped-hello+)
   ;; delta_tool_calls 6: ChatToolCall {id 1, name 2, arguments_json 3}
   (dv-frame 0 (dv-bytes #x32 24 #x0a 6 "call_1" #x12 4 "eval" #x1a 8 "{\"form\":"))
   ;; the next delta names no id: it continues the active call
   (dv-frame 0 (dv-bytes #x32 12 #x1a 10 "\"(+ 1 2)\"}"))
   ;; stop_reason 5 = FUNCTION_CALL, usage 7: input 2, output 3, cache_read 5
   (dv-frame 0 (dv-bytes #x28 10 #x3a 6 #x10 12 #x18 7 #x28 3))
   (dv-frame 2 (dv-bytes "{}"))))

;;; --- the protobuf codec ------------------------------------------------------------------

(deftest devin-cell-encodes-the-cli-metadata-byte-for-byte ()
  (with-stubbed-fdefinition (nodecode-devin::devin-os () "linux")
    (is (equalp (dv-bytes #x0a 9 "devin-cli"          ; 1 ide_name
                          #x3a 9 "3000.11.3"          ; 7 ide_version
                          #xe2 #x01 6 "chisel"        ; 28 ide_type
                          #x62 6 "chisel"             ; 12 extension_name
                          #x12 9 "3000.11.3"          ; 2 extension_version
                          #x1a 23 "devin-session-token$tok" ; 3 api_key
                          #x22 2 "en"                 ; 4 locale
                          #x2a 5 "linux"              ; 5 os
                          #xaa #x01 1 "j")            ; 21 user_jwt
                (nodecode-devin::cli-metadata "tok" "j"))
        "the descriptor's order, not field-number order, and the session-token scheme in front")
    (is (equalp (dv-bytes #x0a 6 "chisel" #x3a 9 "0.0.0-dev" #x62 6 "chisel" #x12 9 "0.0.0-dev"
                          #x1a 21 "devin-session-token$k" #x22 2 "en" #x2a 5 "linux"
                          #xf2 #x01 5 3 4 6 7 8)     ; 30 supported_model_displays, packed
                (nodecode-devin::discovery-metadata "k"))
        "discovery asks with the dev-channel identity and the display slots packed")))

(deftest devin-cell-encodes-scalars-as-protobuf-does ()
  (is (equalp (dv-bytes #x08 1                         ; 1 num_completions
                        #x10 #xac #x02                 ; 2 max_tokens 300
                        #x18 #xc8 #x01                 ; 3 max_newlines 200
                        #x29 #x9a #x99 #x99 #x99 #x99 #x99 #xd9 #x3f ; 5 temperature 0.4
                        #x31 #x9a #x99 #x99 #x99 #x99 #x99 #xd9 #x3f ; 6 first_temperature
                        #x38 50                        ; 7 top_k
                        #x41 0 0 0 0 0 0 #xf0 #x3f     ; 8 top_p 1.0
                        #x4a 8 "<|user|>"              ; 9 stop_patterns
                        #x59 0 0 0 0 0 0 #xf0 #x3f)    ; 11 fim_eot_prob_threshold
              (nodecode-devin::encode-completion-configuration
               :max-tokens 300 :max-newlines 200 :temperature 0.4d0 :first-temperature 0.4d0
               :top-k 50 :top-p 1 :stop-patterns '("<|user|>") :fim-eot-threshold 1)))
  (is (equalp (dv-bytes #x0a 1 "b" #x10 2
                        #x32 14 #x0a 2 "c1" #x12 4 "eval" #x1a 2 "{}"
                        #x5a 1 "t" #x62 1 "s")
              (nodecode-devin::encode-chat-prompt
               :message-id "b" :source 2 :thinking "t" :signature "s"
               :tool-calls (list (nodecode-devin::encode-tool-call "c1" "eval" "{}"))))
      "an assistant prompt: its tool call nested, thinking 11 and signature 12")
  (is (equalp (dv-bytes #x0a 1 "m" #x10 4 #x1a 2 "ok" #x3a 2 "c1")
              (nodecode-devin::encode-chat-prompt :message-id "m" :source 4 :prompt "ok" :tool-call-id "c1"
                                                  :tool-result-is-error nil))
      "false and empty fields are left out")
  (let ((out (nodecode-devin::make-writer)))
    (nodecode-devin::put-varint out -1)
    (is (equalp (dv-bytes #xff #xff #xff #xff #xff #xff #xff #xff #xff #x01) out)
        "a negative int32 is its 64-bit two's complement, ten bytes")))

(deftest devin-cell-decodes-a-scripted-stream ()
  (let ((frames (nodecode-devin::decode-connect-frames (dv-response-frames))))
    (is (equal '(0 1 0 0 0 2) (mapcar #'car frames)) "six envelopes, one gzipped, one the trailer")
    (let ((first (nodecode-devin::decode-chat-response (cdr (first frames)))))
      (is (equal "m1" (getf first :message-id)))
      (is (equal "think" (getf first :delta-thinking)))
      (is (equal "sig" (getf first :delta-signature))))
    (is (equal "Hello" (getf (nodecode-devin::decode-chat-response
                              (nodecode-devin::gunzip (cdr (second frames))))
                             :delta-text))
        "a zlib-compressed frame inflates")
    (is (equal '((:id "call_1" :name "eval" :arguments "{\"form\":"))
               (getf (nodecode-devin::decode-chat-response (cdr (third frames))) :tool-calls)))
    (let ((last (nodecode-devin::decode-chat-response (cdr (fifth frames)))))
      (is (= 10 (getf last :stop-reason)))
      (is (equal '(:input 12 :output 7 :cache-write 0 :cache-read 3) (getf last :usage))))))

(deftest devin-cell-refuses-a-broken-envelope ()
  (is (equalp (dv-bytes 1 0 0 0 3 1 2 3) (nodecode-devin::connect-frame (dv-bytes 1 2 3) 1)))
  (is (signals-error nodecode-devin::proto-error
        (nodecode-devin::decode-connect-frames (dv-bytes 0 1 0 0 1)))
      "a length past the 16 MiB cap fails before anything is buffered")
  (is (signals-error nodecode-devin::proto-error
        (nodecode-devin::decode-connect-frames (dv-bytes 0 0 0 0 5 1 2)))
      "a stream that ends inside a frame is no frame")
  (is (signals-error nodecode-devin::proto-error
        (nodecode-devin::decode-fields (dv-bytes #x0b 1)))
      "a group wire type is refused")
  (let ((big (make-array 70000 :element-type '(unsigned-byte 8))))
    (dotimes (index 70000) (setf (aref big index) (mod (* index 7) 251)))
    (is (equalp big (nodecode-devin::gunzip (nodecode-devin::gzip big)))
        "the stored-block gzip member is one any inflater reads, past one block")
    (is (equalp (dv-bytes) (nodecode-devin::gunzip (nodecode-devin::gzip (dv-bytes)))))))

(deftest devin-cell-reads-the-connect-trailer ()
  (let ((trailer (nodecode-devin::trailer-error
                  "{\"error\":{\"code\":\"invalid_argument\",\"message\":\"bad\",\"details\":[{\"type\":\"t\",\"value\":\"v\"}]}}")))
    (is (equal "Devin stream error invalid_argument: bad [details: t: v]" (getf trailer :formatted))))
  (is (null (nodecode-devin::trailer-error "{}")) "a clean end carries no error")
  (is (null (nodecode-devin::trailer-error "not json"))))

;;; --- the catalog -----------------------------------------------------------------------

(deftest devin-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((devin-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "devin"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Devin" (nlk:json-value row :string "name")))
      (is (equal "https://server.codeium.com" (nlk:json-value row :string "api")))
      (is (gethash "swe-1-6" models) "the seeds are listed")
      (is (gethash "swe-1-6-fast" models))
      (is (equal "devin" (nle::configured-provider-lane "devin")) "the cell's own lane drives it")
      (is (equal "https://server.codeium.com" (nle::lane-endpoint "devin" "devin"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "devin")))))

(deftest devin-cell-base-follows-the-section ()
  (with-cell-stop ((devin-start "base_url" "https://relay.example"))
    (is (equal "https://relay.example" (nlk:json-value (nle::models-catalog-table) :string "devin" "api")))))

;;; --- the credential --------------------------------------------------------------------

(defun dv-jwt (claims)
  "An unsigned JWT carrying CLAIMS (a JSON text)."
  (format nil "eyJhbGciOiJub25lIn0.~a.sig"
          (string-right-trim "." (cl-base64:usb8-array-to-base64-string
                                  (sb-ext:string-to-octets claims :external-format :utf-8) :uri t))))

(defun dv-signed-in (&key (token "tok-devin") (expires 4000000000))
  "An auth.json text holding a Devin sign-in."
  (shasht:write-json
   (nlk:json-object "oauth_tokens"
                    (nlk:json-object "devin" (nlk:json-object "access_token" token "refresh_token" token
                                                              "expires_at" expires)))
   nil))

(deftest devin-cell-answers-the-kept-sign-in ()
  (with-cell-stop ((devin-start))
    (with-temp-auth (auth (dv-signed-in))
      (let ((credential (nle::resolve-provider-credential "devin" :auth-path auth :probe t)))
        (is (equal "tok-devin" (nle:credential-key credential)))
        (is (eq :oauth (nle:credential-source credential)))))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "DEVIN_API_KEY") "dk-env"))
        (let ((credential (nle::resolve-provider-credential "devin" :auth-path auth :probe t)))
          (is (equal "dk-env" (nle:credential-key credential)) "DEVIN_API_KEY answers without a sign-in")
          (is (eq :env (nle:credential-source credential))))))
    (with-temp-auth (auth "{\"api_keys\":{\"devin\":{\"provider\":\"devin\",\"key\":\"dk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "DEVIN_API_KEY") "dk-env"))
        (is (equal "dk-saved" (nle:credential-key (nle::resolve-provider-credential "devin" :auth-path auth :probe t)))
            "a key /connect saved outranks the variable")))))

(deftest devin-cell-never-sends-another-familys-key ()
  (with-cell-stop ((devin-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (member name '("OPENAI_API_KEY" "ANTHROPIC_API_KEY" "GOOGLE_API_KEY")
                                              :test #'equal)
                                      "sk-other"))
        (let ((credential (nle::resolve-provider-credential "devin" :auth-path auth :probe t)))
          (is (not (equal "sk-other" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))
        (is (eq :public (nle:credential-source
                         (nle::resolve-provider-credential "devin" :auth-path auth :endpoint "https://server.codeium.com")))
            "nor on a round")))))

(deftest devin-cell-asks-again-for-an-expired-sign-in ()
  (with-cell-stop ((devin-start))
    (with-temp-auth (auth (dv-signed-in :expires (- (nodecode-devin::unix-now) 10)))
      (with-stubbed-fdefinition (dex:post (url &rest args) (error "no exchange may happen: ~a" url))
        (is (eq :public (nle:credential-source (nle::resolve-provider-credential "devin" :auth-path auth :probe t)))
            "a probe sets the expired token aside, and says nothing")
        (is (null (cell-notice "nodecode-devin")))
        (is (signals-error nle::credential-error
              (nle::resolve-provider-credential "devin" :auth-path auth :endpoint "https://server.codeium.com"))
            "a round refuses it: omp has no refresh for Devin")
        (is (search "/devin login" (or (second (cell-notice "nodecode-devin")) ""))
            "and the notice asking for a sign-in stands"))
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "DEVIN_API_KEY") "dk-env"))
        (is (equal "dk-env" (nle:credential-key (nle::resolve-provider-credential
                                                 "devin" :auth-path auth :endpoint "https://server.codeium.com")))
            "DEVIN_API_KEY still answers")))))

;;; --- the sign-in ------------------------------------------------------------------------

(defun dv-entry (auth)
  "The sign-in AUTH keeps, or NIL."
  (nlk:json-value (ignore-errors (nle::read-auth-file auth)) :object "oauth_tokens" "devin"))

(defun dv-said (fragment)
  "Whether a notice said lately carries FRAGMENT."
  (some (lambda (entry) (search fragment (first entry))) (nlk:notice-log :limit 50)))

(defmacro with-dv-sign-in ((posts token) &body body)
  "BODY with Devin's token endpoint stubbed to answer TOKEN; POSTS collects
(URL HEADERS CONTENT), newest first. Loopback exchanges pass through."
  `(let ((,posts '()))
     (with-stubbed-fdefinitions
         ((dex:post (url &rest args)
           (push (list url (getf args :headers) (getf args :content)) ,posts)
           (if (equal url "https://api.devin.ai/auth/cli/token")
               (values (format nil "{\"token\":\"~a\"}" ,token) 200)
               (values "{}" 404)))
          (dex:get (url &rest args)
           (if (search "127.0.0.1" url) (apply original url args) (values "{}" 404))))
       ,@body)))

(defun dv-login (auth)
  "Run /devin login against AUTH: (values ANSWER REDIRECT-URI STATE CHALLENGE)."
  (let ((answer (let ((nle::*auth-file-path* auth))
                  (cell-entry "nodecode-devin" "devin" "login"))))
    (values answer
            (ppcre:register-groups-bind (uri) ("listens at (http://\\S+)" answer) uri)
            (ppcre:register-groups-bind (state) ("[?&]state=([0-9a-f-]+)" answer) state)
            (ppcre:register-groups-bind (challenge) ("[?&]code_challenge=([A-Za-z0-9_-]+)" answer) challenge))))

(deftest devin-cell-signs-in-through-the-loopback-callback ()
  (with-cell-stop ((devin-start))
    (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"kept\"}}}")
      (let ((token (dv-jwt "{\"sub\":\"u1\",\"exp\":4102444800}")))
        (with-dv-sign-in (posts token)
          (multiple-value-bind (answer redirect state challenge) (dv-login auth)
            (is (search "https://app.devin.ai/auth/cli/continue?response_type=code&" answer))
            (is (search "&code_challenge_method=S256&" answer))
            (is (search "&prompt=select_account" answer) "the rule's own parameter, last")
            (is (null (search "client_id" answer)) "the rule names no client")
            (is (ppcre:scan "^http://127\\.0\\.0\\.1:\\d+/callback$" (or redirect "")) redirect)
            (is (ppcre:scan "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$" (or state ""))
                "the state is a UUID")
            (multiple-value-bind (page status) (dex:get (format nil "~a?code=dv-code&state=~a" redirect state))
              (is (eql 200 status))
              (is (search "Signed in" page)))
            (is (await (:timeout 10) (dv-entry auth)) "the token is kept")
            (let ((exchange (find "https://api.devin.ai/auth/cli/token" posts :key #'first :test #'equal)))
              (is-present exchange "the code was exchanged"
                (let ((body (nlk:decode-json (third exchange))))
                  (is (uiop:string-prefix-p "{\"code\":\"dv-code\",\"code_verifier\":\"" (third exchange))
                      "the rule's two params, as JSON, in its order")
                  (is (equal "dv-code" (nlk:json-value body :string "code")))
                  (is (equal challenge (nodecode-devin::base64url
                                        (nodecode-devin::sha256 (nlk:json-value body :string "code_verifier"))))
                      "the verifier is the one the challenge was made from (S256)"))
                (is (equal "application/json" (cdr (assoc "Accept" (second exchange) :test #'string-equal))))
                (is (equal "application/json" (cdr (assoc "Content-Type" (second exchange) :test #'string-equal))))))
            (let ((entry (dv-entry auth)))
              (is (equal token (nlk:json-value entry :string "access_token")))
              (is (equal token (nlk:json-value entry :string "refresh_token")) "the token is its own refresh token")
              (is (= (- 4102444800 300) (nlk:json-value entry :integer "expires_at")) "its exp less five minutes")
              (is (equal "https://api.devin.ai" (nlk:json-value entry :string "api_endpoint")))
              (is (equal "https://app.devin.ai" (nlk:json-value entry :string "enterprise_url"))))
            (is (equal "kept" (nlk:json-value (nle::read-auth-file auth) :string "api_keys" "other" "key"))
                "every other field of auth.json is kept")
            (is (await (:timeout 10) (dv-said "devin: signed in")) "the outcome is said once")
            (is (null (cell-notice "nodecode-devin")) "and does not stand")))))))

(deftest devin-cell-takes-a-pasted-code ()
  (with-cell-stop ((devin-start))
    (with-temp-auth (auth "{}")
      (with-dv-sign-in (posts "opaque-token")
        (multiple-value-bind (answer redirect state) (dv-login auth)
          (declare (ignore answer redirect))
          (is (search "not for the sign-in"
                      (cell-entry "nodecode-devin" "devin" "code http://127.0.0.1:1/callback?code=x&state=other")))
          (is (search "code received" (cell-entry "nodecode-devin" "devin" (format nil "code dv-pasted#~a" state))))
          (is (await (:timeout 10) (dv-entry auth)))
          (is (equal "dv-pasted" (nlk:json-value (nlk:decode-json (third (first posts))) :string "code"))
              "the code without its #state")
          (let ((expires (nlk:json-value (dv-entry auth) :integer "expires_at")))
            (is (< (abs (- expires (+ (nodecode-devin::unix-now) (* 365 24 60 60)))) 60)
                "a token naming no exp is kept for a year")))))))

(deftest devin-cell-reads-a-callback-address ()
  (is (equal '("c" "s" nil) (multiple-value-list (nodecode-devin::callback-answer "/callback?code=c&state=s"))))
  (is (equal '("a" "s" nil) (multiple-value-list (nodecode-devin::callback-answer "/callback?authCode=a&state=s")))
      "authCode is a code too")
  (is (equal '(nil nil "denied by user")
             (multiple-value-list (nodecode-devin::callback-answer "/callback?error=access_denied&error_description=denied%20by%20user")))))

(deftest devin-cell-logout-forgets-the-sign-in ()
  (with-cell-stop ((devin-start))
    (with-temp-auth (auth (dv-signed-in))
      (let ((nle::*auth-file-path* auth))
        (is (search "signed in until" (cell-entry "nodecode-devin" "devin" "status")))
        (is (search "signed out" (cell-entry "nodecode-devin" "devin" "logout")))
        (is (null (dv-entry auth)))
        (is (search "not signed in" (cell-entry "nodecode-devin" "devin" "status")))))))

;;; --- a round -------------------------------------------------------------------------------

(defun dv-jwt-answer (&optional (url ""))
  "A GetUserJwtResponse: user_jwt 1, custom_api_server_url 2."
  (dv-bytes (dv-len 1 "user-jwt-1") (if (plusp (length url)) (dv-len 2 url) (dv-bytes))))

(defun dv-lane-round (context)
  "One round of CONTEXT on the lane its frozen config names, the way the turn loop runs it."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defmacro with-dv-round ((values posts &key (model "swe-1-6") effort (frames '(dv-response-frames))
                                            (jwt '(dv-jwt-answer "https://chat.example/"))
                                            (answer '(lambda (url body) (declare (ignore url body)) nil))
                                            (context '(user-context "add one and two")))
                         &body forms)
  "FORMS with the cell started and signed in, and one round of MODEL at EFFORT
answered FRAMES: VALUES the lane's four values, POSTS each (URL HEADERS
BODY) dex:post saw, newest first. ANSWER, given a URL and the body, may
answer (values BODY STATUS) instead."
  `(with-cell-stop ((devin-start))
     (with-temp-auth (auth (dv-signed-in))
       (let ((nle::*provider* "devin") (nle::*model* ,model) (nle::*api-key* nil)
             (nle::*reasoning-effort* ,effort) (nle::*endpoint* nil)
             (nle::*auth-file-path* auth) (,posts '()) (,values nil))
         (declare (ignorable ,posts ,values))
         (with-stubbed-fdefinition
             (dex:post (url &rest args)
              (push (list url (getf args :headers) (getf args :content)) ,posts)
              (multiple-value-bind (body status) (funcall ,answer url (getf args :content))
                (cond (status (values body status (make-hash-table :test #'equal)))
                      ((search "GetUserJwt" url) (values ,jwt 200 (make-hash-table :test #'equal)))
                      ((search "GetChatMessage" url)
                       (values (flexi-streams:make-in-memory-input-stream ,frames) 200
                               (make-hash-table :test #'equal)))
                      (t (values (dv-bytes) 404 (make-hash-table :test #'equal))))))
           (setf ,values (multiple-value-list (dv-lane-round ,context))))
         ,@forms))))

(defun dv-post (posts fragment)
  "The newest of POSTS whose URL carries FRAGMENT."
  (find-if (lambda (post) (search fragment (first post))) posts))

(defun dv-chat-request (posts)
  "The GetChatMessageRequest the newest chat POST carried, unframed and inflated, decoded."
  (let* ((frame (first (nodecode-devin::decode-connect-frames (third (dv-post posts "GetChatMessage"))))))
    (values (nodecode-devin::decode-fields (nodecode-devin::gunzip (cdr frame))) (car frame))))

(deftest devin-cell-runs-a-round-through-cascade ()
  (with-dv-round (values posts)
    (destructuring-bind (message usage finish request) values
      (let ((auth-post (dv-post posts "GetUserJwt")))
        (is (equal "https://server.codeium.com/exa.auth_pb.AuthService/GetUserJwt" (first auth-post)))
        (is (equal "application/proto" (cdr (assoc "content-type" (second auth-post) :test #'string-equal))))
        (is (equal "devin-session-token$tok-devin"
                   (nodecode-devin::pb-text (nodecode-devin::pb-sub (nodecode-devin::decode-fields (third auth-post)) 1) 3))
            "the session token rides inside the Metadata"))
      (let ((chat (dv-post posts "GetChatMessage")))
        (is (equal "https://chat.example/exa.api_server_pb.ApiServerService/GetChatMessage" (first chat))
            "the account's own chat host GetUserJwt named")
        (is (equal "application/connect+proto" (cdr (assoc "content-type" (second chat) :test #'string-equal))))
        (is (equal "gzip" (cdr (assoc "connect-content-encoding" (second chat) :test #'string-equal))))
        (is (equal "connect-go/1.18.1 (go1.26.3)" (cdr (assoc "user-agent" (second chat) :test #'string-equal)))))
      (multiple-value-bind (fields flags) (dv-chat-request posts)
        (is (= 1 flags) "one gzipped frame")
        (is (equal "swe-1-6" (nodecode-devin::pb-text fields 21)) "chat_model_uid")
        (is (= 5 (nodecode-devin::pb-uint fields 7)) "request_type CASCADE")
        (is (= 1 (nodecode-devin::pb-uint fields 20)) "planner_mode DEFAULT")
        (is (not (nodecode-devin::pb-flag fields 11)) "the seed takes parallel tool calls")
        (let ((metadata (nodecode-devin::pb-sub fields 1)))
          (is (equal "user-jwt-1" (nodecode-devin::pb-text metadata 21)))
          (is (equal "devin-session-token$tok-devin" (nodecode-devin::pb-text metadata 3)))
          (is (equal "chisel" (nodecode-devin::pb-text metadata 28))))
        (let ((prompts (nodecode-devin::pb-subs fields 3)))
          (is (search "add one and two" (nodecode-devin::pb-text (car (last prompts)) 3)))
          (is (= 1 (nodecode-devin::pb-uint (car (last prompts)) 2)) "the user's turn is USER"))
        (is (equal "auto" (nodecode-devin::pb-text (nodecode-devin::pb-sub fields 12) 1)) "tool_choice auto")
        (is (= 1 (nodecode-devin::pb-uint (nodecode-devin::pb-sub fields 13) 1)) "the system prompt cached, ephemeral")
        (let ((configuration (nodecode-devin::pb-sub fields 8)))
          (is (= 200 (nodecode-devin::pb-uint configuration 3)))
          (is (= 0.4d0 (nodecode-devin::pb-double-value configuration 5)))
          (is (equal '("<|user|>" "<|bot|>" "<|context_request|>" "<|endoftext|>" "<|end_of_turn|>")
                     (nodecode-devin::pb-texts configuration 9))))
        (is (plusp (length (nodecode-devin::pb-text fields 16))) "a cascade id")
        (is (plusp (length (nodecode-devin::pb-text fields 22))) "an execution id"))
      (is (equal "Hello" (nlk:json-value message :string "content")))
      (is (equal "think" (nlk:json-value message :string "reasoning_content")))
      (is (equal "sig" (nlk:json-value message :string "reasoning_signature")))
      (is (equal '("swe-1-6" "m1") (multiple-value-list (nodecode-devin::message-author message)))
          "the model that wrote it and the id Cascade gave it are kept beside the message")
      (is (subsetp (alexandria:hash-table-keys message)
                   '("role" "content" "reasoning_content" "reasoning_signature" "tool_calls") :test #'equal)
          "and no field of the cell's own rides it")
      (let ((call (aref (nlk:json-value message :array "tool_calls") 0)))
        (is (equal "call_1" (nlk:json-value call :string "id")))
        (is (equal "eval" (nlk:json-value call :string "function" "name")))
        (is (equal "{\"form\":\"(+ 1 2)\"}" (nlk:json-value call :string "function" "arguments"))
            "the second delta, which named no id, continued the call"))
      (is (equal "tool_calls" finish))
      (is (= 12 (nle::provider-usage-input-tokens usage)))
      (is (= 7 (nle::provider-usage-output-tokens usage)))
      (is (= 3 (nle::provider-usage-cached-input-tokens usage)))
      (is (= 22 (nle::provider-usage-total-tokens usage)))
      (is (typep request 'nlk:octets))
      (is (not (search (sb-ext:string-to-octets "tok-devin") request)) "the kept request carries no key")
      (is (not (search (sb-ext:string-to-octets "user-jwt-1") request)) "nor the JWT"))))

(deftest devin-cell-replays-its-own-thinking-and-demotes-anothers ()
  (let ((mine (nlk:json-object "role" "assistant" "content" "one" "reasoning_content" "mine"
                               "reasoning_signature" "sig-1")))
    (with-dv-round (values posts :context (progn
                                            ;; a round of swe-1-6 wrote it, in this process
                                            (nodecode-devin::remember-author mine "swe-1-6" "msg-9")
                                            (compiled-context
                                             (list (nle::message "user" "first")
                                                   mine
                                                   (nle::message "user" "second")
                                                   (nlk:json-object "role" "assistant" "content" "two"
                                                                    "reasoning_content" "theirs")
                                                   (nle::message "user" "third")))))
      (let* ((prompts (nodecode-devin::pb-subs (dv-chat-request posts) 3))
             (native (find "msg-9" prompts :key (lambda (prompt) (nodecode-devin::pb-text prompt 1)) :test #'equal))
             (foreign (find "two" prompts :key (lambda (prompt) (nodecode-devin::pb-text prompt 3))
                                          :test (lambda (text prompt) (search text prompt)))))
        (is-present native "this model's answer replays under the id Cascade gave it"
          (is (equal "mine" (nodecode-devin::pb-text native 11)))
          (is (equal "sig-1" (nodecode-devin::pb-text native 12)))
          (is (= 2 (nodecode-devin::pb-uint native 2)) "the assistant's turn is SYSTEM"))
        (is-present foreign "another model's answer"
          (is (equal (format nil "theirs~%two") (nodecode-devin::pb-text foreign 3)) "its thinking demoted to text")
          (is (equal "" (nodecode-devin::pb-text foreign 11)))
          (is (uiop:string-prefix-p "bot-" (nodecode-devin::pb-text foreign 1))))))))

(deftest devin-cell-adds-nothing-another-lane-would-replay ()
  (with-dv-round (values posts)
    (let ((devin-message (first values))
          (body nil))
      (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-test") (nle::*api-key* "k")
            (nle::*reasoning-effort* nil) (nle::*endpoint* nil))
        (with-stubbed-fdefinition
            (dex:post (asked &rest args)
             (setf body (nlk:decode-json (getf args :content)))
             (values (make-truncated-sse-stream
                      "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                      "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                      "[DONE]")
                     200))
          (nle::call-provider-streaming
           (compiled-context (list (nle::message "user" "add one and two")
                                   devin-message
                                   (nle::message "user" "next"))))))
      (let ((replayed (find "assistant" (nlk:json-value body :array "messages")
                            :key (lambda (message) (nlk:json-value message :string "role")) :test #'equal)))
        (is-present replayed "the devin round's message rides the chat lane's request"
          (is (equal "Hello" (nlk:json-value replayed :string "content")))
          (is (subsetp (alexandria:hash-table-keys replayed)
                       '("role" "content" "reasoning_content" "reasoning_signature" "tool_calls")
                       :test #'equal)
              (format nil "no key the cell added reaches another provider: ~s"
                      (alexandria:hash-table-keys replayed))))))))

(deftest devin-cell-retries-auth-with-the-bare-key ()
  (let ((auth-calls 0))
    (with-dv-round (values posts :answer (lambda (url body)
                                           (declare (ignore body))
                                           (when (and (search "GetUserJwt" url) (= 1 (incf auth-calls)))
                                             (values (dv-bytes "{\"code\":\"unauthenticated\"}") 401))))
      (let ((auths (reverse (remove-if-not (lambda (post) (search "GetUserJwt" (first post))) posts))))
        (is (= 2 (length auths)))
        (is (equal "tok-devin" (nodecode-devin::pb-text (nodecode-devin::pb-sub (nodecode-devin::decode-fields (third (second auths))) 1) 3))
            "the second ask carries the key as it was saved")
        (is (equal "tok-devin" (nodecode-devin::pb-text (nodecode-devin::pb-sub (dv-chat-request posts) 1) 3))
            "and the chat request rides the key that worked")
        (is (equal "Hello" (nlk:json-value (first values) :string "content")))))))

(deftest devin-cell-says-a-trailer-error ()
  (let ((condition nil))
    (handler-case
        (with-dv-round (values posts :frames (dv-bytes (dv-frame 3 +dv-gzipped-unavailable+))))
      (nle::provider-error (e) (setf condition e)))
    (is-present condition "the gzipped trailer's error fails the round"
      (is (eql 503 (nle::provider-error-status condition)) "unavailable is Connect's 503: retried")
      (is (search "Devin stream error unavailable: try later" (nle::provider-error-detail condition))))))

(deftest devin-cell-says-an-http-refusal ()
  (let ((condition nil))
    (handler-case
        (with-dv-round (values posts :answer (lambda (url body)
                                               (declare (ignore body))
                                               (when (search "GetChatMessage" url)
                                                 (values (dv-bytes "{\"error\":{\"message\":\"model not allowed\"}}") 403)))))
      (nle::provider-error (e) (setf condition e)))
    (is-present condition "a refused chat request fails the round"
      (is (eql 403 (nle::provider-error-status condition)))
      (is (eq :request (nle::provider-error-scope condition)))
      (is (equal "Devin API error 403: model not allowed" (nle::provider-error-detail condition))))))

(deftest devin-cell-says-it-is-not-signed-in ()
  (with-cell-stop ((devin-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*provider* "devin") (nle::*model* "swe-1-6") (nle::*api-key* nil) (nle::*endpoint* nil)
            (nle::*auth-file-path* auth))
        (with-stubbed-fdefinition (dex:post (url &rest args) (error "no exchange may happen: ~a" url))
          (let ((condition (signals-error nle::provider-config-error (dv-lane-round (user-context)))))
            (is (search "/devin login" (nle::provider-error-detail condition)))))))))

;;; --- the roster ------------------------------------------------------------------------

(defun dv-config (uid label &key family effort default thinking router (display 0) disabled input output)
  "A ClientModelConfig by hand: label 1, model_uid 22, disabled 4, max_tokens 18,
model_info 23 {model_features 6 {supports_tool_calls 12, supports_thinking 15},
display_option 22}, model_family_metadata 30 {label 1, entries 2 {key 1, value
2 {name 2}}}, is_default_model_in_family 31, model_dimensions 32 {label 1,
value 2, denominator 3, kind 6}."
  (dv-len 1
          (dv-len 1 label)
          (dv-len 22 uid)
          (if disabled (dv-varint 4 1) (dv-bytes))
          (dv-varint 18 300000)
          (dv-len 23
                  (if router (dv-bytes) (dv-len 6 (dv-varint 12 1) (if thinking (dv-varint 15 1) (dv-bytes))))
                  (if (plusp display) (dv-varint 22 display) (dv-bytes)))
          (if family
              (dv-len 30 (dv-len 1 family) (dv-len 2 (dv-len 1 "Effort") (dv-len 2 (dv-len 2 effort))))
              (dv-bytes))
          (if default (dv-varint 31 1) (dv-bytes))
          (if input (dv-len 32 (dv-len 1 "Input") (dv-float 2 input) (dv-len 3 "1M tokens") (dv-varint 6 1)) (dv-bytes))
          (if output (dv-len 32 (dv-len 1 "Output") (dv-float 2 output) (dv-len 3 "1M tokens") (dv-varint 6 1)) (dv-bytes))))

(defun dv-roster ()
  "A GetCliModelConfigsResponse: one family over two efforts, a plain priced
model, a router, an internal slot and a disabled model."
  (dv-bytes (dv-config "claude-opus-5-low" "Claude Opus 5 Low" :family "Claude Opus 5" :effort "Low" :thinking t)
            (dv-config "claude-opus-5-high" "Claude Opus 5 High" :family "Claude Opus 5" :effort "High"
                                                                  :thinking t :default t)
            (dv-config "swe-1-7" "SWE-1.7" :input 0.5 :output 2.5)
            (dv-config "adaptive" "Adaptive" :router t :display 3)
            (dv-config "quick-review" "Quick Review" :display 4)
            (dv-config "retired" "Retired" :disabled t)))

(defun dv-roster-answer (url body)
  "A stubbed Cascade answering the roster to GetCliModelConfigs, an assignment to AssignModel."
  (declare (ignore body))
  (cond ((search "GetCliModelConfigs" url) (values (dv-roster) 200))
        ((search "AssignModel" url) (values (dv-len 1 (dv-len 1 "assign-jwt") (dv-len 2 "claude-opus-5-high")) 200))))

(deftest devin-cell-lists-the-accounts-roster ()
  (with-cell-stop ((devin-start))
    (with-temp-auth (auth (dv-signed-in))
      (let ((nle::*auth-file-path* auth) (posts '()))
        (with-stubbed-fdefinition (dex:post (url &rest args)
                                   (push (list url (getf args :content)) posts)
                                   (multiple-value-bind (body status) (dv-roster-answer url nil)
                                     (values body (or status 404) (make-hash-table :test #'equal))))
          (multiple-value-bind (rows error) (nle::list-provider-models "devin")
            (is (null error))
            (is (equal '("adaptive" "claude-opus-5" "swe-1-7") (mapcar (lambda (row) (getf row :id)) rows))
                "the effort family is one model; internal and disabled configs are not listed")
            (is (equal "Claude Opus 5" (getf (second rows) :display)))))
        (let ((request (nodecode-devin::decode-fields (second (first posts)))))
          (is (equal "https://server.codeium.com/exa.api_server_pb.ApiServerService/GetCliModelConfigs" (first (first posts))))
          (is (equal "chisel" (nodecode-devin::pb-text (nodecode-devin::pb-sub request 1) 1)) "the discovery identity"))
        (let* ((models (nlk:json-value (nle::models-catalog-table) :object "devin" "models"))
               (opus (gethash "claude-opus-5" models))
               (swe (gethash "swe-1-7" models)))
          (is (null (gethash "swe-1-6" models)) "the roster replaces the seeds")
          (is-present opus "the family is in the catalog"
            (is (equal '("low" "high") (nle::catalog-model-efforts opus)) "its effort ladder")
            (is (= 300000 (nle::catalog-model-context opus))))
          (is-present swe "the priced model"
            (is (equal '(0.5d0 2.5d0 0 0) (nle::catalog-model-price swe)) "per million, from its dimensions")))
        (let ((opus (nodecode-devin::model-spec "claude-opus-5")))
          (is (equal "claude-opus-5-high" (nodecode-devin::wire-uid opus "claude-opus-5" "high")))
          (is (equal "claude-opus-5-low" (nodecode-devin::wire-uid opus "claude-opus-5" "medium"))
              "an effort the family lacks is the strongest below it")
          (is (equal "claude-opus-5-high" (nodecode-devin::wire-uid opus "claude-opus-5" nil))
              "no effort is the family's default uid"))
        (is (getf (nodecode-devin::model-spec "adaptive") :router) "the router is marked")))))

(deftest devin-cell-checks-the-key-connect-would-save ()
  (with-cell-stop ((devin-start))
    ;; a sign-in is kept: the check must ask with the key under check, never this one
    (with-temp-auth (auth (dv-signed-in :token "stored-token"))
      (let ((nle::*auth-file-path* auth) (keys '()))
        (with-stubbed-fdefinition
            (dex:post (url &rest args)
             (let ((key (nodecode-devin::pb-text
                         (nodecode-devin::pb-sub (nodecode-devin::decode-fields (getf args :content)) 1) 3)))
               (push key keys)
               (if (member key '("good" "devin-session-token$good") :test #'equal)
                   (values (dv-roster) 200 (make-hash-table :test #'equal))
                   (error 'dex:http-request-unauthorized
                          :status 401 :uri url :method :post :headers nil
                          :body (sb-ext:string-to-octets
                                 "{\"code\":\"unauthenticated\",\"message\":\"invalid api key\"}"
                                 :external-format :utf-8)))))
          (multiple-value-bind (rows reason) (nle::list-provider-models "devin" :key "bad"
                                                                                :base "https://server.codeium.com")
            (is (null rows))
            (is (equal "HTTP 401 unauthenticated" reason) reason))
          (is (member "devin-session-token$bad" keys :test #'equal) "Devin was asked with the key under check")
          (is (notany (lambda (key) (search "stored-token" key)) keys) "never with the kept sign-in")
          (is (null nodecode-devin::*discovered*) "a checked key's roster is not kept")
          (with-saved-globals ((nle::*provider-models-cache-path*
                                (format nil "/tmp/devin-cell-models-~a.json" (nodecode-devin::uuid))))
            (is (eq :refused (nle::provider-key-check "devin" "bad")))
            (is (eq :works (nle::provider-key-check "devin" "good")))))))))

(deftest devin-cell-routes-a-family-from-omps-table-before-discovery ()
  (let ((spec (nodecode-devin::model-spec "gpt-5-5")))
    (is (equal "gpt-5-5-xhigh" (nodecode-devin::wire-uid spec "gpt-5-5" "xhigh")))
    (is (equal "gpt-5-5-none" (nodecode-devin::wire-uid spec "gpt-5-5" nil)) "off routes to the none uid")
    (is (equal "MODEL_PRIVATE_11" (nodecode-devin::wire-uid (nodecode-devin::model-spec "claude-haiku-4-5")
                                                            "claude-haiku-4-5" nil))
        "a rename family sends its one uid")))

(deftest devin-cell-assigns-a-router-model ()
  (with-saved-globals ((nodecode-devin::*discovered* nil) (nodecode-devin::*discovery-tried* nil))
    (with-dv-round (values posts :model "adaptive" :answer #'dv-roster-answer)
      (let ((assign (nodecode-devin::decode-fields (third (dv-post posts "AssignModel")))))
        (is (equal "adaptive" (nodecode-devin::pb-text assign 2)) "the router uid")
        (is (equal (nodecode-devin::pb-text (dv-chat-request posts) 16) (nodecode-devin::pb-text assign 3))
            "assignment and chat share the cascade id"))
      (let ((request (dv-chat-request posts)))
        (is (equal "claude-opus-5-high" (nodecode-devin::pb-text request 21)) "the assigned uid, never the router's")
        (is (equal "assign-jwt" (nodecode-devin::pb-text request 26)) "with the assignment JWT"))
      (is (dv-post posts "GetCliModelConfigs") "a model the seeds do not name sent discovery first"))))

(deftest devin-cell-leaves-other-providers-alone ()
  (with-cell-stop ((devin-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "swe-1-6") (nle::*api-key* "k")
          (nle::*endpoint* nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (is (not (search "codeium" asked)))
           (setf body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "swe-1-6" (nlk:json-value body :string "model"))))))
