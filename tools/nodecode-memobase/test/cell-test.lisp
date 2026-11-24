;;;; cell-test.lisp --- lifecycle, the two tools, the write, the refusals, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Memobase: the config
;;;; surface (a section naming no server refuses the start), both tools
;;;; through the real dispatch seam against a stubbed server, every member
;;;; the recall request carries, the write body, the user id the original
;;;; derived, and the refusals.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "memobase"
  (:config "base_url" +base+ "api_key" +key+)
  (:help :memobase)
  (:command "memobase")
  (:running
   (is (nle::find-tool "memobase_search") "memobase_search is registered")
   (is (nle::find-tool "memobase_profile") "memobase_profile is registered")
   (is (equal +base+ (nodecode-memobase::setting :base-url)) "the url the section carries")
   (is (equal +user+ (nodecode-memobase::user-id)) "and the id its user_name derives"))
  (:stopped
   (is (null (nle::find-tool "memobase_search")) "stop unregisters the search tool")
   (is (null (nle::find-tool "memobase_profile")) "and the profile tool"))
  (:refused
   ("base_url" "")
   ("base_url" +base+ "max_tokens" 5)
   ("base_url" +base+ "time_range_days" 0)
   ("base_url" +base+ "timeout_seconds" 0))
  (:idle nodecode-memobase:memobase-error
         (memobase:search "anything")
         (memobase:profile)))

(deftest memobase-cell-without-a-server-refuses ()
  ;; The one setting no call can work without: a section naming no base url
  ;; is refused at start, as the original offered no provider without
  ;; MEMOBASE_BASE_URL.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal (memobase-start "base_url" ""))))
      (is (cl:search "no Memobase server" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "memobase_search")) "and installs nothing"))))

;;; --- recall ----------------------------------------------------------------------

(deftest memobase-cell-the-search-tool-recalls-through-the-context-endpoint (with-memobase ()
  (let ((text (memobase-call-tool "memobase_search" "{\"query\":\"what did we decide about the build\"}")))
    (is (cl:search "drinks tea, no coffee" text) "the profile Memobase assembled")
    (is (cl:search "the build is green on main" text) "and the event it read"))
  (let ((call (first *calls*)))
    (is (= 1 (length *calls*)) "one request, not one a line")
    (is (eq :get (getf call :method)))
    (is (equal (format nil "~a/users/context/~a" +base+ +user+) (path-of call)) "to the recall endpoint, for the id the section derives")
    (is (equal "2000" (param call "max_token_size")) "the section's token budget")
    (is (equal "180" (param call "time_range_in_days")) "and the window's depth")
    (is (equal "true" (param call "fill_window_with_events")) "and the window asked for")
    (is (cl:search "what did we decide about the build" (param call "chats_str")) "the turn rides as chats_str")
    (is (equal "user" (nlk:json-value (aref (nlk:decode-json (param call "chats_str")) 0)
                                      :string "role")) "as one user message")
    (is (equal (format nil "Bearer ~a" +key+) (header call "authorization")) "with the section's token"))))

(deftest memobase-cell-a-call-s-own-length-narrows-the-answer (with-memobase ()
  (let ((text (memobase-call-tool "memobase_search" "{\"query\":\"build\",\"max_length\":12}")))
    (is (cl:search "more characters" text) "a length the call names clips the answer")
    (is (< (length text) 120) "and keeps it short"))))

(deftest memobase-cell-the-section-s-members-ride-a-request (with-memobase ("max_tokens" 500 "time_range_days" 7 "fill_window_events" nil "user_name" "alice")
  (memobase-call-tool "memobase_search" "{\"query\":\"build\"}")
  (let ((call (first *calls*))
        (alice (nodecode-memobase::uuid5 nodecode-memobase::+uuid-namespace-dns+ "alicememobase_client")))
    (is (equal "500" (param call "max_token_size")))
    (is (equal "7" (param call "time_range_in_days")))
    (is (null (param call "fill_window_with_events")) "a window switched off is not asked for")
    (is (equal (format nil "~a/users/context/~a" +base+ alice) (path-of call)) "and another user_name is another id"))))

(deftest memobase-cell-an-empty-memory-says-so (with-memobase ()
  (setf *answer* (lambda (call)
                   (declare (ignore call))
                   (values "{\"errno\":0,\"data\":{\"context\":\"\"}}" 200)))
  (let ((text (memobase-call-tool "memobase_search" "{\"query\":\"the launch date\"}")))
    (is (cl:search "nothing is remembered" text) "an empty context is said plainly")
    (is (cl:search "the launch date" text) "and names the query"))))

(deftest memobase-cell-the-profile-tool-answers-the-profile (with-memobase ()
  (setf *answer* (lambda (call)
                   (declare (ignore call))
                   (values +profile-answer+ 200)))
  (let ((text (memobase-call-tool "memobase_profile" "{}")))
    (is (cl:search "work" text) "the topic Memobase holds")
    (is (cl:search "engineer" text) "and its sub-topic")
    (is (cl:search +user+ text) "named with the user it belongs to"))
  (is (equal (format nil "~a/users/profile/~a" +base+ +user+) (path-of (first *calls*))) "asked of the profile endpoint")))

;;; --- the refusals -------------------------------------------------------------------

(deftest memobase-cell-a-business-error-carries-memobase-s-own-sentence (with-memobase ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +errno-answer+ 200)))
  (let ((condition (signals-error nodecode-memobase:memobase-error
                                  (memobase:search "anything"))))
    (is (cl:search "user not found" (princ-to-string condition)) "the server's own reason"))))

(deftest memobase-cell-a-refused-status-is-named (with-memobase ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values "{}" 401)))
  (let ((condition (signals-error nodecode-memobase:memobase-error (memobase:profile))))
    (is (cl:search "HTTP 401" (princ-to-string condition)) "the status is named"))))

(deftest memobase-cell-a-call-with-no-query-never-reaches-the-wire (with-memobase ()
  (let ((condition (signals-error nodecode-memobase:memobase-error (memobase:search "  "))))
    (is (cl:search "needs a query" (princ-to-string condition))))
  (is (null *calls*) "and nothing was dialled")))

;;; --- the write --------------------------------------------------------------------------

(deftest memobase-cell-remember-writes-one-blob (with-memobase ()
  (let ((text (memobase:remember "the build is green on main"
                                 :assistant "noted, I will not ask again")))
    (is (cl:search "wrote 2 messages" text) "the write is answered with what it wrote"))
  (let* ((call (first *calls*))
         (body (nlk:decode-json (getf call :content)))
         (messages (nlk:json-value body :array "blob_data" "messages")))
    (is (eq :post (getf call :method)))
    (is (equal (format nil "~a/blobs/insert/~a" +base+ +user+) (path-of call)) "to the insert endpoint, for the same id")
    (is (equal "false" (param call "wait_process")) "asynchronously, as the original wrote")
    (is (equal "chat" (nlk:json-value body :string "blob_type")))
    (is (= 2 (length messages)) "both sides of the exchange")
    (is (equal "the build is green on main"
               (nlk:json-value (aref messages 0) :string "content")))
    (is (equal "user" (nlk:json-value (aref messages 0) :string "role")))
    (is (equal "assistant" (nlk:json-value (aref messages 1) :string "role"))))))

;;; --- the id the original derived ------------------------------------------------------------

(deftest memobase-cell-the-user-id-is-the-uuid5-the-original-used ()
  ;; The original hashed a name into Memobase's own id scheme, so its server
  ;; and this cell read one store. SHA-1 is the one primitive that needs
  ;; proving, against its published vector.
  (is (equal "a9993e364706816aba3e25717850c26c9cd0d89d"
             (nodecode-memobase::hex-text
              (nodecode-memobase::sha1-octets
               (sb-ext:string-to-octets "abc" :external-format :utf-8)))) "SHA-1's published vector for abc")
  (is (equal +user+ (nodecode-memobase::user-id "user")) "the default user name hashes to the id an outside uuid5 answers")
  (is (equal "27010d7b-952c-5331-85bc-028568172a1b"
             (nodecode-memobase::user-id "someone")) "and so does another name")
  (is (string/= (nodecode-memobase::user-id "user") (nodecode-memobase::user-id "alice")) "two names are two stores"))

;;; --- the operator's door ---------------------------------------------------------------------

(deftest memobase-cell-the-command-answers-status-profile-and-a-query (with-memobase ()
  (let ((bare (cell-entry "nodecode-memobase" "memobase" "" "memobase-cell-1")))
    (is (cl:search +base+ bare) "with no argument it says where the cell points")
    (is (cl:search +user+ bare) "and which user it reads")
    (is (cl:search "memobase_search" bare) "and what it gives the model"))
  (is (cl:search "2000" (cell-entry "nodecode-memobase" "memobase" "status" "memobase-cell-1")) "status says the budget the section carries")
  (let ((text (cell-entry "nodecode-memobase" "memobase" "the build" "memobase-cell-1")))
    (is (cl:search "the build is green on main" text) "a bare query searches"))
  (setf *answer* (lambda (call) (declare (ignore call)) (values +profile-answer+ 200)))
  (is (cl:search "engineer" (cell-entry "nodecode-memobase" "memobase" "profile" "memobase-cell-1")) "profile reads the whole profile")))

