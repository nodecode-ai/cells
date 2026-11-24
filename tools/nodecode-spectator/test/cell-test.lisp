;;;; cell-test.lisp --- the invite, the mask, the page, an approved suggestion.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a real store: that a
;;;; start installs the command, the tool, the manual and the page's route and
;;;; a stop takes every one of them back; that a share refuses without an
;;;; address to hand out and answers one with it; that the page's lines are
;;;; masked, that a guest's suggestion arrives and only an explicit approve
;;;; puts it into the session -- through the ingress seam, so nothing here
;;;; starts a turn.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------

(define-cell-lifecycle-tests "spectator"
  (:command "spectator")
  (:help :spectator)
  (:running (is (equal "Spectator" (nodecode-spectator::setting :title)) "the page's name")
            (is (equal 404 (first (nodecode-spectator::guest-route
                                   (spectator-env "nope")))) "a token the cell does not know is a 404, not a raise"))
  (:refused ("invite_minutes" 0) ("max_events" 5))
  (:idle spectator:spectator-error (spectator:share "s1")))

;;; --- the mask ----------------------------------------------------------------

(deftest spectator-cell-the-mask-hides-paths-and-assignments ()
  (is (equal "~/... and NAME=*** and ***"
             (nodecode-spectator::mask-text
              "/home/someone/notes.txt and NAME=secret and aaaaaaaaaaaaaaaaaaaaaaaa123")) "a path, an assignment and an opaque token, each rewritten")
  (is (equal "kept /home/someone/notes.txt"
             (nodecode-spectator::mask-text "kept /home/someone/notes.txt" :mask nil)) "mask off keeps the line")
  (is (<= (length (nodecode-spectator::mask-text
                    (format nil "~{~a ~}" (loop repeat 200 collect "word"))))
          400) "a line is clipped"))

(deftest spectator-cell-a-fact-reads-as-one-masked-line ()
  (is (equal "called read_file"
             (nodecode-spectator::fact-line
              "turn.tool_call_started" (nlk:json-object "name" "read_file") t)))
  (is (equal "turn done" (nodecode-spectator::fact-line "turn.completed" (nlk:json-object) t)))
  (is (search "opened ~/..."
              (nodecode-spectator::fact-line
               "turn.assistant_message_completed"
               (nlk:json-object "message" "opened /home/someone/x.lisp and read it") t)) "an assistant message's paths are masked")
  (is (null (nodecode-spectator::fact-line "session.title" (nlk:json-object) t)) "a kind the page has nothing to say about is left out"))

;;; --- sharing ------------------------------------------------------------------

(deftest spectator-cell-a-share-refuses-without-an-address ()
  (with-cell-stop ((spectator-start))
    (let ((condition (signals-error spectator:spectator-error (spectator:share "s1"))))
      (is (search "public_url" (spectator::spectator-error-detail condition)) "the refusal names the setting"))))

(deftest spectator-cell-a-share-answers-the-address-a-guest-opens ()
  (with-temp-store ()
    (ensure-durable-session "s1")
    (with-cell-stop ((spectator-start "public_url" "https://share.example.org/"
                                      "invite_minutes" 30))
    (let* ((text (spectator:share "s1"))
           (token (spectator-token text)))
      (is (search "https://share.example.org/spectator/" text) "the address is the section's")
      (is (search "30 minute" text) "and the section's lifetime")
      (is (= 24 (length token)) "the token is 24 hex characters")
      (is-present (invite (nodecode-spectator::live-invite token))
        "the invite the page reads is the one the address names"
        (is (equal "s1" (getf invite :session))))
      (is (null (nodecode-spectator::live-invite "000000000000000000000000")) "a token nothing named is no invite")))))

;;; --- the page -------------------------------------------------------------------

(deftest spectator-cell-the-page-and-its-facts-are-masked ()
  (with-temp-store ()
    (ensure-durable-session "s1")
    (nlk:record-event "s1" "spectator.line" (list :line "called read_file"))
    (nlk:record-event "s1" "spectator.line" (list :line "wrote /home/someone/out.txt"))
    (with-cell-stop ((spectator-start "public_url" "https://share.example.org"
                                      "title" "Demo"))
      (let* ((token (spectator-token (spectator:share "s1")))
             (page (nodecode-spectator::guest-route (spectator-env token))))
        (is (equal 200 (first page)) "the page answers")
        (is (search "<h1>Demo</h1>" (first (third page))) "under the section's name")
        (is (search token (first (third page))) "and carries the token it was opened with")
        (let ((events (nodecode-spectator::guest-route
                       (spectator-env token :op "events" :query "since=0"))))
          (is (hash-table-p events) "the facts are a JSON body")
          (let ((lines (loop for event across (gethash "events" events) collect (gethash "line" event))))
            (is (equal '("called read_file" "wrote ~/...") lines) "each fact one masked line, oldest first")))
        (is (equal 404 (first (nodecode-spectator::guest-route
                               (spectator-env "unknown")))) "a token nothing minted is a 404")))))

;;; --- a guest's suggestion ---------------------------------------------------------

(deftest spectator-cell-only-an-approve-puts-a-suggestion-into-the-session ()
  (with-temp-store ()
    (ensure-durable-session "s1")
    (with-cell-stop ((spectator-start "public_url" "https://share.example.org"))
      (let* ((token (spectator-token (spectator:share "s1")))
             (answer (nodecode-spectator::guest-route
                      (spectator-env token :op "suggest" :method :post
                                     :query "text=try%20the%20parser%20test")))
             (sent nil))
        (is (hash-table-p answer) "a suggestion answers a JSON receipt")
        (is (equal 1 (length (spectator:pending "s1"))) "and waits for the operator")
        (is (null sent) "nothing reached the session by itself")
        (let ((nodecode-spectator::*submit*
                (lambda (session text &key source source-id)
                  (setf sent (list session text source source-id)) t)))
          (let ((one (first (spectator:pending "s1"))))
            (spectator:approve (getf one :id))
            (is (equal "s1" (first sent)) "the approve reaches its own session")
            (is (search "try the parser test" (second sent)) "with the guest's words")
            (is (equal "approved" (getf (first (spectator:pending "s1")) :status)))
            (is (search "already approved"
                        (handler-case (progn (spectator:approve (getf one :id)) "")
                          (spectator:spectator-error (condition)
                            (spectator::spectator-error-detail condition)))) "approving twice refuses instead of saying it twice")))
        (spectator:revoke)
        (is (search "closed or expired"
                    (first (third (nodecode-spectator::guest-route
                                   (spectator-env token :op "suggest" :method :post
                                                  :query "text=late"))))) "a revoked invite takes no more suggestions")))))
