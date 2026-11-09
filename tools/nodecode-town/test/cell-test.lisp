;;;; cell-test.lisp --- lifecycle, the walks it draws, the command, the routes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the config surface (a
;;;; healthy section installs both hooks, the command, the manual topic and
;;;; three routes; a section without its salt, or one whose residents is not
;;;; an integer, is a refusal), that a turn fact spawns a resident and a tool
;;;; call walks it to that tool's building, that the snapshot publishes a
;;;; pseudonym and never the session id or a tool's arguments, that /town
;;;; answers its verbs, and that the page and the redirect are served the way
;;;; the route contract says.

(in-package #:nodecode.test)

;;; --- lifecycle ----------------------------------------------------------------

(define-cell-lifecycle-tests "town"
  (:config "salt" "sugar" "residents" 4)
  (:hooks :frame :tool)
  (:command "town")
  (:help :town)
  (:running (is (zerop (nodecode-town:received)) "a fresh town has filed nothing")
            (is (equal "sugar" (nodecode-town::setting :salt)) "the section's salt is what the town runs on"))
  (:stopped (is (null (nodecode-town:residents)) "stop empties the town"))
  (:refused ("salt" nil)
            ("residents" "many")))

;;; --- a turn, and the walk a tool call causes -------------------------------------

(deftest town-cell-a-turn-spawns-a-resident-and-a-call-walks-it (with-town ()
  (start-a-turn)
  (is (= 1 (length (nodecode-town:residents))) "one session, one resident")
  (let ((resident (first (nodecode-town:residents))))
    (is (equal "hall" (nodecode-town::resident-place resident)) "a turn is planned in the town hall")
    (is (equal "thinking" (nodecode-town::resident-state resident))))
  (is (equal "done" (run-tool-call "web_search" (nlk:json-object "query" "a secret question") "done"))
      "the call answers exactly what it would have")
  (let ((resident (first (nodecode-town:residents))))
    (is (equal "observatory" (nodecode-town::resident-place resident)) "a search is looked up at the observatory")
    (is (equal "looking" (nodecode-town::resident-state resident)))
    (is (= 1 (nodecode-town::resident-tools resident))))
  (run-tool-call "sh" (nlk:json-object "command" "ls") "done")
  (is (equal "forge" (nodecode-town::resident-place (first (nodecode-town:residents))))
      "and a shell is run at the forge")
  (end-the-turn)
  (is (equal "tavern" (nodecode-town::resident-place (first (nodecode-town:residents))))
      "a finished turn sits the resident down in the tavern")
  (is (plusp (nodecode-town:received)))
  (is (null nodecode-town:*last-error*) "nothing failed on the way")))

(deftest town-cell-a-failed-call-is-drawn-failed (with-town ()
  (start-a-turn)
  (run-tool-call "eval" (nlk:json-object "form" "(/ 1 0)") "ERROR: DIVISION-BY-ZERO")
  (let ((event (first (nodecode-town:journal))))
    (is (equal "tool_started" (getf event :kind)))
    (is (equal "error" (getf event :outcome)) "a result that says ERROR is drawn failed")
    (is (equal "eval" (getf event :tool))))))

;;; --- what the snapshot publishes, and what it cannot ----------------------------

(deftest town-cell-the-snapshot-publishes-a-pseudonym-and-nothing-else (with-town ()
  (start-a-turn "s3cr3t-session" "t1")
  (run-tool-call "web_search" (nlk:json-object "query" "/etc/hostname") "done")
  (let* ((snapshot (nodecode-town:snapshot))
         (residents (town-of snapshot))
         (key (nlk:json-value (first residents) :string "key"))
         (encoded (nlk:encode-json-object snapshot)))
    (is (= 1 (length residents)))
    (is (uiop:string-prefix-p "h/main/" key) "a resident is named by its class and a digest")
    (is (= 23 (length key)) "sixteen hex characters of it")
    (is (not (search "s3cr3t" encoded)) "the session id never leaves the process")
    (is (not (search "/etc/hostname" encoded)) "nor a tool argument")
    (is (equal "observatory" (nlk:json-value (first residents) :string "place")))
    (is (equal "tool_started" (nlk:json-value (first (town-of snapshot "events")) :string "kind"))))))

(deftest town-cell-the-pseudonym-is-stable-and-carries-no-id (with-town ("salt" "one")
  (let ((key (nodecode-town:pseudonym "main" "s1")))
    (is (equal key (nodecode-town:pseudonym "main" "s1")) "the same session is named the same way")
    (is (not (equal key (nodecode-town:pseudonym "main" "s2"))) "two sessions are two residents")
    (is (not (search "s1" key)) "the id itself is not in the name")
    (is (not (search "one" key)) "nor is the salt"))))

;;; --- the command ---------------------------------------------------------------

(deftest town-cell-slash-town-shows-the-town-and-clears-it (with-town ()
  (start-a-turn)
  (let ((text (cell-entry "nodecode-town" "town" "")))
    (is (search "1 resident" text) "the town is counted")
    (is (search "hall" text) "and each resident's place is named")
    (is (search "/town/" text) "with where the page is"))
  (is (search "/town/" (cell-entry "nodecode-town" "town" "page")))
  (is (search "usage" (cell-entry "nodecode-town" "town" "sideways")) "an unknown verb says what it takes")
  (is (search "cleared" (cell-entry "nodecode-town" "town" "clear")))
  (is (null (nodecode-town:residents)) "and the town is empty")))

;;; --- the two routes ------------------------------------------------------------

(deftest town-cell-the-page-and-the-redirect-are-served (with-town ()
  (let ((answer (nodecode-town:serve-page (list :request-method :get :path-info "/town/"))))
    (is (= 200 (first answer)))
    (is (search "text/html" (getf (second answer) :content-type)))
    (is (search "Nodecode town" (first (third answer))))
    (is (search "/api/town/snapshot" (first (third answer))) "the page polls the snapshot route")
    (is (not (search "SNAPSHOT" (first (third answer)))) "with its placeholder filled"))
  (is (= 405 (first (nodecode-town:serve-page (list :request-method :post)))))
  (let ((answer (nodecode-town:redirect-to-page (list :request-method :get :path-info "/town"))))
    (is (= 302 (first answer)))
    (is (equal "/town/" (getf (second answer) :location))))))
