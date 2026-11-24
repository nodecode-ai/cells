;;;; cell-test.lisp --- the lifecycle, the tool, /scratchpad, cleanup, the rule.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops, the scratchpad tool answers
;;;; through the seam the model calls it by, a buffer is edited in place and a
;;;; patch that matches twice is a refusal rather than a guess, a promoted
;;;; buffer lands in a real file only when nobody would be overwritten, a name
;;;; that is no plain file name is refused, cleanup removes its own idle
;;;; folders and nothing else, and the per-turn rule names the buffers this
;;;; session already has. Every call runs against folders in the test's own
;;;; home, so nothing here reads the operator's drafts.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun scratchpad-tool (arguments)
  "Run the registered scratchpad tool with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool "scratchpad")))
    (assert entry () "no tool named scratchpad")
    (funcall (third entry) (nle::tool-body-arguments entry arguments))))

(defmacro with-session ((id) &body body)
  "BODY inside a turn of session ID, which is what a tool call runs in."
  `(let ((nle::*live-turn* (nle::%make-live-turn :session-id ,id :turn-id "t1")))
     ,@body))

(defun scratchpad-answer (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it."
  (cell-json text))

(defun scratchpad-content (name session)
  "The content the scratchpad tool answers for buffer NAME of SESSION."
  (nlk:json-value (scratchpad-answer
                    (with-session (session)
                      (scratchpad-tool (nlk:json-object "action" "read" "name" name))))
                   :string "content"))

(defun scratchpad-folder ()
  (namestring (nlk:home "scratchpad/")))

(defun forget-buffers (session)
  "Drop SESSION's folder, so one test starts from none."
  (let ((folder (merge-pathnames (format nil "~a/" session) (scratchpad-folder))))
    (when (uiop:directory-exists-p folder)
      (uiop:delete-directory-tree folder :validate t))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "scratchpad" "SCRATCHPAD-CELL-" :start nodecode-scratchpad:start-cell)

(define-cell-lifecycle-tests "scratchpad"
  (:hooks :round)
  (:help :scratchpad)
  (:command "scratchpad")
  (:running (is (nle::find-tool "scratchpad") "the scratchpad tool is registered")
            (is (pathnamep (nodecode-scratchpad::root)) "the folder under the home is resolved")
            (is (eql 2048 (nodecode-scratchpad::setting :max-buffer-kb)) "the ceiling is the section's default"))
  (:stopped (is (null (nle::find-tool "scratchpad")) "stop unregisters the tool"))
  (:refused ("idle_days" 0))
  (:idle nodecode-scratchpad:scratchpad-error (nodecode-scratchpad::run-command "" "s1")))

;;; --- the tool -----------------------------------------------------------------

(deftest scratchpad-cell-a-draft-is-written-read-patched-and-listed ()
  (forget-buffers "session-draft")
  (with-cell-stop ((scratchpad-start))
    (with-session ("session-draft")
      (let ((written (scratchpad-answer
                      (scratchpad-tool (nlk:json-object "action" "write" "name" "main.py"
                                                         "content" (format nil "(print 1)~%"))))))
        (is (eq t (nlk:json-value written :boolean "success")) "the write answers success")
        (is (eq t (nlk:json-value written :boolean "created")) "the first write creates it")
        (is (= 10 (nlk:json-value written :integer "bytes")) "its bytes are counted"))
      (is (equal (format nil "(print 1)~%") (scratchpad-content "main.py" "session-draft")) "read answers the buffer's text")
      (scratchpad-tool (nlk:json-object "action" "patch" "name" "main.py"
                                          "old_text" "(print 1)" "new_text" "(print 2)"))
      (is (equal (format nil "(print 2)~%") (scratchpad-content "main.py" "session-draft")) "the patch changed the buffer in place, and nothing else")
      (let ((listed (scratchpad-answer (scratchpad-tool (nlk:json-object "action" "list")))))
        (is (equal '("main.py")
                   (loop for row across (nlk:json-value listed :array "buffers")
                         collect (nlk:json-value row :string "name"))) "the list names the session's one buffer"))
      (scratchpad-tool (nlk:json-object "action" "delete" "name" "main.py"))
      (is-carrying (result (scratchpad-tool (nlk:json-object "action" "read" "name" "main.py")))
                   ("ERROR:" "a read after the delete is a refusal")
                   ("no buffer named" "and it names the buffer")))))

(deftest scratchpad-cell-a-patch-matching-twice-is-refused ()
  (forget-buffers "session-twice")
  (with-cell-stop ((scratchpad-start))
    (with-session ("session-twice")
      (scratchpad-tool (nlk:json-object "action" "write" "name" "notes.md"
                                          "content" (format nil "a~%a~%")))
      (is-carrying (result (scratchpad-tool (nlk:json-object "action" "patch" "name" "notes.md"
                                                                  "old_text" "a" "new_text" "b")))
                   ("ERROR:" "an ambiguous patch is a refusal")
                   ("matches 2 places" "and it counts them"))
      (is (equal (format nil "a~%a~%") (scratchpad-content "notes.md" "session-twice")) "the refusal changed nothing")
      (scratchpad-tool (nlk:json-object "action" "patch" "name" "notes.md"
                                         "old_text" "a" "new_text" "b" "replace_all" t))
      (is (equal (format nil "b~%b~%") (scratchpad-content "notes.md" "session-twice")) "replace_all replaces every match"))))

(deftest scratchpad-cell-a-promoted-buffer-is-a-real-file ()
  (forget-buffers "session-promote")
  (let ((target (namestring (nlk:home "scratchpad-test/draft.txt"))))
    (when (probe-file target) (delete-file target))
    (with-cell-stop ((scratchpad-start))
      (with-session ("session-promote")
        (scratchpad-tool (nlk:json-object "action" "write" "name" "draft.txt"
                                            "content" "kept words"))
        (let ((promoted (scratchpad-answer
                         (scratchpad-tool (nlk:json-object "action" "promote" "name" "draft.txt"
                                                            "path" target)))))
          (is (equal target (nlk:json-value promoted :string "path")) "it answers where the file went")
          (is (equal "kept words" (uiop:read-file-string target)) "and the file holds the bytes"))
        (is-carrying (result (scratchpad-tool (nlk:json-object "action" "promote" "name" "draft.txt"
                                                                    "path" target)))
                     ("already exists" "a file already there is not replaced uninvited"))
        (is-carrying (result (scratchpad-tool (nlk:json-object "action" "promote" "name" "draft.txt"
                                                                    "path" "relative/draft.txt")))
                     ("path must be absolute" "a relative destination is refused"))))
    (when (probe-file target) (delete-file target))))

(deftest scratchpad-cell-a-name-outside-a-plain-file-is-refused ()
  (with-cell-stop ((scratchpad-start))
    (with-session ("session-name")
      (is-carrying (result (scratchpad-tool (nlk:json-object "action" "write"
                                                          "name" "../escape.py" "content" "x")))
                   ("ERROR:" "a name that climbs out is a refusal")
                   ("no buffer name" "and it says what a name is")))))

(deftest scratchpad-cell-a-call-outside-a-session-is-refused ()
  (with-cell-stop ((scratchpad-start))
    (let ((nle::*live-turn* nil)
          (nlk:*scribe-session-id* nil))
      (is-carrying (result (scratchpad-tool (nlk:json-object "action" "list")))
                   ("ERROR:" "a call with no session is a refusal")
                   ("no session is bound" "and it says why")))))

;;; --- /scratchpad and the rule ---------------------------------------------------

(deftest scratchpad-cell-the-command-shows-this-sessions-buffers ()
  (forget-buffers "session-command")
  (with-cell-stop ((scratchpad-start))
    (with-session ("session-command")
      (scratchpad-tool (nlk:json-object "action" "write" "name" "page.html"
                                          "content" "<p>hi</p>")))
    (is-carrying (answer (cell-entry "nodecode-scratchpad" "scratchpad" "" "session-command"))
                 ("scratchpad: 1 buffer" "the command counts this session's buffers")
                 ("page.html" "and names them")
                 ("/scratchpad read NAME" "and says how to open one"))
    (is-carrying (answer (cell-entry "nodecode-scratchpad" "scratchpad" "read page.html"
                                     "session-command"))
                 ("<p>hi</p>" "read NAME shows the buffer"))
    (is-carrying (answer (cell-entry "nodecode-scratchpad" "scratchpad" "delete page.html"
                                     "session-command"))
                 ("deleted" "delete NAME removes it"))
    (is-carrying (answer (cell-entry "nodecode-scratchpad" "scratchpad" "" "session-command"))
                 ("scratchpad: 0 buffers" "and the next listing is empty"))))

(deftest scratchpad-cell-the-rule-names-this-sessions-buffers ()
  (forget-buffers "session-rule")
  (with-cell-stop ((scratchpad-start))
    (is (equal nodecode-scratchpad:*rule* (nodecode-scratchpad::rule-text "session-empty")) "an empty session gets the rule alone")
    (is (search "[scratchpad]" (nodecode-scratchpad::rule-text "session-empty")) "the rule carries the tool's name")
    (is (search "action=promote" (nodecode-scratchpad::rule-text "session-empty")) "and says a real file is promoted on purpose")
    (with-session ("session-rule")
      (scratchpad-tool (nlk:json-object "action" "write" "name" "lesson.py" "content" "x")))
    (is (search "lesson.py" (nodecode-scratchpad::rule-text "session-rule")) "a session with drafts is told their names")))

;;; --- cleanup -------------------------------------------------------------------

(deftest scratchpad-cell-cleanup-removes-its-own-idle-folders-only ()
  (forget-buffers "session-idle")
  (let ((stray (merge-pathnames "not-ours/" (scratchpad-folder))))
    (ensure-directories-exist (merge-pathnames "keep.txt" stray))
    (with-cell-stop ((scratchpad-start))
      (with-session ("session-idle")
        (scratchpad-tool (nlk:json-object "action" "write" "name" "old.txt" "content" "x")))
      (is (equal '("session-idle")
                 (nodecode-scratchpad:prune :idle-days 1
                                             :now (+ (get-universal-time) (* 3 86400)))) "a folder idle past the setting goes")
      (is (not (uiop:directory-exists-p (merge-pathnames "session-idle/" (scratchpad-folder)))) "and it is gone from disk")
      (is (uiop:directory-exists-p stray) "a folder without the cell's marker stays"))))
