;;;; cell.lisp --- the town: a resident per session, a building per tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the passive bridge in Hermes Town
;;;; (sxuff/hermes-town): the same idea, one resident per running session and
;;;; one walk across the square per tool call, on the same privacy rule. What
;;;; is kept about a session is a pseudonym derived from the section's salt
;;;; and the session id, a role, a place, a tool name and whether the call
;;;; failed -- never a prompt, a tool argument, a result or a session id.
;;;;
;;;; Two observers and nothing that blocks. The :FRAME hook reads the turn
;;;; facts the gateway publishes, the :TOOL hook reads one call's name and
;;;; whether it refused, and both file into a bounded town in memory. /town
;;;; answers it as text, /town/ as a page (page.lisp), and /api/town/snapshot
;;;; as the JSON the page polls.

(in-package #:nodecode-town)

(nlk:define-peripheral town :not-running t :lock "town")

;;; --- the town's vocabulary ------------------------------------------------

(defparameter +places+
  '(("library" . "Library") ("workshop" . "Workshop") ("forge" . "Forge")
    ("post" . "Post office") ("observatory" . "Observatory") ("hall" . "Town hall")
    ("market" . "Market") ("tavern" . "Tavern") ("plaza" . "Plaza"))
  "Every place a resident can stand, in the town's own spelling.")

(defparameter *exact-places*
  '(("eval" . "forge") ("sh" . "forge") ("lsp" . "forge") ("guard" . "forge")
    ("help" . "library") ("index" . "library") ("memory" . "library")
    ("board" . "workshop") ("team" . "hall") ("goal" . "hall") ("cron" . "hall")
    ("websearch" . "observatory") ("web" . "observatory") ("chrome" . "observatory")
    ("prs" . "post") ("link" . "post") ("qa" . "post") ("mcp" . "market"))
  "Tool names whose building is known; a name not here falls to *PLACE-RULES*.")

(defparameter *place-rules*
  '(("read" . "library") ("search" . "library") ("find" . "library")
    ("list" . "library") ("recall" . "library") ("get" . "library")
    ("write" . "workshop") ("edit" . "workshop") ("patch" . "workshop")
    ("create" . "workshop") ("add" . "workshop") ("update" . "workshop")
    ("run" . "forge") ("exec" . "forge") ("shell" . "forge") ("bash" . "forge")
    ("test" . "forge") ("build" . "forge") ("scan" . "forge")
    ("send" . "post") ("post" . "post") ("reply" . "post") ("notify" . "post")
    ("message" . "post") ("book" . "post") ("call" . "post")
    ("web" . "observatory") ("fetch" . "observatory") ("browse" . "observatory")
    ("vision" . "observatory") ("preview" . "observatory") ("image" . "observatory")
    ("plan" . "hall") ("delegate" . "hall") ("session" . "hall") ("experience" . "hall")
    ("skill" . "market") ("tool" . "market"))
  "Substrings that place a tool the exact table does not carry; first match wins.")

(defparameter +snapshot-version+ "nodecode-town.snapshot.v1"
  "The wire name of what the snapshot route answers.")

(defparameter +journal-limit+ 256
  "How many events the town keeps at most; the section may ask for fewer.")

;;; --- the town ------------------------------------------------------------

(defstruct (resident (:constructor make-resident (key role)))
  "One session in the town: the pseudonym it is known by, and where it stands."
  key role (place "tavern") (state "idle") (tools 0) (events 0) (seen 0))

(defvar *residents-table* (make-hash-table :test #'equal)
  "Resident key -> resident. Bounded by the section's `residents'.")

(defvar *journal* '()
  "The town's bounded journal, newest first: plists of :KEY :KIND :PLACE :TOOL :OUTCOME :AT.")

(defvar *received* 0
  "Events filed since the town last started.")

(defvar *dropped* 0
  "Events a full journal pushed out, and residents a full town retired.")

(defvar *last-error* nil
  "Why the last observation could not be filed, or NIL: a broken observer never fails a turn.")

(defvar *last-session* nil
  "The raw session id of the newest turn fact seen; the fallback outside a turn.")

(defun reset-town ()
  "Empty the town: no residents, no journal, no counters."
  (setf *residents-table* (make-hash-table :test #'equal)
        *journal* '()
        *received* 0
        *dropped* 0
        *last-error* nil
        *last-session* nil)
  t)

(defun received ()
  "How many events the town has filed since it started."
  *received*)

(defun journal ()
  "The journal, newest first."
  *journal*)

(defun residents ()
  "Every resident, the one seen most recently first."
  (sort (loop for resident being the hash-values of *residents-table* collect resident)
        #'> :key #'resident-seen))

(defun pseudonym (class id)
  "The resident key the town shows for session ID: not reversible without the salt."
  (format nil "h/~a/~a" class
          (subseq (nlk:sha256-text
                   (format nil "~a~c~a" (or (setting :salt) "") #\Nul id))
                  7 23)))

(defun journal-limit ()
  "How many events this section keeps."
  (min +journal-limit+ (setting :journal)))

(defun file-event (key kind &key place tool outcome)
  "File one event under KEY, dropping the oldest when the journal is full."
  (incf *received*)
  (push (list :key key :kind kind :place place :tool tool :outcome outcome
              :at (get-universal-time))
        *journal*)
  (let ((limit (journal-limit)))
    (when (> (length *journal*) limit)
      (incf *dropped* (- (length *journal*) limit))
      (setf *journal* (subseq *journal* 0 limit))))
  key)

(defun prune-town ()
  "Retire the quietest residents past the section's ceiling, and say so."
  (loop while (> (hash-table-count *residents-table*) (setting :residents)) do
    (let* ((all (sort (loop for resident being the hash-values of *residents-table*
                            collect resident)
                      #'< :key #'resident-seen))
           (quietest (first all)))
      (if (null quietest)
          (return)
          (progn
            (remhash (resident-key quietest) *residents-table*)
            (incf *dropped*)
            (file-event (resident-key quietest) "departed" :place (resident-place quietest)))))))

(defun touch-resident (class id)
  "The resident for session ID, spawned in CLASS the first time it is seen."
  (let* ((key (pseudonym class id))
         (resident (gethash key *residents-table*)))
    (if resident
        (setf (resident-seen resident) (get-universal-time))
        (progn
          (setf resident (make-resident key (if (equal class "child") "child" "coordinator"))
                (gethash key *residents-table*) resident
                (resident-seen resident) (get-universal-time))
          (file-event key "spawned" :place "plaza")
          (prune-town)))
    resident))

(defun send-to (resident place state)
  "RESIDENT walks to PLACE and is drawn in STATE."
  (setf (resident-place resident) place
        (resident-state resident) state
        (resident-seen resident) (get-universal-time))
  resident)

;;; --- the tool vocabulary --------------------------------------------------

(defun tool-name (tool)
  "TOOL as the town names it: trimmed, lowercased, an mcp__server__ prefix dropped."
  (when (stringp tool)
    (let* ((name (string-downcase (string-trim '(#\Space #\Tab #\Newline) tool)))
           (cut (search "__" name :from-end t)))
      (if (and cut (uiop:string-prefix-p "mcp__" name))
          (subseq name (+ cut 2))
          name))))

(defun safe-tool (tool)
  "The publishable tool name: a name-shaped word, or the literal `tool`."
  (let ((name (tool-name tool)))
    (if (and name (plusp (length name)) (<= (length name) 64)
             (every (lambda (character)
                      (or (alphanumericp character) (find character "_.:-")))
                    name))
        name
        "tool")))

(defun place-for-tool (tool)
  "The building TOOL is worked at: its known one, else the rule whose word comes
first in its name -- web_search is a search at the observatory, not a read in
the library -- else the market."
  (let ((name (tool-name tool)))
    (if (null name)
        "market"
        (or (cdr (assoc name *exact-places* :test #'string=))
            (let ((best nil) (best-at nil))
              (loop for (needle . place) in *place-rules*
                    for at = (search needle name)
                    when (and at (or (null best-at) (< at best-at)))
                      do (setf best place best-at at))
              best)
            "market"))))

(defun verb-for (place)
  "The one word a resident is doing at PLACE."
  (cond ((equal place "library") "reading")
        ((equal place "workshop") "making")
        ((equal place "forge") "running")
        ((equal place "post") "sending")
        ((equal place "observatory") "looking")
        ((equal place "hall") "planning")
        ((equal place "market") "haggling")
        (t "resting")))

;;; --- the observers -------------------------------------------------------

(defun turn-session ()
  "The session the running turn belongs to, or the newest one a frame named."
  (or (ignore-errors (getf (nle:turn) :session-id)) *last-session*))

(defun turn-role ()
  "`child' for a delegated turn, `coordinator' for a top-level one."
  (let ((turn (ignore-errors (nle:turn))))
    (if (and turn (getf turn :parent)) "child" "coordinator")))

(defun record-frame (op)
  "File the turn fact OP carries, when it is one of the four the town draws."
  (multiple-value-bind (kind payload metadata turn-id) (nlk:frame-fact op)
    (declare (ignore payload metadata turn-id))
    (let ((session (getf op :session-id)))
      (when (and kind session)
        (setf *last-session* session)
        (cond
          ((equal kind "turn.started")
           (let ((resident (touch-resident "main" session)))
             (send-to resident "hall" "thinking")
             (file-event (resident-key resident) "assigned" :place "hall")))
          ((member kind '("turn.completed" "turn.failed" "turn.cancelled") :test #'equal)
           (let* ((key (pseudonym "main" session))
                  (resident (gethash key *residents-table*)))
             (when resident
               (send-to resident "tavern" "idle")
               (file-event key
                           (if (equal kind "turn.completed") "completed" "failed")
                           :place "tavern"
                           :outcome (if (equal kind "turn.completed") "ok" (subseq kind 5)))))))
        kind))))

(defun observe-frame (op next)
  "The :FRAME watcher: NEXT publishes the frame first, then the fact is filed.
A broken observer is the town's, never the turn's."
  (prog1 (funcall next op)
    (handler-case (record-frame op)
      (error (condition)
        (setf *last-error* (princ-to-string condition))))))

(defun record-tool (name session failed)
  "File one tool call: the resident walks to NAME's building and the call is journaled."
  (when session
    (let* ((place (place-for-tool name))
           (resident (touch-resident "main" session)))
      (send-to resident place (verb-for place))
      (incf (resident-tools resident))
      (incf (resident-events resident))
      (file-event (resident-key resident) "tool_started" :place place
                  :tool (safe-tool name) :outcome (if failed "error" "ok")))))

(defun observe-tool (op next)
  "The :TOOL around hook: the call answers exactly what it would have, and the
walk it caused is filed beside it. A refused call is drawn failed."
  (let ((name (getf op :name))
        (session (turn-session))
        (result nil)
        (failed nil))
    (unwind-protect
         (handler-case (setf result (funcall next op))
           (error (condition) (setf failed t) (error condition)))
      (handler-case
          (record-tool name session
                       (or failed (and (stringp result)
                                       (uiop:string-prefix-p "ERROR:" result))))
        (error (condition) (setf *last-error* (princ-to-string condition)))))))

;;; --- what the town answers ------------------------------------------------

(defun resident-line (resident)
  "One resident as /town shows it: its pseudonym, role, place and work."
  (format nil "  ~a  ~a  ~a  ~a~@[  ~d call~:p~]"
          (subseq (resident-key resident) 7)
          (resident-role resident)
          (resident-place resident)
          (resident-state resident)
          (resident-tools resident)))

(defun town-text ()
  "The town as text: who is in it and where each one stands."
  (let ((all (residents)))
    (format nil "~d resident~:p, ~d event~:p filed~@[, ~d dropped~].~%~{  ~a~%~}"
            (length all) *received* *dropped*
            (if all
                (mapcar (lambda (resident) (subseq (resident-line resident) 2)) all)
                (list "(none yet: run a turn, or call a tool)")))))

(defun page-text ()
  "Where the page is, on the port the gateway answers."
  (multiple-value-bind (url token) (nle:gateway-endpoint)
    (declare (ignore token))
    (format nil "Town page: ~a/town/  (it polls ~a~a)"
            (or url "http://127.0.0.1:PORT") (or url "") +snapshot-route+)))

(defun split-words (text)
  "TEXT as words."
  (loop for word in (uiop:split-string (or text "") :separator '(#\Space #\Tab))
        when (plusp (length word)) collect word))

(defun run-slash (args session-id)
  "The /town command: the town in text, its page, or an emptied town."
  (declare (ignore session-id))
  (let ((verb (string-downcase (or (first (split-words args)) ""))))
    (cond
      ((member verb '("" "show" "status") :test #'equal)
       (format nil "~a~%~a" (town-text) (page-text)))
      ((member verb '("page" "open") :test #'equal)
       (page-text))
      ((member verb '("clear" "reset") :test #'equal)
       (reset-town)
       "town cleared: every resident went home, and the journal is empty")
      (t (format nil "usage: /town [page | clear]~%~a~%~a" (town-text) (page-text))))))

(defun snapshot ()
  "The town as the page and the JSON route read it: pseudonyms, places, tool names."
  (nlk:json-object
   "version" +snapshot-version+
   "received" *received*
   "dropped" *dropped*
   "residents" (coerce (loop for resident in (residents)
                             collect (nlk:json-object
                                      "key" (resident-key resident)
                                      "role" (resident-role resident)
                                      "place" (resident-place resident)
                                      "state" (resident-state resident)
                                      "tools" (resident-tools resident)
                                      "seen" (resident-seen resident)))
                        'vector)
   "events" (coerce (loop for event in (subseq *journal* 0 (min 24 (length *journal*)))
                          collect (nlk:json-object
                                   "key" (getf event :key)
                                   "kind" (getf event :kind)
                                   "place" (getf event :place)
                                   "tool" (getf event :tool)
                                   "outcome" (getf event :outcome)
                                   "at" (getf event :at)))
                     'vector)))

(defun snapshot-route (env)
  "GET /api/town/snapshot: the town the page polls."
  (declare (ignore env))
  (snapshot))

;;; --- the declaration ------------------------------------------------------

(defparameter +manual+
  (concatenate
   'string
   "The town draws one resident per session and one building per place: a tool"
   " call walks its resident to the building that tool is worked in, and a finished"
   " turn sits it down in the tavern. It answers four ways. (town:snapshot) is the"
   " JSON, /town in the composer prints the text, /town page names the URL of the"
   " page the cell serves on the gateway's own port (/town/), which polls"
   " /api/town/snapshot every two seconds, and /town clear empties it. Nothing baked"
   " in: (town:journal) is the journal, (town:residents) the residents, and every"
   " setting is in the town section of the config. What it keeps about a session is a"
   " pseudonym, a role, a place, a tool name and whether the call failed - never a"
   " prompt, an argument or a result, so the page can be left open where someone"
   " else sees it.")
  "What (help :town) answers.")

(nle:define-cell town
  (:section ("town")
    (:guide "salt is required: any word of your own, so residents are pseudonymous; residents and journal have defaults")
    ("salt" :string :required t
     :doc "a word of your own: a resident's name is derived from it and its session id, and is not reversible without it")
    ("residents" :integer :default 24 :min 1
     :doc "how many sessions the town holds at once; past this the quietest departs")
    ("journal" :integer :default 128 :min 8
     :doc "how many events the town keeps; older ones fall out"))
  (:hook :frame #'observe-frame)
  (:hook :tool #'observe-tool)
  (:command "town" 'run-slash
            :description "The town: one resident per session, a building per tool"
            :argument-hint "page | clear")
  (:help :town "town: one resident per session, a building per tool, and a page to watch it on" +manual+)
  (:route "/town/" #'serve-page :public t)
  (:route "/town" #'redirect-to-page :public t)
  (:route "/api/town/snapshot" #'snapshot-route :public t)
  (:start (lambda ()
            (reset-town)
            (nle:on-stop (lambda () (reset-town))))))
