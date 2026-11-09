;;;; cell.lisp --- Evalroute: the route table, the card, the ledger, /route and /rate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of keppy/hermes-plugin-evalroute (see NOTICE). The original is a
;;;; thin adapter over its own library; what a user gets from it is three
;;;; things, and they are here in one file:
;;;;
;;;;   * a classifier that turns a task in the user's words into a lane, from
;;;;     keywords alone -- no model call, nothing sent anywhere;
;;;;   * a route card naming the lane's arm (a model and a reasoning effort),
;;;;     why it was picked, the route id, and what that lane has measured so far;
;;;;   * the flywheel: /rate labels the pending route pass, fail or skip, and
;;;;     that ledger is the only thing the card's "payoff" line learns from.
;;;;
;;;; The route table is *LANES*, data a layer may rebind. A route is one row
;;;; in a JSON-lines ledger in the folder the section names, so a label given
;;;; in one session is on the card in the next process. Nothing leaves the
;;;; machine and no key is needed.

(in-package #:nodecode-evalroute)

;;; --- the route table --------------------------------------------------------------

(defparameter *lanes*
  '((:id "build" :effort "high" :label "implement, fix or refactor code"
     :keywords ("refactor" "implement" "fix" "bug" "debug" "compil" "parser" "function"
                "unit test" "patch" "migrat" "rewrite" "class" "module" "endpoint"
                "api" "build" "feature" "regression"))
    (:id "review" :effort "medium" :label "read a diff, a design or a risk critically"
     :keywords ("review" "audit" "critique" "second opinion" "vulnerab" "diff"
                "lint" "risky" "trade-off" "tradeoff" "sanity check"))
    (:id "read" :effort "medium" :label "long documents and specs"
     :keywords ("read" "summar" "paper" "docs" "documentation" "spec" "manual"
                "extract" "article" "transcript" "pdf" "chapter" "long"))
    (:id "quick" :effort "low" :label "a short answer, a rename, a one-liner"
     :keywords ("what is" "what's" "who is" "when " "where " "which " "list" "count"
                "rename" "quick" "one-liner" "one liner" "translate" "define"
                "how many" "spell"))
    (:id "plan" :effort "high" :label "plan, decompose or delegate work"
     :keywords ("plan" "design" "architect" "decompose" "roadmap" "orchestrat"
                "delegate" "break down" "strategy" "propose" "milestone" "scope")))
  "The route table: one plist per lane, a lane's keywords, the effort it starts
at, and what the lane is. Two steps, never one: the lane names the arm, and
the arm is a model and an effort together. Rebind it in a layer to route your
own work.")

(defparameter +ledger-folder+ "evalroute/"
  "Where the ledger lives under the Nodecode home when the section names no folder.")

(defparameter +task-chars+ 200 "How much of a task one ledger row keeps.")
(defparameter +keyword-chars+ 120 "How much of a lane's keyword list the card prints.")

;;; --- lanes -------------------------------------------------------------------------

(defun lane-ids ()
  "Every lane id, in table order."
  (mapcar (lambda (lane) (getf lane :id)) *lanes*))

(defun lane-of (id)
  "The lane ID names, or NIL."
  (and (stringp id) (find id *lanes* :key (lambda (lane) (getf lane :id)) :test #'string-equal)))

(defun keyword-hits (text lane)
  "The keywords of LANE that TEXT carries, in table order: the rules layer's
whole evidence, counted, never guessed."
  (loop for keyword in (getf lane :keywords)
        when (search keyword text)
          collect keyword))

(defun classify (text)
  "TEXT as (values LANE HITS): the lane with the most keyword hits, and which
ones. Table order breaks a tie. => (values NIL NIL) when no lane matched."
  (let ((text (string-downcase (or text "")))
        (best nil)
        (best-hits '()))
    (dolist (lane *lanes*)
      (let ((hits (keyword-hits text lane)))
        (when (> (length hits) (length best-hits))
          (setf best lane
                best-hits hits))))
    (values best best-hits)))

(defun confidence-of (hits)
  "HITS keyword matches as the card's confidence: 1 match is a hunch, 3 is a
lane worth trusting."
  (if (plusp hits)
      (/ (float hits) (+ 2.0 hits))
      0.0))

(defun parse-models (text)
  "TEXT -- the section's `models', as `lane=model, lane=model' -- as an alist
of (lane-id . model-id), ids downcased and trimmed."
  (loop for part in (uiop:split-string (or text "") :separator '(#\, #\; #\Newline))
        for equals = (position #\= part)
        when equals
          collect (cons (string-downcase (nlk:trimmed (subseq part 0 equals)))
                        (nlk:trimmed (subseq part (1+ equals))))))

(defun lane-model (id)
  "The model the section names for lane ID, or NIL."
  (cdr (assoc id (parse-models (setting :models)) :test #'string-equal)))

;;; --- the ledger --------------------------------------------------------------------

(defun ledger-folder ()
  "The folder the ledger file is kept in."
  (let ((named (setting :data-dir)))
    (if (and (stringp named) (plusp (length (nlk:trimmed named))))
        (uiop:ensure-directory-pathname named)
        (nlk:home +ledger-folder+))))

(defun ledger-file ()
  "The ledger: one JSON object a line, newest first."
  (merge-pathnames "routes.jsonl" (ledger-folder)))

(defun now-text ()
  "Now, as UTC ISO-8601 to the second: what a row's timestamp carries."
  (nlk:iso-time (nlk:unix-now) :millis nil))

(defun row-of (object)
  "One ledger row read back from a decoded JSON OBJECT."
  (list :id (or (nlk:json-value object :string "id") "")
        :at (or (nlk:json-value object :string "at") "")
        :lane (or (nlk:json-value object :string "lane") "")
        :model (or (nlk:json-value object :string "model") "")
        :effort (or (nlk:json-value object :string "effort") "")
        :confidence (or (nlk:json-value object :number "confidence") 0.0)
        :hits (loop for hit across (nlk:json-array object "hits") collect hit)
        :task (or (nlk:json-value object :string "task") "")
        :verdict (or (nlk:json-value object :string "verdict") "")
        :note (or (nlk:json-value object :string "note") "")
        :rated-lane (nlk:json-value object :string "rated_lane")))

(defun row-json (row)
  "ROW as the object the ledger writes."
  (nlk:json-object "id" (getf row :id)
                    "at" (getf row :at)
                    "lane" (getf row :lane)
                    "model" (getf row :model)
                    "effort" (getf row :effort)
                    "confidence" (getf row :confidence)
                    "hits" (coerce (getf row :hits) 'vector)
                    "task" (getf row :task)
                    "verdict" (or (getf row :verdict) "")
                    "note" (or (getf row :note) "")
                    "rated_lane" (or (getf row :rated-lane) :null)))

(defun read-rows ()
  "Every row in the ledger, newest first. A file that does not decode gives
the rows it did; a missing one is an empty ledger."
  (let ((file (ledger-file)))
    (if (probe-file file)
        (loop for line in (nlk:lines (uiop:read-file-string file))
              when (plusp (length (nlk:trimmed line)))
                append (let ((object (ignore-errors (nlk:decode-json line))))
                         (when (hash-table-p object)
                           (list (row-of object)))))
        '())))

(defun write-rows (rows)
  "ROWS to the ledger atomically, newest first, keeping the section's
max_routes of them."
  (let* ((file (ledger-file))
         (kept (if (> (length rows) (setting :max-routes))
                   (subseq rows 0 (setting :max-routes))
                   rows)))
    (ensure-directories-exist file)
    (nlk:write-file-atomically
     file
     (format nil "~{~a~%~}" (mapcar (lambda (row) (nlk:encode-json-object (row-json row))) kept))
     :mode :private)
    kept))

(defmacro with-ledger ((rows) &body body)
  "BODY with ROWS read from the ledger, written back under the cell's lock so
two processes never write one ledger from two stale reads."
  `(with-evalroute-lock
     (let ((,rows (read-rows)))
       (multiple-value-prog1 (progn ,@body)
         (write-rows ,rows)))))

(defun next-route-id (rows)
  "The next route id, `r-YYYYMMDD-N', N counting this day's rows."
  (let* ((prefix (format nil "r-~a-" (remove #\- (subseq (now-text) 0 10))))
         (used (loop for row in rows
                     for id = (getf row :id)
                     when (and (stringp id) (uiop:string-prefix-p prefix id))
                       collect (or (ignore-errors (parse-integer id :start (length prefix)
                                                                    :junk-allowed t))
                                   0))))
    (format nil "~a~d" prefix (1+ (reduce #'max used :initial-value 0)))))

(defun pending-row (rows)
  "The newest row no verdict has labelled, or NIL."
  (find-if (lambda (row) (zerop (length (getf row :verdict)))) rows))

(defun row-lane (row)
  "The lane ROW counts for: the correction its rating filed, else its own."
  (or (getf row :rated-lane) (getf row :lane)))

(defun lane-tally (rows id)
  "Lane ID's rows as (:routes :pass :fail :skip :unrated)."
  (let ((tally (list :routes 0 :pass 0 :fail 0 :skip 0 :unrated 0)))
    (dolist (row rows)
      (when (string-equal (row-lane row) id)
        (incf (getf tally :routes))
        (let ((verdict (getf row :verdict)))
          (cond ((string-equal verdict "pass") (incf (getf tally :pass)))
                ((string-equal verdict "fail") (incf (getf tally :fail)))
                ((string-equal verdict "skip") (incf (getf tally :skip)))
                (t (incf (getf tally :unrated)))))))
    tally))

(defun pass-text (tally)
  "TALLY's pass rate in words, or what is missing to have one."
  (let ((rated (+ (getf tally :pass) (getf tally :fail))))
    (if (plusp rated)
        (format nil "~d% pass over ~d rated" (round (* 100 (/ (getf tally :pass) rated))) rated)
        "nothing rated yet")))

(defun tally-text (tally)
  "One lane's measured line: priors until something is rated."
  (if (plusp (+ (getf tally :pass) (getf tally :fail) (getf tally :skip)))
      (format nil "~a (~d route~:p, ~d unrated)"
              (pass-text tally) (getf tally :routes) (getf tally :unrated))
      (format nil "priors: ~d route~:p, nothing rated for this lane yet"
              (getf tally :routes))))

;;; --- the card ----------------------------------------------------------------------

(defun why-text (row)
  "ROW's decided-by line: pinned, the keyword rules, or the default lane."
  (let ((hits (getf row :hits)))
    (cond ((>= (getf row :confidence) 0.99) "pinned to this lane with --lane")
          ((plusp (length hits))
           (format nil "rules match, ~d keyword hit~:p: ~a"
                   (length hits) (nlk:clip (format nil "~{~a~^, ~}" hits) +keyword-chars+)))
          (t (format nil "no keyword hit: the default lane at confidence ~,2f -- say one more sentence about the work, or pin a lane with --lane" (getf row :confidence))))))

(defun next-text (row)
  "ROW's next line: the two switches to set, then the label to leave."
  (let ((model (getf row :model)))
    (if (plusp (length model))
        (format nil "/model ~a, /think ~a; after the work, /rate pass|fail" model (getf row :effort))
        (format nil "no model named for ~a: set models in the evalroute section, as ~a=<model id>"
                (getf row :lane) (getf row :lane)))))

(defun card (row &optional (rows (read-rows)))
  "ROW as the route card: the lane, the arm, why, the id, what it has measured, and the next step."
  (let ((lane (lane-of (getf row :lane))))
    (format nil "evalroute: ~a - ~a~%  arm: ~a, effort ~a~%  decided by: ~a~%  route id: ~a~%  payoff: ~a~%  next: ~a"
            (getf row :lane)
            (or (getf lane :label) "a lane of your own")
            (if (plusp (length (getf row :model))) (getf row :model) "no model named")
            (getf row :effort)
            (why-text row)
            (getf row :id)
            (tally-text (lane-tally rows (getf row :lane)))
            (next-text row))))

;;; --- the verbs ---------------------------------------------------------------------

(define-verb route (task &key lane)
  "Classify TASK into a lane and answer its route card, with the route id
/rate labels. => (values CARD ROW); ROW is the ledger row just written.
LANE pins a lane instead of classifying, and the card says so. Nothing here
switches the model: read the card, then set the arm yourself."
  (let ((text (and (stringp task) (nlk:trimmed task))))
    (unless (and text (plusp (length text)))
      (fail "route it a task in the user's words, as a string; /route lanes names the lanes"))
    (when (and lane (null (lane-of lane)))
      (fail "no lane ~s; the lanes are ~{~a~^, ~}" lane (lane-ids)))
    (with-ledger (rows)
      (multiple-value-bind (found hits)
          (if lane
              (values (lane-of lane) (keyword-hits (string-downcase text) (lane-of lane)))
              (classify text))
        (let* ((weak (and (null lane) (< (length hits) (setting :min-hits))))
               (chosen (if weak (lane-of (setting :default-lane)) found))
               (row (list :id (next-route-id rows)
                          :at (now-text)
                          :lane (getf chosen :id)
                          :model (or (lane-model (getf chosen :id)) "")
                          :effort (getf chosen :effort)
                          :confidence (cond (lane 1.0) (weak 0.0) (t (confidence-of (length hits))))
                          :hits (if weak '() hits)
                          :task (nlk:clip text +task-chars+)
                          :verdict "" :note "" :rated-lane nil)))
          (setf rows (cons row rows))
          (values (card row rows) row))))))

(define-verb rate (verdict &key route-id lane note)
  "Label a route's outcome pass, fail or skip, and answer the one line that
says what was filed and how the lane now measures. --route-id names the row;
without it the newest unlabelled route is the one. LANE files a correction:
the route counted for that lane from now on, not the one the card said."
  (let ((word (and (stringp verdict) (string-downcase (nlk:trimmed verdict)))))
    (unless (member word '("pass" "fail" "skip") :test #'equal)
      (fail "rate it pass, fail or skip; got ~s" verdict))
    (when (and lane (null (lane-of lane)))
      (fail "no lane ~s; the lanes are ~{~a~^, ~}" lane (lane-ids)))
    (with-ledger (rows)
      (let ((row (if (and route-id (plusp (length (nlk:trimmed route-id))))
                     (find (nlk:trimmed route-id) rows :test #'string-equal :key (lambda (r) (getf r :id)))
                     (pending-row rows))))
        (unless row
          (fail (if route-id
                    "no route with id ~s in the ledger"
                    "no pending route in the ledger: /route a task first, then rate it")
                route-id))
        (setf (getf row :verdict) word
              (getf row :note) (or (and (stringp note) (nlk:clip note 400)) "")
              (getf row :rated-lane) (and lane (getf (lane-of lane) :id)))
        (format nil "evalroute: ~a ~a @ ~a -> ~a~@[ (~a)~]~@[; filed as ~a~]~%;  ~a: ~a"
                (getf row :id) (getf row :lane) (getf row :effort) word
                (getf row :note)
                (getf row :rated-lane)
                (row-lane row)
                (tally-text (lane-tally rows (row-lane row))))))))

(define-verb report (&key limit)
  "The flywheel's books: every lane's routes, how many were rated pass, fail
or skipped, and its pass rate. LIMIT counts the newest rows, else the section's
max_routes."
  (let* ((rows (read-rows))
         (count (or (and (integerp limit) (plusp limit) limit) (setting :max-routes)))
         (kept (if (> (length rows) count) (subseq rows 0 count) rows)))
    (format nil "evalroute: ~d route~:p in ~a~%~{  ~a~^~%~}"
            (length kept)
            (namestring (ledger-file))
            (loop for lane in *lanes*
                  for tally = (lane-tally kept (getf lane :id))
                  collect (format nil "~a (~a @ ~a)  ~d route~:p, ~d pass, ~d fail, ~d skip, ~d unrated; ~a"
                                  (getf lane :id)
                                  (or (lane-model (getf lane :id)) "no model named")
                                  (getf lane :effort)
                                  (getf tally :routes) (getf tally :pass) (getf tally :fail)
                                  (getf tally :skip) (getf tally :unrated)
                                  (pass-text tally))))))

(define-verb lanes ()
  "The route table as the card reads it: each lane, its model, the effort it
starts at, and the keywords that pick it."
  (format nil "evalroute lanes (default ~a):~%~{  ~a~^~%~}"
          (setting :default-lane)
          (loop for lane in *lanes*
                for id = (getf lane :id)
                collect (format nil "~a - ~a~%    arm: ~a @ ~a~%    keywords: ~{~a~^, ~}"
                                id (getf lane :label)
                                (or (lane-model id) "no model named")
                                (getf lane :effort)
                                (getf lane :keywords)))))

(define-verb status ()
  "What the cell is doing: the ledger it writes, how many routes it keeps, the
pending route, and the model named for each lane."
  (let* ((rows (read-rows))
         (pending (pending-row rows)))
    (format nil "evalroute: ~d route~:p kept (max ~d) in ~a~%;  pending: ~a~%;  arms: ~{~a~^; ~}~%;  default lane: ~a"
            (length rows) (setting :max-routes) (namestring (ledger-file))
            (if pending
                (format nil "~a (~a @ ~a, unrated)" (getf pending :id)
                        (getf pending :lane) (getf pending :effort))
                "none")
            (loop for lane in *lanes*
                  collect (format nil "~a=~a" (getf lane :id)
                                  (or (lane-model (getf lane :id)) "?")))
            (setting :default-lane))))

;;; --- /route and /rate ---------------------------------------------------------------

(defun unquoted (text)
  "TEXT without the pair of double quotes a shell habit puts round a value."
  (let ((text (or text "")))
    (if (and (> (length text) 1)
             (char= (char text 0) #\") (char= (char text (1- (length text))) #\"))
        (subseq text 1 (1- (length text)))
        text)))

(defun flag-value (words)
  "WORDS' leading value up to the next --flag, joined by one space, with
surrounding double quotes dropped. => (values VALUE REST)."
  (let ((taken '()))
    (loop while (and words (not (uiop:string-prefix-p "--" (first words))))
          do (push (pop words) taken))
    (values (unquoted (format nil "~{~a~^ ~}" (nreverse taken))) words)))

(defun parse-args (text)
  "TEXT as (values WORDS FLAGS): FLAGS a plist of :lane, :note and :route-id,
WORDS what is left, in order. --lane and --route-id take the next word; --note
takes everything to the next --flag, joined, with its double quotes dropped."
  (let ((words (nlk:split-words (or text "")))
        (flags '())
        (rest '()))
    (loop while words
          for word = (pop words)
          do (cond ((string-equal word "--lane")
                    (setf (getf flags :lane) (unquoted (pop words))))
                   ((string-equal word "--route-id")
                    (setf (getf flags :route-id) (unquoted (pop words))))
                   ((string-equal word "--note")
                    (multiple-value-bind (value after) (flag-value words)
                      (setf (getf flags :note) value
                            words after)))
                   (t (push word rest))))
    (values (nreverse rest) flags)))

(defun run-route (args session-id)
  "One /route invocation: /route <task>, /route --lane <lan> <task>, or
/route lanes."
  (declare (ignore session-id))
  (multiple-value-bind (words flags) (parse-args args)
    (let ((task (format nil "~{~a~^ ~}" words)))
      (cond ((zerop (length (nlk:trimmed task))) (lanes))
            ((string-equal (nlk:trimmed task) "lanes") (lanes))
            (t (route task :lane (getf flags :lane)))))))

(defun run-rate (args session-id)
  "One /rate invocation: /rate pass|fail|skip [--route-id <id>] [--lane <lane>]
[--note <text>]; the words after the verdict are the note when --note is absent."
  (declare (ignore session-id))
  (multiple-value-bind (words flags) (parse-args args)
    (let ((note (or (getf flags :note)
                    (and (rest words) (format nil "~{~a~^ ~}" (rest words))))))
      (rate (or (first words) "")
            :route-id (getf flags :route-id)
            :lane (getf flags :lane)
            :note note))))

;;; --- the tool ----------------------------------------------------------------------

(nlk:define-contract route-request
  (:doc "Route a task to the right (model, reasoning effort) arm before starting it. Keyword classification runs here on this machine: it costs no tokens and sends nothing anywhere. Answers a route card -- the lane, the model and effort it starts at, why it was picked, its route id, and how that lane has measured -- and records the route so /rate pass|fail can label it afterwards. Advisory only: it never switches the model, so read the card and set the arm yourself.")
  ("task" :text :required t
          :doc "the task in the user's words, as much of it as they typed")
  ("lane" :string
          :doc "a lane id to pin instead of classifying; /route lanes names them"))

(defun route-tool (request)
  "The evalroute_route tool: one card from the model's call, a refusal as the
`ERROR:' line the model reads."
  (handler-case
      (route (route-request-task request) :lane (route-request-lane request))
    (error (condition)
      (nle:failure (format nil "ERROR: evalroute: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the manual --------------------------------------------------------------------

(defparameter +manual+
  "Evalroute routes a task to the right (model, reasoning effort) arm before the work starts, and
keeps a ledger of how each arm did. The classifier is keywords on this machine: no tokens, no
network. A Nodecode rewrite of the Hermes plugin hermes-plugin-evalroute.
  (evalroute:route TASK &key lane)   classify TASK and answer its route card; => (values CARD ROW)
  (evalroute:rate VERDICT &key route-id lane note)   label a route pass, fail or skip
  (evalroute:report &key limit)   every lane's routes, labels and pass rate
  (evalroute:lanes)   the route table: each lane's model, effort and keywords
  (evalroute:status)   the ledger, the pending route, the arms named per lane
/route <task> is the card in chat, /route lanes the table, /rate pass|fail labels the newest
unrated route. The card's `next:' line is the workflow: set /model and /think from it, do the
work, then /rate it -- an unrated route teaches the table nothing. A lane with no model named in
`models' prints the effort and asks for one. Nothing leaves the machine; there is no key."
  "What (help :evalroute) answers while the cell runs.")

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell evalroute
  (:section ("evalroute")
    (:guide "models names the model each lane runs on, as build=<model id>, quick=<model id>; next step: /route <task> prints a card")
    ("models" :string :required t
     :doc "the model each lane runs on, as lane=model pairs, e.g. build=z-ai/glm-5,quick=z-ai/glm-5-air")
    ("data_dir" :path
     :doc "the folder the ledger is kept in; empty is evalroute/ under the Nodecode home")
    ("default_lane" :choice :options '("build" "review" "read" "quick" "plan") :default "read"
     :doc "the lane a task with no keyword hit goes to, at confidence 0")
    ("min_hits" :integer :default 1 :min 1
     :doc "how many keyword hits a lane needs before it is picked over the default")
    ("max_routes" :integer :default 200 :min 1
     :doc "how many routes the ledger keeps, newest first"))
  (:help :evalroute "Evalroute: route a task to a model and an effort - (evalroute:route \"task\"), /route <task>, /rate pass|fail, (evalroute:report)" +manual+)
  (:tool "evalroute_route" (nlk:contract 'route-request) 'route-tool)
  (:command "route" 'run-route
            :description "Classify a task and print its (model, effort) route card"
            :argument-hint "[--lane <lane>] <task> | lanes")
  (:command "rate" 'run-rate
            :description "Label the newest unrated route pass, fail or skip"
            :argument-hint "pass|fail|skip [--route-id <id>] [--lane <lane>] [--note <text>]")
  (:start (lambda ()
            ;; A card is only half its value without the ledger the label lands
            ;; in: the folder is made at start, and its failure is a refusal.
            ;; A card names an arm, and an arm is a model beside an effort:
            ;; a section that names no model refuses here, naming the member
            ;; the way the setup panel would.
            (unless (plusp (length (or (setting :models) "")))
              (error 'nlk:config-refusal
                     :detail "evalroute: models names the model each lane runs on, as build=<model id>, quick=<model id>; set it in the evalroute section of config.jsonc or in /settings"))
            (handler-case (ensure-directories-exist (ledger-file))
              (error (condition)
                (fail "evalroute: the ledger folder cannot be made: ~a"
                      (nlk:one-line (princ-to-string condition))))))))
