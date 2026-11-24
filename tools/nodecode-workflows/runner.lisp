;;;; runner.lisp --- one worker advances every live run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; TICK is the whole engine: poll the nodes whose turn is in flight,
;;;; expand the fan-outs whose ancestors committed, start what is ready, skip
;;;; what stands behind a failure, and settle a run with nothing left to do.
;;;; Each mutation lands on disk as it happens, so the folder is the truth
;;;; and a run outlives the process: at the next boot a run still marked
;;;; running is expanded again, and every node that committed is skipped.
;;;;
;;;; A node that is an agent runs its prompt in a child session of its own,
;;;; so nothing it does touches the session that started the run; when the
;;;; run ends the session that started it is woken with one line.

(in-package #:nodecode-workflows)

(defvar *launch-function* nil
  "The seam that starts one node's turn; a test replaces it. => (values SESSION TURN).")

(defvar *poll-function* nil
  "The seam that reads one node's turn; a test replaces it. => (values :RUNNING),
(values :DONE TEXT), (values :FAILED DETAIL) or (values :LOST).")

;;; --- where a node's turn stands --------------------------------------------------

(defun newest-turn (session)
  "The newest turn SESSION was admitted, or NIL."
  (when (and (stringp session) (nlk:store-open-p))
    (nlk:events :session-id session :kind "turn.started" :order :newest :limit 1
                :as :value :columns '("turn_id"))))

(defun session-id-for (run node-id)
  "The child session node NODE-ID of RUN runs in."
  (format nil "wf/~a/~a" (wrun-id run) (substitute #\- #\. node-id)))

(defun launch-node (run node)
  "Start NODE's one turn in a session of its own. => (values SESSION TURN)."
  (let* ((session (session-id-for run (node-id node)))
         (origin (wrun-origin run))
         (origin-live (and origin (nlk:store-open-p) (nlk:session-exists-p origin))))
    (unless (and (nlk:store-open-p) (nlk:session-exists-p session))
      (if origin-live
          (nlk:create-session :id session :cwd (nlk:find-session-cwd origin)
                                     :parent origin :detached t)
          (nlk:create-session :id session)))
    (nle:submit session (prompt-for run node)
                :source "workflow" :source-id (format nil "~a/~a" (wrun-id run) (node-id node)))
    (values session (newest-turn session))))

(defun poll-node (run node)
  "Where NODE's turn stands, read off its session's log."
  (let* ((table (node-table run (node-id node)))
         (turn (nlk:json-value table :string "turn"))
         (session (nlk:json-value table :string "session"))
         (kind (and turn (ignore-errors (nlk:turn-terminal-kind turn)))))
    (cond ((null session) (values :lost))
          ((null turn)
           (if (nlk:active-turn-p session) (values :running) (values :lost)))
          ((null kind) (values :running))
          ((string= kind "turn.completed")
           (values :done (or (nlk:turn-answer session turn) "")))
          (t (values :failed
                     (format nil "its turn ~a"
                             (if (string= kind "turn.cancelled") "was cancelled" "failed")))))))

;;; --- what a node's prompt is -----------------------------------------------------

(defun json-part (text)
  "TEXT's JSON object: the text itself, or the one a fenced block carries."
  (when (and (stringp text) (plusp (length text)))
    (flet ((decode (string)
             (let ((value (ignore-errors (nlk:decode-json string))))
               (and (hash-table-p value) value))))
      (or (decode (string-trim '(#\Space #\Newline #\Tab #\Return) text))
          (let* ((open (search "```" text))
                 (start (and open (position #\Newline text :start (1+ open))))
                 (end (and start (search "```" text :start2 (1+ start)))))
            (and end (decode (string-trim '(#\Space #\Newline)
                                          (subseq text (1+ start) end)))))))))

(defun split-on (character text)
  (let ((parts '())
        (start 0))
    (loop
      (let ((at (position character text :start start)))
        (push (subseq text start at) parts)
        (unless at (return (nreverse parts)))
        (setf start (1+ at))))))

(defun ancestor-json (run id)
  "The JSON object node ID committed, or NIL."
  (json-part (run-output run id)))

(defun ancestor-value (run reference)
  "REFERENCE, \"<node>.<path>\", read off what that node committed. => (values VALUE PRESENT)."
  (let* ((at (position #\. reference))
         (id (if at (subseq reference 0 at) reference))
         (path (and at (subseq reference (1+ at))))
         (object (ancestor-json run id)))
    (if (null path)
        (values object (not (null object)))
        (let ((value object))
          (dolist (key (split-on #\. path))
            (setf value (nlk:json-value value :any key)))
          (values value (not (null value)))))))

(defun encode (value)
  (nlk:encode-json-object value))

(defun prompt-for (run node)
  "What NODE is asked: its goal, its item when it is one lane of a fan-out,
and every input it stands on."
  (let* ((parent (and (node-parent node) (node-by-id (wrun-graph run) (node-parent node))))
         (definition (or parent node))
         (limit (setting :output-chars)))
    (with-output-to-string (out)
      (format out "[workflow ~a, node ~a]~%~a~%" (wrun-id run) (node-id node) (node-goal definition))
      (when (node-parent node)
        (format out "~%One lane of the fan-out ~s, over this item:~%~a~%"
                (node-parent node) (encode (nlk:json-value (node-table run (node-id node)) :any "item"))))
      (let ((edges (node-after definition)))
        (when edges
          (format out "~%## Inputs~%")
          (dolist (edge edges)
            (format out "~%### ~a~%~a~%" edge
                    (nlk:clip (or (run-output run edge) "(it committed nothing)") limit)))))
      (dolist (reference (node-inputs definition))
        (multiple-value-bind (value present) (ancestor-value run reference)
          (format out "~%### ~a~%~a~%" reference
                  (if present (nlk:clip (encode value) limit) "(nothing at that path)"))))
      (when (node-from definition)
        (format out "~%## The fan-out ~s, whole~%~a~%" (node-from definition)
                (nlk:clip (encode (coerce (loop for id in (dep-ids run (node-from definition))
                                                for text = (or (run-output run id) "")
                                                collect (or (json-part text) text))
                                          'vector))
                          limit)))
      (when (node-schema definition)
        (format out "~%Answer with one fenced JSON object shaped like:~%~a~%"
                (encode (node-schema definition))))
      (format out "~%Your answer is this node's output: lead with the result. Nothing you do reaches ~
the session that started the run until every node it waits on has committed.~%"))))

;;; --- one node's transition -------------------------------------------------------

(defun commit (run id text)
  "Node ID committed TEXT: it is done, and its output is on disk."
  (set-output run id text)
  (set-node-state run id "done")
  id)

(defun fail-node (run id detail)
  (set-node-state run id "failed" :error detail)
  id)

(defun start-node (run node)
  "Start NODE: an echo commits, a gate holds, an agent gets a turn."
  (let ((id (node-id node)))
    (ecase (node-type node)
      (:echo (commit run id (node-echo node)))
      (:gate (set-node-state run id "held"))
      (:agent
       (handler-case
           (multiple-value-bind (session turn)
               (funcall (or *launch-function* #'launch-node) run node)
             (set-node-state run id "running" :session session :turn turn))
         (workflows-error (condition) (fail-node run id (princ-to-string condition)))
         (error (condition)
           (fail-node run id (format nil "its turn never started: ~a" (princ-to-string condition)))))))))

(defun timeout-seconds (run node)
  "How long NODE of RUN may run."
  (or (node-budget node) (node-budget (or (and (node-parent node) (node-def run (node-parent node))) node))
      (setting :node-timeout-seconds)))

(defun timed-out-p (run node)
  (let* ((started (node-field run (node-id node) "started_at"))
         (then (and started (nlk:iso-universal started))))
    (and then (> (- (get-universal-time) then) (timeout-seconds run node)))))

(defun poll-nodes (run)
  "Read every node whose turn is in flight; commit what answered."
  (dolist (id (node-ids run))
    (when (string= (node-state run id) "running")
      (let ((node (node-def run id)))
        (cond
          ((timed-out-p run node)
           (fail-node run id (format nil "it ran past its ~d seconds" (timeout-seconds run node))))
          (t
           (multiple-value-bind (state text)
               (funcall (or *poll-function* #'poll-node) run node)
             (case state
               (:running nil)
               (:done (commit run id text))
               (:failed (fail-node run id (or text "its turn failed")))
               (:lost (fail-node run id "its turn left no trace in the log"))))))))))

;;; --- fan-outs -------------------------------------------------------------------

(defun fan-items (run node)
  "The items NODE fans out over, as a vector, or a refusal."
  (if (node-items node)
      (node-items node)
      (multiple-value-bind (value present) (ancestor-value run (node-items-from node))
        (cond ((not present)
               (fail "node ~s fans out over ~s, which names nothing an ancestor committed"
                     (node-id node) (node-items-from node)))
              ((vectorp value) value)
              (t (fail "node ~s fans out over ~s, and that is not a JSON array"
                       (node-id node) (node-items-from node)))))))

(defun expand-fanouts (run)
  "Give every ready fan-out its instances, one node an item."
  (dolist (node (coerce (graph-nodes (wrun-graph run)) 'list))
    (when (and (or (node-items node) (node-items-from node))
               (not (gethash (node-id node) (wrun-expanded run)))
               (deps-done-p run node))
      (handler-case
          (let ((items (fan-items run node)))
            (if (zerop (length items))
                (progn (setf (gethash (node-id node) (wrun-expanded run)) '())
                       (set-node-state run (node-id node) "skipped" :error "it fanned out over no items"))
                (let ((ids (loop for item across items
                                 for position from 1
                                 for id = (format nil "~a.~d" (node-id node) position)
                                 collect id)))
                  (setf (gethash (node-id node) (wrun-expanded run)) ids)
                  (loop for id in ids
                        for item across items
                        do (setf (gethash id (wrun-instances run))
                                 (copy-node node :id id :parent (node-id node)
                                                 :items nil :items-from nil))
                           (setf (gethash "item" (node-table run id)) item))
                  (set-node-state run (node-id node) "expanded"))))
        (workflows-error (condition)
          (fail-node run (node-id node) (princ-to-string condition)))))))

;;; --- readiness ------------------------------------------------------------------

(defun deps-done-p (run node)
  "Every ancestor NODE waits on has committed (or been skipped): its
after-edges, and the fan-out it consolidates."
  (every (lambda (dep)
           (every (lambda (id)
                    (member (node-state run id) '("done" "skipped") :test #'string=))
                  (dep-ids run dep)))
         (node-numbers node)))

(defun running-count (run)
  (loop for id in (node-ids run)
        count (string= (node-state run id) "running")))

(defun launch-ready (run)
  "Start every pending node whose ancestors committed, up to MAX-PARALLEL."
  (let ((budget (setting :max-parallel)))
    (dolist (id (node-ids run))
      (let ((node (node-def run id)))
        (when (and node
                   (< (running-count run) budget)
                   (string= (node-state run id) "pending")
                   (null (or (node-items node) (node-items-from node)))
                   (deps-done-p run node))
          (start-node run node))))))

(defun cascade (run)
  "Skip every pending node that stands behind a failure, to a fixpoint."
  (loop
    (let ((changed nil))
      (dolist (node (coerce (graph-nodes (wrun-graph run)) 'list))
        (when (string= (node-state run (node-id node)) "pending")
          (let ((dead (loop for dep in (node-numbers node)
                            thereis (find-if (lambda (id)
                                               (member (node-state run id) '("failed" "skipped")
                                                       :test #'string=))
                                             (dep-ids run dep)))))
            (when dead
              (set-node-state run (node-id node) "skipped"
                              :error (format nil "after ~a, which never committed" dead))
              (setf changed t)))))
      (unless changed (return)))))

;;; --- the end --------------------------------------------------------------------

(defun waiting-state-p (state)
  (member state '("pending" "running" "held") :test #'string=))

(defun settle (run)
  "A run with nothing left to do gets its verdict and wakes who started it."
  (when (and (string= (wrun-status run) "running")
             (notany (lambda (id) (waiting-state-p (node-state run id))) (node-ids run)))
    (let* ((ids (node-ids run))
           (done (count-if (lambda (id) (string= (node-state run id) "done")) ids))
           (failed (count-if (lambda (id) (string= (node-state run id) "failed")) ids))
           (status (cond ((and (zerop failed) (plusp done)) "done")
                         ((plusp done) "partial")
                         (t "failed"))))
      (setf (wrun-status run) status)
      (save-run run)
      (wake-origin run)
      status)))

(defun wake-text (run)
  (let ((failed (loop for id in (node-ids run)
                      when (string= (node-state run id) "failed")
                        collect (format nil "~a (~a)" id (or (node-field run id "error") "no detail")))))
    (format nil "[workflow ~a ~a: ~d of ~d nodes committed~@[; failed: ~{~a~^, ~}~]. ~
Every output is under the run's folder; workflow{action:\"status\",run_id:\"~a\"} reads the whole run, ~
and workflow{action:\"show\",run_id:\"~a\",node_id:\"<node>\"} reads one node's output.]"
            (wrun-id run) (wrun-status run)
            (count-if (lambda (id) (string= (node-state run id) "done")) (node-ids run))
            (length (node-ids run)) failed (wrun-id run) (wrun-id run))))

(defun wake-origin (run)
  "Wake the session that started RUN once, with what the run came to."
  (let ((origin (wrun-origin run)))
    (when (and origin (not (wrun-woke run)) (nlk:store-open-p)
               (nlk:session-exists-p origin))
      (setf (wrun-woke run) t)
      (handler-case
          (nle:submit origin (wake-text run) :source "workflow" :source-id (wrun-id run))
        (error () nil)))))

;;; --- the tick -------------------------------------------------------------------

(defun advance (run)
  "One turn of the engine over RUN, from polling to the verdict."
  (when (string= (wrun-status run) "running")
    (poll-nodes run)
    (expand-fanouts run)
    (launch-ready run)
    (cascade run)
    (settle run)
    (save-run run))
  run)

(defun tick ()
  "The worker's step: every live run advanced, oldest first, under the lock."
  (when (nlk:store-open-p)
    (with-workflows-lock
      (dolist (run (reverse (copy-list *runs*)))
        (when (string= (wrun-status run) "running")
          (handler-case (advance run)
            (error (condition) (warn "workflow ~a: ~a" (wrun-id run) condition)))))))
  nil)
