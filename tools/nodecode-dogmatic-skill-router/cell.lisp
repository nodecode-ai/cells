;;;; cell.lisp --- the section, the manual, /dogmatic-skill-router, and the turn's one line.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The Hermes plugin rode one pre_llm_call hook: given the user's message
;;;; it asked TypeSafe's Jev which of the installed skills fits, and put one
;;;; <skill_relevance> line into the turn's context. Nodecode has the same
;;;; two seams by name: NLE:SUBMIT is the one in-process ingress (the prompt
;;;; is in hand, before any turn reads its sections), and
;;;; NLE::READ-HARNESS-SECTIONS is where a turn's standing sections are
;;;; read. The first routes and remembers one line per session; the second
;;;; hands it to the request that turn builds. NIL is silence, and a turn
;;;; never waits on a router that is off, keyless, or unwell.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "dogmatic-skill-router": {"mode": "auto", "backend": "auto", ...}
;;;; The key is auth.json's api_keys.typesafe (or .openrouter), else
;;;; TYPESAFE_API_KEY (OPENROUTER_API_KEY, AI_GATEWAY_API_KEY).

(in-package #:nodecode-dogmatic-skill-router)

(defparameter +manual+
  "The skill router asks a judgment model (TypeSafe's Jev, System One) which one of the prose
skills the organism keeps fits a request, before the model call, and names at most one. It runs
on its own: every prompt you send is routed when the section's mode is on (or auto, with a key),
and the turn carries one line naming the skill. It says nothing when nothing fits.
  (dogmatic-skill-router:route \"text\")   one decision on TEXT, now: the skill it names and its
      numbers, or why it named none. It costs one or two judgment calls.
  (dogmatic-skill-router:skills)          the roster it routes over: every prose (define-skill ...) this
      organism keeps, by name, with the one sentence the ranking reads.
  /dogmatic-skill-router                   where it stands: mode, backend, key, roster size, thresholds;
      /dogmatic-skill-router skills | suggest TEXT | check.
The line it adds reads `Relevant to the current request: NAME', and says to ignore it when it
does not fit: the model stays in charge. A slash command, an empty message, a message past
suggest_chars and a turn that already carries a line are left alone.
ERROR: DOGMATIC-SKILL-ROUTER-ERROR names the problem: the cell is not running, or no key answers."
  "What (help :dogmatic-skill-router) answers while the cell runs.")

(defparameter +hint-key+ "skill_relevance"
  "The harness section a routed turn carries: the plugin's own block name.")

(defparameter +hint-seconds+ 900
  "How long a routed line stands for its session before it is stale.")

(defvar *hints* (make-hash-table :test #'equal)
  "Session id -> (TICKS . LINE): the one line that session's next turn is given.")

(defvar *hint-lock* (bt2:make-lock :name "dogmatic-skill-router hints"))

;;; --- the line a turn reads ------------------------------------------------------

(defun hint-line (name)
  "The one line NAME earns: what it is and how to read it, with the caveat
that keeps the model in charge."
  (format nil "Relevant to the current request: ~a. Load it with (help '~a) when it fits; ignore this if it does not fit what the user actually asked for."
          name name))

(defun remember-hint (session line)
  "Stand LINE as SESSION's hint, or clear it when LINE is NIL."
  (bt2:with-lock-held (*hint-lock*)
    (if (and session line)
        (setf (gethash session *hints*) (cons (get-internal-real-time) line))
        (and session (remhash session *hints*)))))

(defun hint-for (session)
  "SESSION's hint while it is fresh, else NIL."
  (when session
    (bt2:with-lock-held (*hint-lock*)
      (let ((entry (gethash session *hints*)))
        (when (and entry
                   (< (- (get-internal-real-time) (car entry))
                      (* +hint-seconds+ internal-time-units-per-second)))
          (cdr entry))))))

(defun rate-turn (session text)
  "Route TEXT and stand (or clear) SESSION's hint. Never signals: a router
that cannot answer must never fail a turn."
  (handler-case
      (let ((backend (unless (equal (setting :mode) "off") (resolve-backend))))
        (remember-hint session
                        (when (and backend (null (skip-reason text)))
                          (let ((result (decide text)))
                            (and result (hint-line (getf result :skill)))))))
    (error () nil))
  t)

(defun advise-submit (next &rest args)
  "NLE:SUBMIT's advice: route the incoming prompt before its turn is admitted.
ARGS are SUBMIT's own — session id, prompt, and keys; a steer is left alone."
  (let ((session (first args))
        (text (second args)))
    (when (and (stringp session) (stringp text) (null (getf (cddr args) :steer)))
      (rate-turn session text))
    (apply next args)))

(defun advise-sections (next &rest args)
  "READ-HARNESS-SECTIONS' advice: the one hint this session was routed to.
ARGS are the function's own — the session id."
  (let ((sections (apply next args))
        (line (hint-for (first args))))
    (if line
        (acons +hint-key+ line (remove +hint-key+ sections :key #'car :test #'equal))
        sections)))

;;; --- what it says it is ------------------------------------------------------------

(defun model-of (backend)
  "The model BACKEND asks, as the section and its variables name it."
  (ecase backend
    (:typesafe (typesafe-model))
    (:openrouter (openrouter-model))
    (:gateway (gateway-model))))

(defun status-text ()
  "Where the router stands, in one line: mode, backend, key, roster."
  (let* ((backend (resolve-backend))
         (where (if backend
                    (format nil "~(~a~) at ~a, model ~a"
                            backend (endpoint backend) (model-of backend))
                    (format nil "no key for backend ~a, so nothing is asked" (setting :backend)))))
    (format nil "dogmatic-skill-router: mode ~a; ~a; ~d skill~:p; gate ~d%, fits ~d%, shortlist ~d"
            (setting :mode) where (length (roster))
            (setting :gate) (setting :fits) (setting :shortlist))))

;;; --- the verbs -----------------------------------------------------------------------

(define-verb route (text)
  "Name the one kept skill that fits TEXT, now, through the judgment model
the section points at. => a line naming the skill and its numbers (gate, fits),
or why it named none: nothing on the roster fits, the turn is skipped (a slash
command, an empty or very long message), or the router is silent."
  (let ((reason (skip-reason text)))
    (cond ((and (equal (setting :mode) "on") (null (resolve-backend)))
           (fail "no ~(~a~) key: make one at ~a, then set TYPESAFE_API_KEY or save it in auth.json under api_keys.typesafe"
                 (setting :backend) +key-page+))
          (reason (format nil "dogmatic-skill-router: ~a, so nothing was asked" reason))
          (t (let ((result (decide text)))
               (if result
                   (format nil "suggested: ~a (gate ~,2f, fits ~,2f, p ~,2f, ~d call~:p)"
                           (getf result :skill) (getf result :gate) (getf result :fits)
                           (getf result :probability) (getf result :calls))
                   (format nil "no suggestion: no skill on the roster of ~d fits, or the router is silent"
                           (length (roster))))))))) 

(define-verb skills ()
  "The roster it routes over: every prose (define-skill ...) this organism
keeps, one line each, name and the sentence the ranking reads."
  (let ((skills (roster)))
    (if (null skills)
        "dogmatic-skill-router: the roster is empty: keep a procedure with (define-skill name \"one sentence\" ...) and it is routed over"
        (format nil "~{~a~^~%~}"
                (loop for skill in skills
                      collect (format nil "~a - ~a" (getf skill :name) (getf skill :description)))))))

(defun panel (summary)
  "The dialog bare /dogmatic-skill-router answers: SUMMARY over the three rows it runs."
  (nle:list-dialog
   :title "Skill router" :summary summary
   :rows (list (list :label "The roster" :run (list :enter "/dogmatic-skill-router skills")
                     :detail "every prose skill it routes over")
               (list :label "Suggest for…" :run (list :enter "/dogmatic-skill-router suggest ")
                     :detail "one live decision on the text typed after it")
               (list :label "Check the key" :run (list :enter "/dogmatic-skill-router check")
                     :detail "ask the judgment model one question"))))

(defun check-text ()
  "Whether the key answers, in one line: one judgment spent."
  (let ((probability (probe-answer)))
    (if probability
        (format nil "dogmatic-skill-router: ~(~a~) answered (P(yes) ~,2f); ~d skill~:p on the roster"
                (resolve-backend) probability (length (roster)))
        (format nil "dogmatic-skill-router: nothing answered: set TYPESAFE_API_KEY (make one at ~a) or save it in auth.json under api_keys.typesafe"
                +key-page+))))

(defun run-slash (args session-id)
  "/dogmatic-skill-router typed alone: where it stands, and its panel; a verb after it
(skills, suggest TEXT, check) runs that."
  (declare (ignore session-id))
  (let* ((words (nlk:trimmed (or args "")))
         (space (position #\Space words))
         (verb (string-downcase (if space (subseq words 0 space) words)))
         (rest (if space (nlk:trimmed (subseq words (1+ space))) "")))
    (cond ((zerop (length verb)) (let ((text (status-text))) (values text (panel text))))
          ((equal verb "skills") (skills))
          ((equal verb "suggest")
           (if (zerop (length rest))
               "dogmatic-skill-router: (suggest) needs the text to route: /dogmatic-skill-router suggest fix the spreadsheet"
               (handler-case (route rest)
                 (dogmatic-skill-router-error (condition) (format nil "~a" condition)))))
          ((equal verb "check") (check-text))
          (t (values (format nil "dogmatic-skill-router: ~a is not one of its verbs (skills, suggest, check)" verb)
                     (panel (status-text)))))))

;;; --- the declaration -----------------------------------------------------------------

(nle:define-cell dogmatic-skill-router
  (:section ("dogmatic-skill-router")
    (:guide "it speaks when the section's mode is on, or auto with a key; make a key at https://console.typesafe.ai/, then set TYPESAFE_API_KEY or save it in auth.json under api_keys.typesafe; an OpenRouter or Vercel AI Gateway key works too (backend picks one); thresholds are percentages, and nothing here is required")
    ("mode" :choice :options '("auto" "on" "off") :default "auto"
     :doc "auto speaks when a key is present; on asks even without one; off never asks")
    ("backend" :choice :options '("auto" "typesafe" "openrouter" "gateway") :default "auto"
     :doc "auto takes the first of typesafe, openrouter, gateway that has a key")
    ("model" :string :default ""
     :doc "the judgment model the typesafe route asks; empty is jev-latest")
    ("base_url" :string :default ""
     :doc "the TypeSafe API root; empty is TYPESAFE_BASE_URL, else https://api.typesafe.ai")
    ("openrouter_model" :string :default ""
     :doc "the model the openrouter route asks; empty is ~typesafe/jev-latest")
    ("openrouter_base_url" :string :default ""
     :doc "OpenRouter's root; empty is https://openrouter.ai/api/alpha")
    ("gateway_model" :string :default ""
     :doc "the model the gateway route asks; empty is typesafe-ai/jev")
    ("gateway_base_url" :string :default ""
     :doc "the gateway root; empty is https://ai-gateway.vercel.sh/v4/ai")
    ("gate" :integer :default 30 :min 0
     :doc "percent: the mean of the three turn judgments below which nothing is named")
    ("fits" :integer :default 40 :min 0
     :doc "percent: the winner's own does-it-fit judgment below which nothing is named")
    ("shortlist" :integer :default 3 :min 1
     :doc "how many candidates the second request re-reads")
    ("excerpt" :integer :default 700 :min 0
     :doc "how much of each candidate's text the second request carries")
    ("suggest_chars" :integer :default 4000 :min 0
     :doc "messages longer than this are left alone")
    ("timeout" :integer :default 4 :min 1
     :doc "seconds one judgment call may take"))
  (:help :dogmatic-skill-router "dogmatic-skill-router:route names the one kept skill that fits a request, before the model call (TypeSafe Jev)" +manual+)
  (:hook 'nle:submit #'advise-submit)
  (:hook 'nle::read-harness-sections #'advise-sections)
  (:command "dogmatic-skill-router" 'run-slash
   :description "Which of your skills fits this turn"
   :session nil)
  (:start (lambda ()
            (clrhash *hints*)
            (when (and (not (equal (setting :mode) "off"))
                       (null (resolve-backend)))
              (nle:notice (format nil "dogmatic-skill-router: no ~(~a~) key yet, so nothing is routed: set TYPESAFE_API_KEY, or pick another backend"
                                   (setting :backend))
                          :level :warning :key "nodecode-dogmatic-skill-router"))
            (nle:on-stop (lambda ()
                           (clrhash *hints*)
                           (nle:notice nil :key "nodecode-dogmatic-skill-router"))))))
