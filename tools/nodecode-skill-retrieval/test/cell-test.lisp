;;;; cell-test.lisp --- lifecycle, the tool, the round and /skill-search.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here, offline, against a library of SKILL.md files written
;;;; into a scratch folder: that a start installs the tool, /skill-search, the
;;;; manual and the round hook and a stop takes every one of them back; that a
;;;; section without roots refuses; that a ranking puts the skill whose
;;;; description and triggers fit first and leaves the unrelated one out; that
;;;; the command answers the same ranking; and that a turn's own input is
;;;; injected into its own request once, not twice, and not at all when inject
;;;; is off.

(in-package #:nodecode.test)

(define-test-slice "skill-retrieval" "SKILL-RETRIEVAL-CELL-" :start nodecode-skill-retrieval:start-cell)

;;; --- the library ---------------------------------------------------------------

(defun skill-md (dir id name description &key triggers tags)
  "Write a SKILL.md under DIR at ID and answer the folder, as a string."
  (let* ((folder (merge-pathnames (format nil "~a/" id) dir))
         (file (merge-pathnames "SKILL.md" folder)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (format out "---~%name: ~a~%description: ~a~@[~%triggers: [~{~a~^, ~}]~]~@[~%tags: [~{~a~^, ~}]~]~%---~%~%# ~a~%~%The steps.~%"
              name description triggers tags name))
    (namestring folder)))

(defmacro with-skill-library ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS over a scratch library of four skills."
  `(with-temp-directory (dir "skills")
     (skill-md dir "deploy" "Deploy a service"
               "Ship a docker container to production with one command."
               :triggers '("release" "rollout"))
     (skill-md dir "devops/logs" "Read the logs"
               "Find the error in a service's log lines.")
     (skill-md dir "braise" "Braise a stew"
               "Cook root vegetables slowly in a covered pot."
               :tags '("kitchen"))
     (with-cell-stop ((skill-retrieval-start "roots" (vector (namestring dir)) ,@pairs))
       ,@body)))

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "skill-retrieval"
  (:hooks :round)
  (:help :skill-retrieval)
  (:command "skill-search")
  (:config "roots" (vector "/nonexistent-skill-library"))
  (:running (is (nle::find-tool "skill_search") "the search tool is registered")
            (is (= 6 (nodecode-skill-retrieval::setting :top-k)) "top_k defaults to six")
            (is (nodecode-skill-retrieval::setting :inject) "injection is on by default"))
  (:refused ("top_k" 0) ("roots" 5))
  (:stopped (is (null (nle::find-tool "skill_search")) "stop unregisters the tool"))
  (:idle nodecode-skill-retrieval::skill-retrieval-error
         (nodecode-skill-retrieval:search-skills "anything")))

(deftest skill-retrieval-cell-a-section-without-roots-refuses ()
  (is (signals-error nlk:config-refusal (skill-retrieval-start)) "roots is required")
  (is (signals-error nlk:config-refusal (skill-retrieval-start "top_k" 3)) "and nothing else stands in for it")
  (is (null (nle::find-tool "skill_search")) "a refused start installs nothing"))

;;; --- the ranking ---------------------------------------------------------------

(defun skill-answer (query &optional top-k)
  "The tool's answer to QUERY, as the model reads it."
  (nodecode-skill-retrieval::skill-search-tool
   (nodecode-skill-retrieval::make-skill-search :query query :top-k top-k)))

(deftest skill-retrieval-cell-the-tool-ranks-the-files-under-the-roots ()
  (with-skill-library ()
    (let ((answer (skill-answer "ship the docker container to production")))
      (is (search "Deploy a service" answer) "the description that fits is answered")
      (is (search "(deploy)" answer) "with the folder that holds its steps")
      (is (not (search "Braise a stew" answer)) "an unrelated skill is left out")
      (is (< (search "Deploy a service" answer) (or (search "Read the logs" answer) most-positive-fixnum)) "best first")))
  (with-skill-library ()
    (is (search "Read the logs" (skill-answer "what did the service log")) "a nested SKILL.md is indexed by its folder")
    (is (search "(devops/logs)" (skill-answer "what did the service log")) "its id is the folder below the root")))

(deftest skill-retrieval-cell-triggers-and-tags-are-indexed-but-not-shown ()
  (with-skill-library ()
    (let ((answer (skill-answer "rollout")))
      (is (search "Deploy a service" answer) "a trigger phrase reaches a skill its description does not")
      (is (not (search "triggers:" answer)) "and the phrases themselves are never shown")
      (is (search "Ship a docker container" answer) "only the description is"))
    (is (search "Braise a stew" (skill-answer "kitchen")) "a tag reaches one too"))
  (with-skill-library ("triggers" nil)
    (is (not (search "Deploy a service" (skill-answer "rollout"))) "with triggers off, only name and description are indexed")))

(deftest skill-retrieval-cell-top-k-bounds-one-answer ()
  (with-skill-library ("top_k" 1)
    (is (search "best 1 for" (skill-answer "docker container log lines service")) "the section's top_k bounds one answer")))

(deftest skill-retrieval-cell-the-command-answers-the-same-ranking ()
  (with-skill-library ()
    (let ((answer (cell-entry "nodecode-skill-retrieval" "skill-search" "log lines" nil)))
      (is (search "Read the logs" answer) "the command ranks the words it is given"))
    (let ((answer (cell-entry "nodecode-skill-retrieval" "skill-search" "" nil)))
      (is (search "3 skills indexed" answer) "typed alone it says what the roots hold"))))

;;; --- the round -----------------------------------------------------------------

(defun skill-turn (session input)
  "A turn admitted on SESSION with INPUT, as the gateway admits one."
  (let ((command (nlk:make-durable-id "command")))
    (nlk:admit-turn session command input
                    :invocation (nlk:default-invocation "turn.start" command
                                                         :source "test" :id "skill-1"))))

(defun skill-told (session)
  "Every reminder text SESSION's turns were told, oldest first."
  (loop for payload in (nlk:events :session-id session :kind "turn.context.prompt" :as :payloads)
        for text = (nlk:json-value payload :string "text")
        when text collect text))

(deftest skill-retrieval-cell-a-turn-carries-its-best-matches-once ()
  (with-skill-library ()
    (with-temp-store ()
      (ensure-durable-session "skill-a")
      (let ((turn (skill-turn "skill-a" "ship the container to production")))
        (nodecode-skill-retrieval::on-round (list :session-id "skill-a" :turn turn) (function identity))
        (nodecode-skill-retrieval::on-round (list :session-id "skill-a" :turn turn) (function identity))
        (let ((told (skill-told "skill-a")))
          (is (= 1 (length told)) "one block per turn, however many rounds ask")
          (is (search "## Retrieved skills" (first told)) "under the block's own head")
          (is (search "Deploy a service" (first told)) "carrying the skill that fits the input")
          (is (not (search "Braise a stew" (first told))) "and not the one that does not"))))
    (with-temp-store ()
      (ensure-durable-session "skill-b")
      (let ((turn (skill-turn "skill-b" "braise the vegetables")))
        (nodecode-skill-retrieval::on-round (list :session-id "skill-b" :turn turn) (function identity))
        (is (search "Braise a stew" (first (skill-told "skill-b"))) "a different input, a different skill")))
    )) 

(deftest skill-retrieval-cell-inject-off-leaves-the-request-alone ()
  (with-skill-library ("inject" nil)
    (with-temp-store ()
      (ensure-durable-session "skill-d")
      (let ((turn (skill-turn "skill-d" "ship the container to production")))
        (nodecode-skill-retrieval::on-round (list :session-id "skill-d" :turn turn) (function identity))
        (is (null (skill-told "skill-d")) "nothing is recorded when inject is off")))))
