;;;; cell-test.lisp --- lifecycle, the verbs, one judgment, one plan, one apply.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything here is offline: a SKILL.md library written into a scratch
;;;; folder, and every exchange with the judgment endpoint stubbed at
;;;; DEX:REQUEST (under NLK:HTTP). What is proved: that a start installs the
;;;; tool, /curator and the manual and a stop takes them back; that a section
;;;; without roots refuses; that the inventory reads front matter and
;;;; digests; that the candidates rank an overlapping pair first and leave an
;;;; unrelated skill out; that a judgment posts the versioned question set
;;;; and folds the answer into the evidence policy's terms; that nothing is
;;;; asked without egress; that a duplicate with full coverage and
;;;; preservation becomes a validated star plan whose id is content; and that
;;;; an apply, only in mode apply and only at the judged digests, moves the
;;;; absorbed folder into the archive instead of deleting it.

(in-package #:nodecode.test)

(define-test-slice "jev-curator" "JEV-CURATOR-CELL-" :start nodecode-jev-curator:start-cell)

;;; --- the library -----------------------------------------------------------------

(defun curator-skill (dir id name description steps)
  "Write a SKILL.md under DIR at ID and answer the folder, as a string."
  (let* ((folder (merge-pathnames (format nil "~a/" id) dir))
         (file (merge-pathnames "SKILL.md" folder)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (format out "---~%name: ~a~%description: ~a~%---~%~%# ~a~%~%~a~%" name description name steps))
    (namestring folder)))

(defmacro with-library ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS over a scratch library of three skills:
two that overlap, one that does not."
  `(with-temp-directory (dir "skills")
     (curator-skill dir "deploy" "deploy" "Ship a service with one command."
                     "Run scripts/deploy.sh and watch the rollout finish.")
     (curator-skill dir "deploy-logs" "deploy-logs" "Ship a service and read its logs."
                     "Run scripts/deploy.sh, then tail the service's log lines for errors.")
     (curator-skill dir "braise" "braise" "Cook vegetables slowly in a covered pot."
                     "Sear the vegetables, add stock, cover the pot for two hours.")
     (with-cell-stop ((jev-curator-start "roots" (vector (namestring dir)) ,@pairs))
       ,@body)))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "jev-curator"
  (:help :jev-curator)
  (:command "curator")
  (:config "roots" (vector "/nonexistent-skill-library"))
  (:running (is (nle::find-tool "skill_relations") "the tool is registered")
            (is (string= "observe" (nodecode-jev-curator::mode)) "mode defaults to observe")
            (is (null (nodecode-jev-curator::egress-enabled-p)) "egress is off by default"))
  (:refused ("top_k" 0) ("roots" 5) ("mode" "sideways"))
  (:stopped (is (null (nle::find-tool "skill_relations")) "stop unregisters the tool"))
  (:idle nodecode-jev-curator::jev-curator-error
         (nodecode-jev-curator:inventory)
         (nodecode-jev-curator:candidates)
         (nodecode-jev-curator:plans)))

(deftest jev-curator-cell-a-section-without-roots-refuses ()
  (is (signals-error nlk:config-refusal (jev-curator-start)) "roots is required")
  (is (signals-error nlk:config-refusal (jev-curator-start "mode" "observe")) "and nothing stands in for it"))

;;; --- what the judgment endpoint answers, stubbed -------------------------------------

(defun jev-answer (relation &key (confidence 0.9) (coverage 0.9) (a-in-b 0.95) (b-in-a 0.95) (conflict 0.02))
  "One System One answer object naming RELATION, as the transport would read it."
  (format nil "{\"model\":\"jev-test\",\"answers\":{\"relation\":{\"type\":\"choice\",\"choice\":~s,\"probabilities\":{~s:~,2f},\"confidence\":~,2f},\"coverage\":{\"type\":\"noul\",\"noul\":~,2f},\"a_in_b\":{\"type\":\"noul\",\"noul\":~,2f},\"b_in_a\":{\"type\":\"noul\",\"noul\":~,2f},\"conflict\":{\"type\":\"noul\",\"noul\":~,2f}}},\"usage\":{}}"
          relation relation confidence confidence coverage a-in-b b-in-a conflict))

(defvar *jev-requests* '()
  "Every request the stubbed transport was sent, oldest first.")

(defmacro with-jev-server ((&key (relation "duplicate") (status 200)
                                (env '(("TYPESAFE_API_KEY" . "ts-env"))) (auth "{}"))
                           &body body)
  "BODY with AUTH as the auth file, ENV as the environment, and the judgment
endpoint answering one JE­V-ANSWER for RELATION with STATUS."
  `(with-temp-auth (auth ,auth)
     (let ((nle::*auth-file-path* auth)
           (*jev-requests* '()))
       (with-stubbed-fdefinitions
           ((nle::credential-env (name) (cdr (assoc name ',env :test #'equal)))
            (dex:request (url &rest args)
                         (push (list (getf args :method) url (getf args :headers)
                                     (getf args :content))
                               *jev-requests*)
                         (values (jev-answer ,relation) ,status (make-hash-table :test #'equal))))
         ,@body))))

;;; --- the inventory and the candidates -------------------------------------------------

(deftest jev-curator-cell-the-inventory-reads-front-matter ()
  (with-library ()
    (let ((skills (nodecode-jev-curator::collect-inventory)))
      (is (= 3 (length skills)) "three SKILL.md files, one skill each")
      (is (equal '("braise" "deploy" "deploy-logs") (mapcar #'nodecode-jev-curator::skill-name skills))
          "sorted by name")
      (is (equal "Ship a service with one command."
                 (nodecode-jev-curator::skill-description
                  (nodecode-jev-curator::skill-by-name skills "deploy")))
          "the description is the front matter's")
      (is (uiop:string-prefix-p "sha256:" (nodecode-jev-curator::skill-digest (first skills)))
          "an sha256 digests every skill"))
    (is (search "deploy  " (nodecode-jev-curator:inventory)) "the report lists the skills")))

(deftest jev-curator-cell-the-candidates-rank-the-overlapping-pair ()
  (with-library ()
    (let ((text (nodecode-jev-curator:candidates)))
      (is (search "deploy deploy-logs" text) "the two deploy skills are a pair")
      (is (search "name-prefix" text) "and the shared first word is why")
      (is (not (search "braise" text)) "an unrelated skill is left out"))
    (let* ((skills (nodecode-jev-curator::collect-inventory))
           (pairs (nodecode-jev-curator::candidate-pairs skills :top-k 1 :max-pairs 10)))
      (is (= 1 (length pairs)) "one pair, once")
      (is (string= "deploy" (nodecode-jev-curator::candidate-a (first pairs))) "a is the first name")
      (is (realp (nodecode-jev-curator::candidate-similarity (first pairs))) "with a score"))))

;;; --- one judgment ---------------------------------------------------------------------

(deftest jev-curator-cell-a-judgment-posts-the-contract-and-folds-it ()
  (with-library ("mode" "observe" "egress" t)
    (with-jev-server ()
      (let ((judgment (nodecode-jev-curator:judge "deploy" "deploy-logs")))
        (destructuring-bind (method url headers content) (first *jev-requests*)
          (is (eq :post method) "one POST")
          (is (equal "https://api.typesafe.ai/v1/systemone" url) "to TypeSafe's judgment route")
          (is (equal "Bearer ts-env" (cdr (assoc "authorization" headers :test #'string-equal)))
              "with the key")
          (let* ((body (nlk:decode-json (if (stringp content) content (nlk:body-text content))))
                 (questions (nlk:json-value body :object "questions"))
                 (state (nlk:json-value body :object "state")))
            (is (equal "jev-latest" (nlk:json-value body :string "model")) "the default model")
            (is (equal "choice" (nlk:json-value questions :string "relation" "type")) "the relation choice")
            (is (= 8 (hash-table-count (nlk:json-value questions :object "relation" "criteria")))
                "whose eight options carry their meanings")
            (is (equal "noul" (nlk:json-value questions :string "a_in_b" "type")) "and the preservation questions")
            (is (equal "deploy" (nlk:json-value state :string "skill_a" "name")) "both skills go along")
            (is (equal "skill-relations-v3" (nlk:json-value state :string "contract")) "under the contract's version")))
        (is (equal "duplicate" (getf judgment :relation)) "the answer is the judgment")
        (is (= 1 (length *jev-requests*)) "one request, one pair")))))

(deftest jev-curator-cell-nothing-is-asked-without-egress ()
  (with-library ()
    (is (search "egress" (refusal-text nodecode-jev-curator:jev-curator-error
                                        (nodecode-jev-curator:judge "deploy" "deploy-logs")))
        "a judgment without egress names the consent it lacks")
    (is (search "noop" (nodecode-jev-curator:plans)) "and a scan without judgments plans nothing")
    (is (null *jev-requests*) "nothing was sent")))

;;; --- plans -----------------------------------------------------------------------------

(defun only-plan ()
  "The one plan the last scan built."
  (let ((plans '()))
    (maphash (lambda (id plan) (declare (ignore id)) (push plan plans))
             nodecode-jev-curator::*plans*)
    (first plans)))

(deftest jev-curator-cell-a-duplicate-becomes-a-validated-plan ()
  (with-library ("mode" "observe" "egress" t)
    (with-jev-server ()
      (let ((text (nodecode-jev-curator:plans)))
        (is (search "validated" text) "the duplicate is a validated plan")
        (is (search "keep \"deploy\"" text) "keeping the first name")
        (is (search "deploy-logs" text) "and absorbing the other")
        (let ((plan (only-plan)))
          (is (search "merge-" (nodecode-jev-curator::merge-plan-id plan)) "a content-hash id")
          (is (equal "validated" (nodecode-jev-curator::merge-plan-status plan)) "validated")
          (is (equal '("deploy-logs") (nodecode-jev-curator::merge-plan-absorbed plan)) "one member absorbed")
          (is (uiop:string-prefix-p "sha256:" (nodecode-jev-curator::merge-plan-canonical-digest plan))
              "bound to the digest it judged"))))))

(deftest jev-curator-cell-a-protected-skill-is-never-proposed ()
  (with-library ("mode" "observe" "egress" t "protected" (vector "deploy-logs"))
    (with-jev-server ()
      (is (not (search "deploy-logs" (nodecode-jev-curator:candidates)))
          "a protected skill is left out of the pairs a scan would judge")
      (let* ((skills (nodecode-jev-curator::collect-inventory))
             (by-name (make-hash-table :test #'equal)))
        (dolist (skill skills) (setf (gethash (nodecode-jev-curator::skill-name skill) by-name) skill))
        (let* ((judgment (nodecode-jev-curator::judge-pair
                           (nodecode-jev-curator::skill-by-name skills "deploy")
                           (nodecode-jev-curator::skill-by-name skills "deploy-logs")))
               (edge (nodecode-jev-curator::evaluate-judgment judgment by-name '("deploy-logs"))))
          (is (null (nodecode-jev-curator::edge-authorized edge)) "a protected member is never absorbed")
          (is (member "protected:deploy-logs" (nodecode-jev-curator::edge-refusals edge)
                      :test #'string=)
              "and the refusal names it"))))))

;;; --- apply -----------------------------------------------------------------------------

(deftest jev-curator-cell-an-apply-moves-the-folder-into-the-archive ()
  (with-temp-directory (dir "skills")
    (curator-skill dir "deploy" "deploy" "Ship a service with one command."
                    "Run scripts/deploy.sh and watch the rollout finish.")
    (curator-skill dir "deploy-logs" "deploy-logs" "Ship a service and read its logs."
                    "Run scripts/deploy.sh, then tail the service's log lines for errors.")
    (with-cell-stop ((jev-curator-start "roots" (vector (namestring dir)) "mode" "observe" "egress" t))
      (with-jev-server ()
        (ignore-errors (nodecode-jev-curator:plans))
        (is (search "mode: apply"
                    (refusal-text nodecode-jev-curator:jev-curator-error
                                  (nodecode-jev-curator:apply-plan
                                   (nodecode-jev-curator::merge-plan-id (only-plan)))))
            "observe refuses to apply")))
    (with-cell-stop ((jev-curator-start "roots" (vector (namestring dir)) "mode" "apply" "egress" t))
      (with-jev-server ()
        (nodecode-jev-curator:plans)
        (let ((text (nodecode-jev-curator:apply-plan
                     (nodecode-jev-curator::merge-plan-id (only-plan)))))
          (is (search "archived" text) "the apply says what moved")
          (is (null (uiop:directory-exists-p (merge-pathnames "deploy-logs/" dir)))
              "the absorbed folder left the library")
          (is (uiop:directory-exists-p (merge-pathnames "deploy/" dir))
              "the canonical one stayed")
          (let ((archive (nodecode-jev-curator::archive-dir)))
            (is (plusp (length (uiop:subdirectories archive))) "and sits in the archive")))))))
