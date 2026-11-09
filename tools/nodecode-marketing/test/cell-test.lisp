;;;; cell-test.lisp --- the marketing library: found by words, read whole or in part.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches the network or the operator's files: the corpus is
;;;; the tree the cell ships, plus a small tree of skills written under the
;;;; temporary folder for the shapes the shipped prose cannot be asked to have
;;;; on demand (a quoted description with an escaped quote, a body over the
;;;; clip). Each tool is called the way a model's call reaches it — through
;;;; the real dispatch, contract and all — so the schema the provider is shown
;;;; and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "marketing" "MARKETING-CELL-" :start nodecode-marketing:start-cell
  :defaults (("max_chars" 6000)))

;;; --- fixtures --------------------------------------------------------------------

(defparameter +long-body+
  (with-output-to-string (out)
    (format out "---~%name: long-post~%description: \"A body longer than the clip. \\\"yes\\\"\"~%---~%~%")
    (format out "# Long post~%~%Quality gates:~%")
    (dotimes (index 12) (format out "- gate ~d of the list, with enough words to pass the clip~%" index))
    (format out "~%## Quality gates~%~%Every claim is checked twice before it ships.~%")
    (format out "~%## Output format~%Markdown, with a title and a summary.~%"))
  "A frontmatter, two sections and a body past six hundred characters.")

(defun marketing-fixture (&rest skills)
  "A skills tree under the temporary folder holding SKILLS, each (GROUP
FOLDER TEXT); answers its truename. The caller deletes it."
  (let ((root (merge-pathnames "marketing-cell-fixture/" (uiop:temporary-directory))))
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
    (ensure-directories-exist root)
    (dolist (skill skills)
      (destructuring-bind (group folder text) skill
        (let ((file (merge-pathnames (format nil "~a/~a/SKILL.md" group folder) root)))
          (ensure-directories-exist file)
          (with-open-file (out file :direction :output :if-does-not-exist :create
                                         :if-exists :supersede :external-format :utf-8)
            (write-string text out)))))
    (truename root)))

(defmacro with-marketing-fixture ((root &rest skills) &body body)
  `(let ((,root (marketing-fixture ,@skills)))
     (nlk:with-cleanup ((uiop:delete-directory-tree ,root :validate t :if-does-not-exist :ignore))
       ,@body)))

;;; --- calling the cell -------------------------------------------------------------

(defun marketing-answer (id tool arguments)
  "TOOL's answer to the JSON ARGUMENTS, as the model reads it."
  (nle::message-content (execute-wire-call id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "marketing"
  (:config "max_chars" 6000)
  (:help :marketing)
  (:command "marketing")
  (:running
   (is (= 6000 (nodecode-marketing::setting :max-chars)) "max_chars defaults to six thousand")
   (is (= 207 (length (nodecode-marketing::catalog))) "and the cell ships the plugin's 207 skills")
   (let ((overview (nodecode-marketing::overview)))
     (is-carrying overview "207 marketing skills" ("strategy (" "the first group, counted")
       ("social (" "the last group, counted")))
   (let ((note (nodecode-marketing::catalogue-note)))
     (is-carrying note "207 skills" "content" "social"))
   (let ((entry (assoc :marketing nle::*help-topics*)))
     (is (search "207 marketing skills" (second entry)) "the manual's one line says what is in it")))
  (:refused ("max_chars" 100))
  (:idle nodecode-marketing::marketing-error (nodecode-marketing::catalog)))

;;; --- finding one ------------------------------------------------------------------

(deftest marketing-cell-a-search-finds-a-skill-by-its-name-and-one-sentence-summary ()
  (with-cell-stop ((marketing-start))
    (let ((text (marketing-answer "c1" "marketing_skills" "{\"query\": \"ab test sample size\"}")))
      (is-carrying text "of 207 skills matching" "ab-test-plan" "[strategy]" "marketing_skill {name"))
    (let ((text (marketing-answer "c2" "marketing_skills" "{\"query\": \"pr pitch\"}")))
      (is-carrying text "pr-pitch" "[strategy]"))
    (let ((text (marketing-answer "c3" "marketing_skills" "{\"query\": \"carousel\"}")))
      (is-carrying text "[social]"))))

(deftest marketing-cell-a-search-honours-the-group-filter-and-the-limit ()
  (with-cell-stop ((marketing-start))
    (let ((text (marketing-answer "c1" "marketing_skills" "{\"query\": \"content\", \"group\": \"social\"}")))
      (is-carrying text "in social" "[social]"
        (:absent "[strategy]" "a group filter answers that group only")))
    (let* ((text (marketing-answer "c2" "marketing_skills" "{\"query\": \"content\", \"limit\": 2}"))
           (count 0))
      (with-input-from-string (in text)
        (loop for line = (read-line in nil nil) while line
              when (search "[strategy]" line) do (incf count)))
      (is (= 2 count) "the limit bounds the answer"))))

(deftest marketing-cell-a-word-nowhere-answers-what-to-try-instead ()
  (with-cell-stop ((marketing-start))
    (let ((text (marketing-answer "c1" "marketing_skills" "{\"query\": \"quantum basket weaving\"}")))
      (is-carrying text "Nothing matches" "207 skills in" "Try fewer or broader words"))
    (let ((text (marketing-answer "c2" "marketing_skills" "{\"query\": \"\"}")))
      (is-carrying text "of 207 skills" "marketing_skill"))))

;;; --- reading one ------------------------------------------------------------------

(deftest marketing-cell-a-read-answers-the-skill-without-its-frontmatter ()
  (with-cell-stop ((marketing-start))
    (let ((text (marketing-answer "c1" "marketing_skill" "{\"name\": \"ab-test-plan\"}")))
      (is-carrying text "ab-test-plan (strategy)" "from skills/strategy/ab-test-plan/SKILL.md"
        (:absent "description: \"Plan an A/B" "the frontmatter is not part of the prose"))
      (is (plusp (or (search "# " text) 0)) "the body's headings are there"))))

(deftest marketing-cell-a-clipped-read-names-the-headings-that-are-left ()
  (with-marketing-fixture (root (list "content" "long-post" +long-body+))
    (with-cell-stop ((marketing-start "skills_dir" (namestring root) "max_chars" 600))
      (let ((text (marketing-answer "c1" "marketing_skill" "{\"name\": \"long-post\"}")))
        (is-carrying text "long-post (content)" "of "
          ("characters" "the clip says how much it gave")
          ("headings further down" "Output format"))
        (is (< (length text) (length +long-body+)) "the clip is shorter than the file"))
      (let ((text (marketing-answer "c2" "marketing_skill"
                                    "{\"name\": \"content/long-post\", \"section\": \"Output format\"}")))
        (is-carrying text "## Output format" "Markdown, with a title and a summary."
          (:absent "Every claim is checked twice" "one section, not the whole skill")))
      (let ((text (marketing-answer "c3" "marketing_skill"
                                    "{\"name\": \"long-post\", \"section\": \"nothing here\"}")))
        (is-carrying text "has no heading holding" "Quality gates" "Output format")))))

(deftest marketing-cell-the-fixture-frontmatter-is-read-as-the-skills-read-it ()
  (with-marketing-fixture (root (list "content" "long-post" +long-body+))
    (with-cell-stop ((marketing-start "skills_dir" (namestring root)))
      (let ((skill (nodecode-marketing::resolve-skill "long-post")))
        (is (equal "long-post" (nodecode-marketing::skill-name skill)))
        (is (equal "A body longer than the clip. \"yes\""
                   (nodecode-marketing::skill-description skill)) "a quoted description loses its quotes and keeps its escaped ones")
        (is (= 1 (length (nodecode-marketing::catalog))) "one skill in the tree")))))

;;; --- what a wrong name, an empty tree and the command answer --------------------------

(deftest marketing-cell-an-unknown-name-says-what-to-search-for ()
  (with-cell-stop ((marketing-start))
    (let ((text (marketing-answer "c1" "marketing_skill" "{\"name\": \"quantum basket weaving\"}")))
      (is-carrying text "No skill is named" "marketing_skills"))))

(deftest marketing-cell-an-empty-tree-says-where-to-point-skills-dir ()
  (with-marketing-fixture (root)
    (with-cell-stop ((marketing-start "skills_dir" (namestring root)))
      (let ((text (marketing-answer "c1" "marketing_skills" "{\"query\": \"seo\"}")))
        (is-carrying text "No skill found under" "skills_dir"))
      (let ((text (marketing-answer "c2" "marketing_skill" "{\"name\": \"ab-test-plan\"}")))
        (is-carrying text "No skill found")))))

(deftest marketing-cell-the-command-with-words-finds-and-with-none-lists ()
  (with-cell-stop ((marketing-start))
    (let ((text (nth-value 0 (cell-entry "nodecode-marketing" "marketing" "" nil))))
      (is-carrying text "207 marketing skills, in three groups" "strategy (" "social ("
        "marketing_skills finds it" "marketing_skill reads it"))
    (let ((text (nth-value 0 (cell-entry "nodecode-marketing" "marketing" "technical seo" nil))))
      (is-carrying text "skills matching" "technical-seo" "Read one with marketing_skill"))))
