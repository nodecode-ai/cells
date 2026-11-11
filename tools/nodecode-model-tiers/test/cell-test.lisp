;;;; cell-test.lisp --- the hierarchy's data, its four verbs, and /model-tiers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The hierarchy is proved offline, because the hierarchy is offline: every
;;;; model named with a tier the file describes, every quote at a line number,
;;;; the ladder in the file's order. The one verb that reads the leaked file is
;;;; proved against a five-line fixture, so the slice needs no network at all.

(in-package #:nodecode.test)

(define-test-slice "model-tiers" "MODEL-TIERS-CELL-" :start nodecode-model-tiers:start-cell)

(define-cell-lifecycle-tests "model-tiers"
  (:help :model-tiers)
  (:command "model-tiers")
  (:running (is (nle::find-tool "model_tiers_list") "the list tool is registered")
            (is (nle::find-tool "model_tiers_get") "the get tool is registered")
            (is (nle::find-tool "model_tiers_search") "the search tool is registered")
            (is (nle::find-tool "model_tiers_quote") "the quote tool is registered"))
  (:stopped (is (null (nle::find-tool "model_tiers_list")) "stop unregisters the list tool")
            (is (null (nle::find-tool "model_tiers_quote")) "stop unregisters the quote tool")))

;;; --- the data ---------------------------------------------------------------------

(deftest model-tiers-cell-every-model-carries-what-a-person-needs ()
  (let ((models (nodecode-model-tiers:models)))
    (is (= 6 (length models)) "the file names six models and the cell carries all six")
    (loop for model in models
          do (is (plusp (length (nodecode-model-tiers:model-name model))) "every entry has a name")
             (is (member (nodecode-model-tiers:model-tier model)
                         (nodecode-model-tiers:tiers)) "on a tier the file describes")
             (is (plusp (length (nodecode-model-tiers:model-availability model))) "and says whether a person can have it")
             (is (plusp (length (nodecode-model-tiers:model-quotes model))) "and carries at least one sentence of the file's own"))))

(deftest model-tiers-cell-no-two-models-share-a-name ()
  (let ((names (mapcar #'nodecode-model-tiers:model-name (nodecode-model-tiers:models))))
    (is (= (length names) (length (remove-duplicates names :test #'string-equal))) "a name a person types names one model")))

(deftest model-tiers-cell-every-quote-stands-on-a-line ()
  (loop for model in (nodecode-model-tiers:models)
        do (loop for quote in (nodecode-model-tiers:model-quotes model)
                 do (is (integerp (car quote)) "a quote carries the line it stands on")
                    (is (plusp (car quote)) "and the number is a line of a 7,832-line file")
                    (is (> (length (cdr quote)) 20) "and the sentence itself, not a fragment"))))

(deftest model-tiers-cell-the-ladder-is-the-one-the-file-states ()
  (is (equal '(:mythos :opus :sonnet :haiku) (nodecode-model-tiers:tiers)) "Mythos above Opus, then Sonnet, then Haiku")
  (is (string= "mythos" (nodecode-model-tiers:tier-name :mythos)) "and a tier reads as the file spells it")
  (is (search "above Opus" (nodecode-model-tiers:tier-sentence :mythos)) "the mythos tier is described in the file's own direction of the ladder"))

(deftest model-tiers-cell-fable-is-a-mythos-tier-model ()
  (let ((fable (nodecode-model-tiers:model-by-name "Claude Fable 5.2")))
    (is fable "the model whose prompt this is is in the file")
    (is (eq :mythos (nodecode-model-tiers:model-tier fable)) "and it sits on the Mythos tier, which is the thing the vendor page does not say")
    (is (string= "claude-fable-5-2" (nodecode-model-tiers:model-api fable)) "with the API string the file gives it")))

(deftest model-tiers-cell-mythos-52-and-fable-52-are-one-model ()
  (let ((mythos (nodecode-model-tiers:model-by-name "Claude Mythos 5.2")))
    (is mythos "Mythos 5.2 is in the file")
    (is (eq :mythos (nodecode-model-tiers:model-tier mythos)) "on the same tier as Fable 5.2")
    (is (some (lambda (quote) (cl:search "same underlying model" (cdr quote)))
              (nodecode-model-tiers:model-quotes mythos)) "and the file says the two share one underlying model")))

(deftest model-tiers-cell-the-preview-is-not-for-sale ()
  (let ((preview (nodecode-model-tiers:model-by-name "Mythos Preview")))
    (is preview "the first Mythos-class model is in the file")
    (is (cl:search "not" (string-downcase (nodecode-model-tiers:model-availability preview))) "and the entry says a person cannot have it")))

(deftest model-tiers-cell-the-timeline-is-the-files-and-in-order ()
  (let ((events (nodecode-model-tiers:events)))
    (is (= 6 (length events)) "six dates, from the release to the day the file was committed")
    (is (string= "9 June 2026" (nodecode-model-tiers:event-when (first events))) "the first is the release")
    (is (cl:search "export control" (nodecode-model-tiers:event-what (second events))) "the second is the suspension, and it names export controls")))

(deftest model-tiers-cell-a-question-is-answerable-without-the-network ()
  (let ((fact (first (nodecode-model-tiers:facts-matching "above opus"))))
    (is fact "the question the file is opened for is in the data")
    (is (= 117 (nodecode-model-tiers:fact-lines fact)) "and points at the line that answers it")))

;;; --- what a person gets -------------------------------------------------------------

(deftest model-tiers-cell-the-ladder-names-every-model-once ()
  (let ((text (nodecode-model-tiers:hierarchy-text)))
    (loop for model in (nodecode-model-tiers:models)
          do (is (cl:search (nodecode-model-tiers:model-name model) text) "the ladder shows every model the file names"))))

(deftest model-tiers-cell-a-tier-narrows-to-its-models ()
  (let ((names (mapcar #'nodecode-model-tiers:model-name
                       (nodecode-model-tiers:models-matching :tier "mythos"))))
    (is (= 3 (length names)) "three models on the Mythos tier")
    (is (member "Claude Fable 5.2" names :test #'string=) "Fable 5.2 among them")
    (is (not (member "Claude Opus 5.5" names :test #'string=)) "and Opus below it is not")))

(deftest model-tiers-cell-a-search-crosses-names-notes-and-quotes ()
  (let ((kernel (mapcar #'nodecode-model-tiers:model-name
                        (nodecode-model-tiers:models-matching :text "kernel"))))
    (is kernel "a word only the quotes carry still finds its models")
    (is (member "Claude Fable 5.2" kernel :test #'string=) "and finds the one whose training it describes")))

(deftest model-tiers-cell-an-approximate-name-still-names-one ()
  (let ((model (nodecode-model-tiers:model-by-name "fable")))
    (is model "half a name finds the model")
    (is (string= "Claude Fable 5.2" (nodecode-model-tiers:model-name model)) "and finds the right one"))
  (is (null (nodecode-model-tiers:model-by-name "gpt-6")) "a model the file never names is nil"))

(deftest model-tiers-cell-a-block-quotes-its-lines ()
  (let* ((model (nodecode-model-tiers:model-by-name "Claude Fable 5.2"))
         (block (nodecode-model-tiers:model-block model)))
    (is (cl:search "line 117" block) "the block gives the line a sentence stands on")
    (is (cl:search "biology and cybersecurity" block) "and the sentence itself")))

(deftest model-tiers-cell-every-answer-says-where-it-came-from ()
  (let ((line (nodecode-model-tiers:source-line)))
    (is (cl:search "CL4R1T4S" line) "the provenance names the repository")
    (is (cl:search (subseq (getf nodecode-model-tiers:+file+ :sha256) 0 16) line) "and the digest of the file it was read from")))

;;; --- the file itself, on a fixture so the slice needs no network ---------------------

(deftest model-tiers-cell-a-line-range-is-parsed-or-refused ()
  (is (equal (cons 117 117) (nodecode-model-tiers::line-range "117")) "a line")
  (is (equal (cons 116 122) (nodecode-model-tiers::line-range "116-122")) "a range")
  (is (null (nodecode-model-tiers::line-range "mythos")) "and a word is not a range"))

(deftest model-tiers-cell-a-range-reads-the-numbered-lines-of-a-file ()
  (let ((fixture (merge-pathnames "model-tiers-fixture.txt" (uiop:temporary-directory))))
    (with-open-file (out fixture :direction :output :if-exists :supersede)
      (format out "one~%two~%three~%four~%five~%"))
    (unwind-protect
         (let ((text (nodecode-model-tiers::read-lines 2 3 :path fixture)))
           (is (cl:search "two" text) "the range starts where it was asked")
           (is (cl:search "three" text) "and ends there")
           (is (not (cl:search "four" text)) "and carries no line past it")
           (is (cl:search "2  two" text) "with the line number beside each line"))
      (ignore-errors (delete-file fixture)))))

(deftest model-tiers-cell-a-quoted-word-comes-back-numbered-from-the-file ()
  (let ((fixture (merge-pathnames "model-tiers-fixture-2.txt" (uiop:temporary-directory))))
    (with-open-file (out fixture :direction :output :if-exists :supersede)
      (format out "one~%Above Opus sits the Mythos tier.~%three~%"))
    (unwind-protect
         (let ((text (nodecode-model-tiers::quote-text :text "mythos" :path fixture)))
           (is (cl:search "Above Opus sits" text) "the line carrying the word comes back")
           (is (cl:search "2  Above" text) "numbered by where it stands"))
      (ignore-errors (delete-file fixture)))))

;;; --- /model-tiers ------------------------------------------------------------------

(deftest model-tiers-cell-the-command-answers-its-four-shapes ()
  (let ((bare (nodecode-model-tiers::run-slash "" nil))
        (tier (nodecode-model-tiers::run-slash "mythos" nil))
        (source (nodecode-model-tiers::run-slash "source" nil)))
    (is (cl:search "MYTHOS" bare) "bare, the ladder")
    (is (cl:search "the mythos tier" tier) "a tier name narrows to that tier")
    (is (cl:search "sha256" source) "and 'source' gives the file, its digest and its commit")))
