;;;; cell-test.lisp --- the archive, the hierarchy, the five verbs, and /model-tiers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The hierarchy and the archive index are proved offline, because they are
;;;; offline: every model named with a tier and a line, every document with a
;;;; digest, every quote pointing at a document the archive holds. The one verb
;;;; that reads a document is proved against a five-line fixture, so the slice
;;;; needs no network at all.

(in-package #:nodecode.test)

(define-test-slice "model-tiers" "MODEL-TIERS-CELL-" :start nodecode-model-tiers:start-cell)

(define-cell-lifecycle-tests "model-tiers"
  (:help :model-tiers)
  (:command "model-tiers")
  (:running (is (nle::find-tool "model_tiers_files") "the archive tool is registered")
            (is (nle::find-tool "model_tiers_list") "the list tool is registered")
            (is (nle::find-tool "model_tiers_get") "the get tool is registered")
            (is (nle::find-tool "model_tiers_search") "the search tool is registered")
            (is (nle::find-tool "model_tiers_quote") "the quote tool is registered"))
  (:stopped (is (null (nle::find-tool "model_tiers_files")) "stop unregisters the archive tool")
            (is (null (nle::find-tool "model_tiers_quote")) "stop unregisters the quote tool")))

;;; --- the archive -------------------------------------------------------------------

(deftest model-tiers-cell-the-archive-holds-documents-with-their-digests ()
  (let ((files (nodecode-model-tiers:files)))
    (is (= 13 (length files)) "thirteen documents in the archive index")
    (loop for file in files
          do (is (plusp (length (nodecode-model-tiers:file-key file))) "every document has a key")
             (is (plusp (length (nodecode-model-tiers:file-vendor file))) "and a vendor")
             (is (plusp (length (nodecode-model-tiers:file-document file))) "and a sentence saying what it is")
             (is (plusp (nodecode-model-tiers:file-bytes file)) "and a size")
             (is (plusp (nodecode-model-tiers:file-lines file)) "and a line count")
             (is (= 64 (length (nodecode-model-tiers:file-sha file))) "and a sha256"))))

(deftest model-tiers-cell-no-two-documents-share-a-key ()
  (let ((keys (mapcar #'nodecode-model-tiers:file-key (nodecode-model-tiers:files))))
    (is (= (length keys) (length (remove-duplicates keys :test #'string-equal))) "a key a person types names one document")))

(deftest model-tiers-cell-the-archive-covers-the-vendors-it-says ()
  (let ((vendors (nodecode-model-tiers:vendors)))
    (is (member "Anthropic" vendors :test #'string=) "Anthropic is in it")
    (is (member "OpenAI" vendors :test #'string=) "so is OpenAI")
    (is (>= (length vendors) 5) "and the smaller agents beside them")))

(deftest model-tiers-cell-a-key-finds-its-document-or-nothing ()
  (let ((file (nodecode-model-tiers:file-by-key "fable-5.2-code")))
    (is file "an exact key finds its document")
    (is (cl:search "FABLE-5.2" (nodecode-model-tiers:file-path file)) "and it is the Claude Code prompt for Fable 5.2"))
  (is (nodecode-model-tiers:file-by-key "opus") "a fragment finds one too")
  (is (null (nodecode-model-tiers:file-by-key "gemini")) "a vendor the archive lacks is nil"))

;;; --- the hierarchy -----------------------------------------------------------------

(deftest model-tiers-cell-every-model-carries-what-a-person-needs ()
  (let ((models (nodecode-model-tiers:models)))
    (is (= 6 (length models)) "the archive names six Claude models and the cell carries all six")
    (loop for model in models
          do (is (plusp (length (nodecode-model-tiers:model-name model))) "every entry has a name")
             (is (member (nodecode-model-tiers:model-tier model)
                         (nodecode-model-tiers:tiers)) "on a tier the archive describes")
             (is (plusp (length (nodecode-model-tiers:model-availability model))) "and says whether a person can have it")
             (is (plusp (length (nodecode-model-tiers:model-quotes model))) "and carries at least one sentence of the archive's own"))))

(deftest model-tiers-cell-every-quote-names-a-document-and-a-line ()
  (let ((keys (mapcar #'nodecode-model-tiers:file-key (nodecode-model-tiers:files))))
    (loop for model in (nodecode-model-tiers:models)
          do (loop for quote in (nodecode-model-tiers:model-quotes model)
                   do (is (member (getf quote :file) keys :test #'string=) "a quote names a document the archive really holds")
                      (is (plusp (getf quote :line)) "and the line it stands on")
                      (is (> (length (getf quote :text)) 20) "and the sentence itself")))))

(deftest model-tiers-cell-the-ladder-is-the-one-the-archive-states ()
  (is (equal '(:mythos :opus :sonnet :haiku) (nodecode-model-tiers:tiers)) "Mythos above Opus, then Sonnet, then Haiku")
  (is (string= "mythos" (nodecode-model-tiers:tier-name :mythos)) "and a tier reads as the archive spells it")
  (is (cl:search "above Opus" (nodecode-model-tiers:tier-sentence :mythos)) "the mythos tier is described in the archive's own direction of the ladder"))

(deftest model-tiers-cell-fable-is-a-mythos-tier-model ()
  (let ((fable (nodecode-model-tiers:model-by-name "Claude Fable 5.2")))
    (is fable "the model whose prompt this is is in the archive")
    (is (eq :mythos (nodecode-model-tiers:model-tier fable)) "and it sits on the Mythos tier, which is the thing a vendor page does not say")
    (is (string= "claude-fable-5-2" (nodecode-model-tiers:model-api fable)) "with the API string the archive gives it")))

(deftest model-tiers-cell-two-generations-say-the-same-thing ()
  (let ((fable (nodecode-model-tiers:model-by-name "Claude Fable 5.2")))
    (is (some (lambda (quote)
                (and (string= "fable-5.1" (getf quote :file))
                     (cl:search "sits above Claude Opus" (getf quote :text))))
              (nodecode-model-tiers:model-quotes fable)) "the generation before says the same of 5.1, in its own document")))

(deftest model-tiers-cell-mythos-52-and-fable-52-are-one-model ()
  (let ((mythos (nodecode-model-tiers:model-by-name "Claude Mythos 5.2")))
    (is mythos "Mythos 5.2 is in the archive")
    (is (eq :mythos (nodecode-model-tiers:model-tier mythos)) "on the same tier as Fable 5.2")
    (is (some (lambda (quote) (cl:search "same underlying model" (getf quote :text)))
              (nodecode-model-tiers:model-quotes mythos)) "and the archive says the two share one underlying model")))

(deftest model-tiers-cell-the-preview-is-not-for-sale ()
  (let ((preview (nodecode-model-tiers:model-by-name "Mythos Preview")))
    (is preview "the first Mythos-class model is in the archive")
    (is (cl:search "not" (string-downcase (nodecode-model-tiers:model-availability preview))) "and the entry says a person cannot have it")))

(deftest model-tiers-cell-the-timeline-is-the-archives-and-in-order ()
  (let ((events (nodecode-model-tiers:events)))
    (is (= 6 (length events)) "six dates, from the release to the day the archive was read")
    (is (string= "9 June 2026" (nodecode-model-tiers:event-when (first events))) "the first is the release")
    (is (cl:search "export control" (nodecode-model-tiers:event-what (second events))) "the second is the suspension, and it names export controls")))

(deftest model-tiers-cell-a-question-answers-with-a-document-and-a-line ()
  (let ((fact (first (nodecode-model-tiers:facts-matching "above opus")))
        (keys (mapcar #'nodecode-model-tiers:file-key (nodecode-model-tiers:files))))
    (is fact "the question the archive is opened for is in the data")
    (is (= 117 (nodecode-model-tiers:fact-line fact)) "and points at the line that answers it")
    (is (member (nodecode-model-tiers:fact-file fact) keys :test #'string=) "in a document the archive holds")))

;;; --- what a person gets -------------------------------------------------------------

(deftest model-tiers-cell-the-ladder-names-every-model-once ()
  (let ((text (nodecode-model-tiers:hierarchy-text)))
    (loop for model in (nodecode-model-tiers:models)
          do (is (cl:search (nodecode-model-tiers:model-name model) text) "the ladder shows every model the archive names"))))

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

(deftest model-tiers-cell-a-search-also-crosses-the-documents ()
  (let ((names (mapcar #'nodecode-model-tiers:file-key
                       (nodecode-model-tiers:files-matching :text "Codex"))))
    (is (>= (length names) 3) "the Codex documents are findable by their own word"))
  (let ((anthropic (nodecode-model-tiers:files-matching :vendor "Anthropic")))
    (is (= 5 (length anthropic)) "and a vendor narrows the archive to its own documents")))

(deftest model-tiers-cell-an-approximate-name-still-names-one ()
  (let ((model (nodecode-model-tiers:model-by-name "fable")))
    (is model "half a name finds the model")
    (is (string= "Claude Fable 5.2" (nodecode-model-tiers:model-name model)) "and finds the right one"))
  (is (null (nodecode-model-tiers:model-by-name "gpt-6")) "a model the archive never puts on a tier is nil"))

(deftest model-tiers-cell-a-block-quotes-its-document-and-its-line ()
  (let* ((model (nodecode-model-tiers:model-by-name "Claude Fable 5.2"))
         (block (nodecode-model-tiers:model-block model)))
    (is (cl:search "fable-5.2-code line 117" block) "the block names the document and the line")
    (is (cl:search "biology and cybersecurity" block) "and the sentence itself")))

(deftest model-tiers-cell-every-answer-says-where-it-came-from ()
  (let ((line (nodecode-model-tiers:source-line)))
    (is (cl:search "CL4R1T4S" line) "the provenance names the archive")
    (is (cl:search "11 Nov 2026" line) "and the day it was read")))

(deftest model-tiers-cell-the-index-shows-a-digest-for-each-document ()
  (let ((text (nodecode-model-tiers:files-text (nodecode-model-tiers:files))))
    (loop for file in (nodecode-model-tiers:files)
          do (is (cl:search (nodecode-model-tiers:file-key file) text) "the index names every document it can quote"))))

;;; --- a document's own lines, on a fixture so the slice needs no network -------------

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

(deftest model-tiers-cell-a-quoted-word-comes-back-numbered-and-named ()
  (let ((fixture (merge-pathnames "model-tiers-fixture-2.txt" (uiop:temporary-directory))))
    (with-open-file (out fixture :direction :output :if-exists :supersede)
      (format out "one~%Above Opus sits the Mythos tier.~%three~%"))
    (unwind-protect
         (let ((text (nodecode-model-tiers::quote-text :text "mythos" :path fixture
                                                       :file "fable-5.2-code")))
           (is (cl:search "Above Opus sits" text) "the line carrying the word comes back")
           (is (cl:search "2  Above" text) "numbered by where it stands")
           (is (cl:search "fable-5.2-code" text) "and the answer says which document it was"))
      (ignore-errors (delete-file fixture)))))

;;; --- /model-tiers ------------------------------------------------------------------

(deftest model-tiers-cell-the-command-answers-its-shapes ()
  (let ((bare (nodecode-model-tiers::run-slash "" nil))
        (tier (nodecode-model-tiers::run-slash "mythos" nil))
        (files (nodecode-model-tiers::run-slash "files" nil))
        (source (nodecode-model-tiers::run-slash "source" nil)))
    (is (cl:search "MYTHOS" bare) "bare, the ladder")
    (is (cl:search "the mythos tier" tier) "a tier name narrows to that tier")
    (is (cl:search "GPT-6" files) "and 'files' gives the archive, the rival documents with it")
    (is (cl:search "sha256" source) "and 'source' gives the archive, its head and a digest")))
