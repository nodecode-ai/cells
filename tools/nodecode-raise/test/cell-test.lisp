;;;; cell-test.lisp --- the pipeline file, the ranking, the gate, the lint, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every tool is called the way the engine calls it -- through its declared
;;;; contract, by the name it registered -- over a scratch home, so the store is
;;;; a real file and nothing of the machine's own is read or written. No test
;;;; reaches the network: the desk has none.

(in-package #:nodecode.test)

(define-test-slice "raise" "RAISE-CELL-" :start nodecode-raise:start-cell)

(define-cell-lifecycle-tests "raise"
  (:help :raise)
  (:command "raise")
  (:running (is (= 10 (nodecode-raise::setting :focus-size)) "focus_size defaults to ten")
            (is (equal "none" (nodecode-raise::setting :email-mode)) "Raise sends nothing by default")
            (is (null (probe-file (nodecode-raise::store-file))) "a read writes no store file"))
  (:refused ("focus_size" 0) ("cap_per_channel" 0) ("email_mode" "post"))
  (:idle raise:raise-error (raise:focus)))

(defmacro with-raise ((&rest config) &body body)
  "BODY with a scratch home and the cell started on CONFIG (the defaults when none)."
  `(with-temp-trees ((home "raise-home"))
     (let ((nlk:*home* home))
       (with-cell-stop ((raise-start ,@config))
         ,@body))))

(defun raise-call (tool json)
  "TOOL called as the engine calls it: its contract decodes JSON, then its body."
  (let ((entry (nle::find-tool tool)))
    (unless entry (error "~a is not registered" tool))
    (funcall (third entry) (nlk:contract-decode (second entry) (nlk:decode-json json)))))

(defun raise-investor (name &key (firm "Northwind Capital") (email "ada@northwind.example"))
  "An investor in the pipeline with a confirmed address, ready to draft to."
  (raise-call "raise_investor"
              (format nil "{\"name\": \"~a\", \"firm\": \"~a\", \"kind\": \"vc\", \"email\": \"~a\", \"email_confirmed\": true}"
                      name firm email)))

(defun raise-evidence (name)
  "One cited fact, so a ranking and a draft have something to stand on."
  (raise-call "raise_evidence"
              (format nil "{\"investor\": \"~a\", \"kind\": \"deal\", \"claim\": \"led the Series A at Northwind in 2021\", \"source_url\": \"https://northwind.example/deals\"}"
                      name)))

(defun text (value)
  "VALUE as the answer string a check reads."
  (princ-to-string value))

(defun raise-card (name)
  (text (raise-call "raise_show" (format nil "{\"investor\": \"~a\"}" name))))

(defun raise-gate (name)
  (text (raise-call "raise_can_draft" (format nil "{\"investor\": \"~a\"}" name))))

(defun raise-lint (name textline)
  (text (raise-call "raise_lint"
                     (if name
                         (format nil "{\"investor\": \"~a\", \"text\": \"~a\"}" name textline)
                         (format nil "{\"text\": \"~a\"}" textline)))))

;;; --- the file ---------------------------------------------------------------------

(deftest raise-cell-tools-write-one-file-and-read-it-back ()
  (with-raise ()
    (is-carrying (text (raise-call "raise_profile"
                                    "{\"fields\": {\"company\": \"Kettle\", \"round_amount\": 1500000}}"))
      "company: Kettle")
    (raise-call "raise_claim" "{\"text\": \"410k ARR\", \"tag\": \"verified\", \"source\": \"https://kettle.example/metrics\"}")
    (raise-investor "Ada Lind")
    (raise-evidence "Ada Lind")
    (let ((path (nodecode-raise::store-file)))
      (is-present (probe-file path) "the tools wrote one store file")
      (is (search "Ada Lind" (nlk:read-text path)) "it holds the investor")
      (is (search "410k ARR" (nlk:read-text path)) "and the claim a draft may use"))))

;;; --- ranking ----------------------------------------------------------------------

(deftest raise-cell-a-ranking-counts-only-cited-evidence ()
  (with-raise ()
    (raise-investor "Ada Lind")
    (raise-evidence "Ada Lind")
    (let* ((evidence-id (getf (car (nodecode-raise::evidence-of
                                    (nodecode-raise::in-pipeline "Ada Lind")))
                              :id))
           (json (format nil "{\"investor\": \"Ada Lind\", \"dims\": {\"thesis\": {\"score\": 8, \"evidence_ids\": [~a]}, \"stage_fit\": {\"score\": 5}}}"
                         evidence-id)))
      (is-carrying (text (raise-call "raise_rank" json))
        "scores")
      (let ((record (nodecode-raise::in-pipeline "Ada Lind")))
        (is (plusp (getf record :score)) "a cited dimension scores")
        (let ((thesis (find "thesis" (getf record :dims) :key (lambda (d) (getf d :name)) :test #'equal))
              (stage (find "stage_fit" (getf record :dims) :key (lambda (d) (getf d :name)) :test #'equal)))
          (is (= 8.0 (getf thesis :score)) "its own score stands")
          (is (= 0.0 (getf stage :score)) "an uncited dimension is zeroed")
          (is (search "zeroed" (getf stage :note)) "and says so"))
        (is-carrying (text (raise-call "raise_focus" "{}"))
          "Ada Lind at Northwind Capital"
          ("email" "reachable by the route it computes"))
        (is-carrying (text (raise-card "Ada Lind"))
          "reachability"
          "led the Series A at Northwind in 2021")))))

;;; --- the gate -----------------------------------------------------------------------

(deftest raise-cell-the-gate-holds-a-pending-touch-and-answers-a-reply ()
  (with-raise ()
    (raise-investor "Ada Lind")
    (is-carrying (text (raise-gate "Ada Lind"))
      "may be drafted to"
      "(cold)"
      "email: open")
    (raise-call "raise_log" "{\"investor\": \"Ada Lind\", \"kind\": \"touch\", \"channel\": \"email\", \"summary\": \"intro email\"}")
    (is-carrying (text (raise-gate "Ada Lind"))
      "may not be drafted to"
      ("still pending" "a fresh touch holds the next one"))
    (raise-call "raise_log" "{\"investor\": \"Ada Lind\", \"kind\": \"reply\", \"channel\": \"email\", \"direction\": \"in\", \"summary\": \"send the deck\"}")
    (is-carrying (text (raise-gate "Ada Lind"))
      "may be drafted to"
      "(reply)"
      ("email" "it answers where they wrote"))))

;;; --- the lint -----------------------------------------------------------------------

(deftest raise-cell-the-lint-blocks-a-number-nobody-can-cite ()
  (with-raise ()
    (raise-call "raise_claim" "{\"text\": \"410k ARR\", \"tag\": \"verified\"}")
    (raise-investor "Ada Lind")
    (raise-evidence "Ada Lind")
    (is-carrying (text (raise-lint "Ada Lind" "Ada, you led the Series A at Northwind in 2021. We are at 900k ARR and growing. Worth twenty minutes?"))
      "BLOCKED by the code checks"
      ("900k" "a number the claims do not carry") ("unverified_number"))
    (is-carrying (text (raise-lint "Ada Lind" "Ada, you led the Series A at Northwind in 2021. We are at 410k ARR and growing. Worth twenty minutes?"))
      "passes the code checks"
      (:absent "unverified_number" "a number the claims carry"))
    (is-carrying (text (raise-lint "Ada Lind" "We are at 410k ARR and growing. Worth twenty minutes?"))
      "BLOCKED"
      ("no_personalisation" "the draft names nothing the investor did"))
    (is-carrying (text (raise-lint nil "Hello. We are solving a seamless, robust problem."))
      "stock_phrase"
      ("Stock AI phrase" "a warning says which phrase"))))

;;; --- the command --------------------------------------------------------------------

(deftest raise-cell-the-command-and-the-brief-answer-the-operator ()
  (with-raise ()
    (raise-investor "Ada Lind")
    (raise-evidence "Ada Lind")
    (is-carrying (text (nodecode-raise::run-slash "" "s1"))
      "Raise: 1 investor"
      "0 ranked"
      "stages:")
    (let ((dialog (nle::dialog-json (nth-value 1 (nodecode-raise::run-slash "" "s1")))))
      (is (equal "Raise" (nle::json-value dialog :text "title")) "typed alone, its panel answers"))
    (is-carrying (text (nodecode-raise::run-slash "Ada" "s1"))
      "Ada Lind at Northwind Capital"
      "stage: new"
      "evidence:")
    (is-carrying (text (nodecode-raise::run-slash "status" "s1"))
      "stages:"
      "store:")
    (is-carrying (text (raise-call "raise_brief" "{}"))
      "Rank Ada Lind"
      ("raise_evidence then raise_rank" "an unranked investor is an action"))
    (is (search "id,name,firm" (raise-call "raise_export" "{\"format\": \"csv\"}")) "the export is a CSV a spreadsheet reads")))
