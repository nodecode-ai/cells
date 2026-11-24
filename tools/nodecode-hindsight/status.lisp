;;;; status.lisp --- /hindsight: the bank it is on, and the verbs by hand.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-hindsight)

(defun status-line ()
  "One line: the bank, the endpoint, the switches, what was injected, and
the last failure, if one is still standing."
  (format nil "hindsight: bank ~a at ~a; auto-recall ~:[off~;on~], auto-retain ~:[off~;on~]; ~d memories injected this session~@[; last error: ~a~]"
          (bank-id)
          (api-url)
          (eq t (setting :auto-recall))
          (eq t (setting :auto-retain))
          *recalled*
          (and *last-error* (subseq *last-error* 0 (min 200 (length *last-error*))))))

(defun panel (line)
  "The dialog bare /hindsight answers: ask a question of the memory, or
synthesize across it, then the line as it stands."
  (nle:list-dialog
   :title "Hindsight" :summary line
   :rows (list (nle:ask-row "Recall memories" "Query" "/hindsight recall"
                             :placeholder "what to search for" :verb "recall")
               (nle:ask-row "Reflect on the bank" "Question" "/hindsight reflect"
                             :placeholder "what is known about …" :verb "reflect")
               (nle:ask-row "Remember something" "Text" "/hindsight retain"
                             :placeholder "what to store" :verb "retain"))))

(defun split-verb (args)
  "ARGS as (values HEAD REST): the first word, and the trimmed remainder."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text)))
    (values (if space (subseq text 0 space) text)
            (if space (nlk:trimmed (subseq text (1+ space))) ""))))

(defun ask (verb rest)
  "ANSWER a verb the operator typed: recall or reflect dial; a verb with
nothing after it says so without dialing."
  (if (zerop (length rest))
      (format nil "hindsight: ~a what? /hindsight ~a QUERY" verb verb)
      (funcall (if (string-equal verb "recall") #'recall #'reflect) rest)))

(defun run-slash (args session-id)
  "One /hindsight invocation => the answer's text, and typed alone the
dialog of what it does."
  (declare (ignore session-id))
  (multiple-value-bind (head rest) (split-verb args)
    (cond ((zerop (length head))
           (let ((line (status-line)))
             (values line (panel line))))
          ((string-equal head "status") (values (status-line) (panel (status-line))))
          ((or (string-equal head "recall") (string-equal head "reflect"))
           (values (ask head rest) (panel (status-line))))
          ((string-equal head "retain")
           (values (if (zerop (length rest))
                       "hindsight: retain what? /hindsight retain TEXT"
                       (remember rest :context "stored by the operator from /hindsight"))
                   (panel (status-line))))
          (t (values (format nil "hindsight: ~a is not one of its choices (recall, reflect, retain, status)"
                              head)
                     (panel (status-line)))))))

(defun start ()
  "A fresh cell starts with no recall cached, no turn retained and no error
standing; a stop takes the state with it."
  (setf *seen* (make-hash-table :test #'equal)
        *retained* (make-hash-table :test #'equal)
        *recalled* 0
        *last-error* nil)
  (nle:on-stop (lambda ()
                 (setf *seen* (make-hash-table :test #'equal)
                       *retained* (make-hash-table :test #'equal)
                       *recalled* 0
                       *last-error* nil))))

;;; --- the one declaration ---------------------------------------------------------

(nle:define-cell hindsight
  (:section ("hindsight")
    (:guide "create a bank key at hindsight.vectorize.io and paste it in api_key (or export HINDSIGHT_API_KEY); bank_id is the partition every session of this home shares; api_url is the cloud endpoint, or a server of your own running hindsight-api")
    ("api_key" :secret
               :doc "the Hindsight API key; HINDSIGHT_API_KEY answers when this is empty")
    ("api_url" :string :default "https://api.hindsight.vectorize.io"
               :doc "the API endpoint: Hindsight Cloud, or a local hindsight-api's address")
    ("bank_id" :string :default "nodecode"
               :doc "the memory bank to read and write; every session using it shares its memories")
    ("budget" :choice :options '("low" "mid" "high") :default "mid"
              :doc "how hard a recall or reflect searches: low is fastest")
    ("recall_max_tokens" :integer :default 4096 :min 1
                         :doc "the most one recall may bring back")
    ("auto_recall" :boolean :default t
                   :doc "recall each turn's input and inject the memories into that request")
    ("auto_retain" :boolean :default t
                   :doc "write the operator's words to the bank once per turn")
    ("tags" :list
            :doc "tags every memory this cell writes carries, beyond each call's own")
    ("timeout" :integer :default 20 :min 1
               :doc "seconds one call to the API may take"))
  (:tool "hindsight_retain" (nlk:contract 'retain-call) 'retain-tool)
  (:tool "hindsight_recall" (nlk:contract 'recall-call) 'recall-tool)
  (:tool "hindsight_reflect" (nlk:contract 'reflect-call) 'reflect-tool)
  (:command "hindsight" 'run-slash
            :description "Long-term memory: recall, reflect, retain"
            :argument-hint "[recall|reflect|retain] QUERY")
  (:help :hindsight "hindsight: long-term memory across sessions - hindsight:recall, hindsight:reflect, hindsight:remember" +primer+)
  (:hook :round (nle:observer #'on-round "hindsight"))
  (:start #'start))
