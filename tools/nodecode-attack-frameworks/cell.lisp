;;;; cell.lisp --- the declaration, four tools, /attack-frameworks, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A table, not a client: every verb answers from DATA.LISP with no request,
;;;; a key or an account. The repositories are links a person opens themselves,
;;;; which is the whole of the "direct access" this cell gives.

(in-package #:nodecode-attack-frameworks)

;;; --- the manual --------------------------------------------------------------------

(defparameter +primer+
  "Five LLM-driven attack frameworks that have already turned up in real intrusions, with what each
one is, how it fails to ask permission, and what it leaves in a log.
  attack_frameworks_list    all five, or those under one licence (mit, apache, agpl): name,
      repository, stars, licence, one line on what it is, and the link.
  attack_frameworks_get     one in full: what it is, how it fails to ask before acting, who has seen
      it in the field, the hunt defaults the post names, and the repository link.
  attack_frameworks_hunt    only the huntable defaults -- User-Agents, ports, markers, persistence
      names, timing -- one line each, ready to paste into a log search. This is the useful verb.
  attack_frameworks_search  free text over names, repositories, licences, descriptions and the hunt
      strings: 'nginx', 'AGPL', 'Storm-0940', '8888'.
  attack_frameworks_summary how many, how starred, the licences, and the post's own reading.
Say what the table is: a snapshot of one post (13 Nov 2026) against the repositories' metadata the
next day, not a signature set. A default can move in any release -- ARTEX logged about 390 commits in
ten and a half weeks -- while the behaviour (credential access, remote execution, offensive tools
started by a service) stays. Two of the five are AGPL-3.0 and nothing of theirs is redistributed
here. These are attack tools: describing one for a hunt, or running it against an estate you own, is
the use; aiming one at somebody else's is not authorized by having read this."
  "What (help :attack-frameworks) answers while the cell runs.")

;;; --- the boundaries the model crosses ------------------------------------------------

(nlk:define-contract attack-frameworks-list
  (:doc "List the LLM-driven attack frameworks in the table: name, repository, stars, licence, one line on what it is, and the link. Narrow by licence (mit, apache, agpl). Use it when the user asks which of these frameworks there are, or which ones are open enough to reuse.")
  ("licence" :text
             :doc "mit, apache, or agpl: the licence family to keep"))

(nlk:define-contract attack-frameworks-get
  (:doc "One framework whole: what it is, how it fails to ask before acting, who has seen it in the field, and the hunt defaults it leaves (User-Agents, ports, markers, persistence names, timing). The name may be approximate -- 'pent' finds PentAGI. Use it when the user names one, or asks what a given framework does or leaves behind.")
  ("name" :text :required t
          :doc "the framework's name, or enough of it to name one"))

(nlk:define-contract attack-frameworks-hunt
  (:doc "Only the huntable defaults, one line each, grouped by framework: User-Agents, ports, file markers, persistence names, timing and account patterns. Give the framework name to narrow, or omit it for all five. Use it when the user asks what to search for in logs, or how to spot one of these on a network.")
  ("framework" :text
               :doc "one framework's name; omitted, every framework's defaults"))

(nlk:define-contract attack-frameworks-search
  (:doc "Search the table by any word: a framework name, a repository, a licence, a phrase from a description, or a hunt string ('8888', 'HexStrike-BrowserAgent', 'Storm-0940'). Answers the frameworks that mention it, one line each. Use it when the user describes something without naming a framework.")
  ("text" :text :required t
          :doc "a word or phrase to look for"))

;;; --- what the model calls ----------------------------------------------------------

(defun list-text (&key licence)
  "The five, or one licence family, with the source under them."
  (let* ((licence (and (stringp licence) (plusp (length licence)) licence))
         (rows (frameworks-matching :licence licence)))
    (format nil "~a~%~a"
            (framework-list-text rows
                                 :heading (if licence
                                              (format nil "~a-licensed frameworks, ~a of ~a"
                                                      (string-downcase licence) (length rows) (length (frameworks)))
                                              (format nil "the ~a frameworks, most-starred first" (length (frameworks)))))
            (source-line))))

(defun get-text (name)
  "One framework, whole, or the whole table when NAME names none."
  (let ((f (framework-by-name name)))
    (if f
        (format nil "~a~%~a~%" (framework-block f) (source-line))
        (format nil "no framework in the table is named ~s. ~a~%~a"
                name (framework-list-text (frameworks)) (source-line)))))

(defun list-tool (arguments)
  "The attack_frameworks_list tool."
  (list-text :licence (attack-frameworks-list-licence arguments)))

(defun get-tool (arguments)
  "The attack_frameworks_get tool."
  (get-text (attack-frameworks-get-name arguments)))

(defun hunt-tool (arguments)
  "The attack_frameworks_hunt tool."
  (format nil "~a~%~a"
          (hunt-text (attack-frameworks-hunt-framework arguments))
          (source-line)))

(defun search-tool (arguments)
  "The attack_frameworks_search tool."
  (let ((text (attack-frameworks-search-text arguments)))
    (format nil "~a~%~a"
            (framework-list-text (frameworks-matching :text text)
                                 :heading (format nil "frameworks mentioning ~s" text))
            (source-line))))

(defparameter +summary-schema+
  (list :description
        (concatenate 'string
                     "How many frameworks the table holds, how many stars between them, their licences, "
                     "and the post's own reading: that none enforces authorization in code by default. "
                     "Use it when the user asks how many there are or what the point of the list is.")
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "attack_frameworks_summary takes no members: the table counts itself.")

(defun summary-tool (arguments)
  "The attack_frameworks_summary tool; its arguments are empty by declaration."
  (declare (ignore arguments))
  (format nil "~a~%~a~%~a~%~a" (summary-text) +revocation+ +rot+ (source-line)))

;;; --- /attack-frameworks ------------------------------------------------------------

(defparameter +usage+
  "usage: /attack-frameworks [name|hunt|licence|words]
  /attack-frameworks            all five, and what the post concluded
  /attack-frameworks hunt       only the huntable defaults, ready for a log search
  /attack-frameworks PentAGI    one framework whole, hunt strings included
  /attack-frameworks agpl       the ones you may not lift code from (mit, apache, agpl)
  /attack-frameworks 8888       anything mentioning that string")

(defun run-slash (args session-id)
  "One /attack-frameworks invocation: the summary, the hunt table, one framework, a licence, or a search."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((zerop (length text)) (format nil "~a~%~a~%~a" (summary-text) +usage+ (source-line)))
          ((string-equal text "hunt") (format nil "~a~%~a" (hunt-text) (source-line)))
          ((member text '("mit" "apache" "agpl") :test #'string-equal)
           (format nil "~a~%~a"
                   (framework-list-text (frameworks-matching :licence text)
                                        :heading (format nil "~a-licensed frameworks" (string-downcase text)))
                   (source-line)))
          ((framework-by-name text) (get-text text))
          (t (format nil "~a~%~a"
                      (framework-list-text (frameworks-matching :text text)
                                           :heading (format nil "frameworks mentioning ~s" text))
                      (source-line))))))

;;; --- the entry ------------------------------------------------------------------------

(nle:define-cell attack-frameworks
  (:section ("attack-frameworks")
    (:guide "nothing is required: the table ships with the cell.")
    ("read_on" :string :default "14 Nov 2026"
               :doc "the day the table was read, as the cell prints it"))
  (:help :attack-frameworks
         "LLM-driven attack frameworks seen in real intrusions, and what they leave in a log"
         +primer+)
  (:tool "attack_frameworks_list" (nlk:contract 'attack-frameworks-list) 'list-tool)
  (:tool "attack_frameworks_get" (nlk:contract 'attack-frameworks-get) 'get-tool)
  (:tool "attack_frameworks_hunt" (nlk:contract 'attack-frameworks-hunt) 'hunt-tool)
  (:tool "attack_frameworks_search" (nlk:contract 'attack-frameworks-search) 'search-tool)
  (:tool "attack_frameworks_summary" +summary-schema+ 'summary-tool)
  (:command "attack-frameworks" 'run-slash
            :description "LLM attack frameworks, and the defaults they leave in a log"
            :argument-hint "name|hunt|licence|words")
  (:start (lambda () nil)))
