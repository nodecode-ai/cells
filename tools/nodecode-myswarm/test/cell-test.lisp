;;;; cell-test.lisp --- lifecycle, the six tools, /myswarm, the scrubber and
;;;; the import's refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not a person's agents.md:
;;;; that the start installs the six tools, /myswarm and the manual and the
;;;; stop takes all three back; that a section of the wrong shape refuses;
;;;; that the table, the diagram, the scrubbed export and the copy-paste
;;;; round trip read the file the test wrote; that an import shows what it
;;;; takes in, keeps the agents.md it replaces and reverts to it; that a
;;;; suspicious file stops an import until a person allows it; and that an
;;;; import with no data_dir refuses naming the setting.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "myswarm"
  (:config "workspace" (myswarm-workspace) "data_dir" (myswarm-data))
  (:help :myswarm)
  (:command "myswarm")
  (:running
   (is (nle::find-tool "myswarm_list") "myswarm_list is registered")
   (is (nle::find-tool "myswarm_ascii") "myswarm_ascii is registered")
   (is (nle::find-tool "myswarm_export") "myswarm_export is registered")
   (is (nle::find-tool "myswarm_import") "myswarm_import is registered")
   (is (nle::find-tool "myswarm_revert") "myswarm_revert is registered")
   (is (equal "unicode" (nodecode-myswarm::setting :default-style)) "the frame defaults to box characters")
   (is (equal "myswarm-export.md" (nodecode-myswarm::setting :default-export-filename))))
  (:stopped
   (is (null (nle::find-tool "myswarm_list")) "stop unregisters myswarm_list")
   (is (null (nle::find-tool "myswarm_import")) "and myswarm_import"))
  (:refused ("default_style" "sideways")
            ("default_export_filename" 42)))

;;; --- reading an agents.md ---------------------------------------------------------

(deftest myswarm-cell-the-list-reads-every-agent-and-its-role ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (let ((answer (nodecode-myswarm:list-agents)))
      (is-carrying (text answer)
        ("3 agents") ("Conductor") ("Forge") ("Aegis")
        ("Persona") ("Goal") ("★ = Orchestrator")))
    (let ((answer (nodecode-myswarm:list-agents)))
      (is (cl:search "* Conductor" answer) "the orchestrator carries the asterisk")
      (is (cl:search "Calm, structured engineering director" (nodecode-myswarm:list-agents "agents.md")) "the persona the section declares is what the table shows"))))

(deftest myswarm-cell-the-topology-draws-the-swarm-and-its-flows ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (let ((answer (nodecode-myswarm:topology)))
      (is-carrying (text answer)
        ("MySwarm Topology") ("[ORCHESTRATOR] Conductor") ("[AGENT] Forge")
        ("Delegation Flows") ("Conductor ──▶ Forge") ("Conductor ──▶ Aegis")
        ("MYSWARM // ") ("[Shared via MySwarm]")))))

(deftest myswarm-cell-a-shared-block-reads-back-as-the-swarm-it-shows ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (multiple-value-bind (title agents) (nodecode-myswarm::swarm-of (merge-pathnames "agents.md" (myswarm-ws-path)))
      (let ((block (nodecode-myswarm:copy-paste-text title agents)))
        (multiple-value-bind (read-title read-agents method) (nodecode-myswarm:parse-agents block)
          (is (eq :ascii-block method) "the block is read as one")
          (is (equal title read-title) "and keeps its title")
          (is (equal (mapcar #'nodecode-myswarm:agent-name agents) (mapcar #'nodecode-myswarm:agent-name read-agents)) "and every agent, by name")
          (is (equal (list "Forge" "Aegis") (nodecode-myswarm:agent-delegates (first read-agents))) "and the routes each one takes")
          (is (equal "orchestrator" (nodecode-myswarm:agent-role (first read-agents))) "and which of them is the orchestrator"))))))

(deftest myswarm-cell-the-export-scrubs-what-a-person-would-not-share ()
  (with-swarm ()
    (myswarm-write "agents.md" +leaky+)
    (let ((answer (nodecode-myswarm:export-copy "shared.md")))
      (is-carrying (text answer) ("shared.md") ("best effort")))
    (let ((written (myswarm-read "shared.md")))
      (is-present written "the copy is on disk"
        (is-carrying (text written)
          ("Exported via MySwarm cell | Agents: 1")
          ("[REDACTED_EMAIL]") ("[REDACTED_SECRET]") ("[REDACTED_IP]")
          ("[REDACTED_PHONE]") ("[REDACTED_PII]"))
        (is (cl:search "~/work" written) "a home path keeps its shape and loses the name")
        (is (null (cl:search "alice@example.com" written)) "the address is gone")
        (is (null (cl:search "/Users/alice" written)) "and so is the folder it lived in")))
    (is (search "Leaky Swarm" (myswarm-read "agents.md")) "the source file is untouched")
    (is (search "may not take" (princ-to-string (or (signals-error nodecode-myswarm:myswarm-error (nodecode-myswarm:export-copy "agents.md")) ""))) "an export may not take a configuration filename")
    (is (search "Leaky Swarm" (myswarm-read "agents.md")) "an export that named agents.md wrote nothing")))

;;; --- taking a stranger's file in --------------------------------------------------

(deftest myswarm-cell-an-import-keeps-what-it-replaces-and-revert-puts-it-back ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (myswarm-write "shared.md" +stranger+)
    (let ((answer (nodecode-myswarm:import-file (namestring (merge-pathnames "shared.md" (myswarm-ws-path))))))
      (is-carrying (text answer) ("imported") ("2 agents") ("/myswarm revert")))
    (is (search "Research Swarm" (myswarm-read "agents.md")) "the workspace's agents.md is the imported one")
    (is (search "Conductor" (myswarm-read "agents.md" (merge-pathnames "backup/" (myswarm-data-path)))) "the replaced file is kept in the data folder")
    (is-carrying (text (nodecode-myswarm:revert)) ("reverted"))
    (is (search "Conductor" (myswarm-read "agents.md")) "and the revert puts it back")
    (is (search "no import to revert" (princ-to-string (or (signals-error nodecode-myswarm:myswarm-error (nodecode-myswarm:revert)) ""))) "a second revert says there is nothing live")))

(deftest myswarm-cell-a-preview-writes-nothing ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (myswarm-write "shared.md" +stranger+)
    (let ((answer (nodecode-myswarm:import-file (namestring (merge-pathnames "shared.md" (myswarm-ws-path)))
                                            :preview t)))
      (is-carrying (text answer) ("preview") ("Nothing was written") ("Scout") ("2 agents")))
    (is (search "Conductor" (myswarm-read "agents.md")) "the workspace's agents.md never moved")
    (is (null (myswarm-read "agents.md" (myswarm-data-path))) "and no backup was made")))

(deftest myswarm-cell-a-suspicious-file-stops-until-a-person-allows-it ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (myswarm-write "gift.md" +suspicious+)
    (let* ((path (namestring (merge-pathnames "gift.md" (myswarm-ws-path))))
           (answer (nodecode-myswarm:import-file path)))
      (is-carrying (text answer)
        ("was not imported") ("high severity") ("prompt injection")
        ("remote code execution") ("credential exfiltration"))
      (is (search "Conductor" (myswarm-read "agents.md")) "the workspace is what it was"))
    (let ((path (namestring (merge-pathnames "gift.md" (myswarm-ws-path)))))
      (is-carrying (text (nodecode-myswarm:import-file path :allow-suspicious t))
        ("imported") ("Helpful Swarm"))
      (is (search "Helpful Swarm" (myswarm-read "agents.md")) "and now it is the import"))))

(deftest myswarm-cell-a-shared-block-imports-back-as-markdown ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (multiple-value-bind (title agents)
        (nodecode-myswarm::swarm-of (merge-pathnames "agents.md" (myswarm-ws-path)))
      (myswarm-write "shared.txt" (nodecode-myswarm:copy-paste-text title agents)))
    (nodecode-myswarm:import-file (namestring (merge-pathnames "shared.txt" (myswarm-ws-path))))
    (let ((written (myswarm-read "agents.md")))
      (is (stringp written))
      (is (cl:search "## Conductor" written) "the block came back as markdown sections")
      (is (cl:search "- **Delegates to**: Forge, Aegis" written) "with the routes it showed"))))

(deftest myswarm-cell-an-import-with-no-data-dir-refuses-naming-it ()
  (with-cell-stop ((myswarm-start "workspace" (myswarm-workspace)))
    (myswarm-write "agents.md" +swarm+)
    (myswarm-write "shared.md" +stranger+)
    (let ((condition (signals-error nodecode-myswarm:myswarm-error
                        (nodecode-myswarm:import-file (namestring (merge-pathnames "shared.md" (myswarm-ws-path)))))))
      (is condition "an import that cannot keep a backup refuses")
      (when condition
        (is-carrying (text (princ-to-string condition)) ("data_dir"))))
    (is (search "Conductor" (myswarm-read "agents.md")) "and nothing was written")))

;;; --- the tools and the command ----------------------------------------------------

(deftest myswarm-cell-the-tool-answers-through-a-call ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (let ((answer (nle:message-content (execute-wire-call "ms-tool-1" "myswarm_list" "{}"))))
      (is-carrying (text answer) ("Conductor") ("Forge") ("Aegis"))
      (is (null (search "ERROR" answer)) "the call answered, it did not refuse"))
    (let ((answer (nle:message-content (execute-wire-call "ms-tool-2" "myswarm_status" "{}"))))
      (is-carrying (text answer) ("workspace") ("agents.md") ("import: none")))))

(deftest myswarm-cell-the-command-answers-every-verb ()
  (with-swarm ()
    (myswarm-write "agents.md" +swarm+)
    (is-carrying (text (cell-entry "nodecode-myswarm" "myswarm" "" nil))
      ("/myswarm list") ("/myswarm import")) 
    (is-carrying (text (cell-entry "nodecode-myswarm" "myswarm" "list" nil))
      ("Conductor") ("Aegis"))
    (is-carrying (text (cell-entry "nodecode-myswarm" "myswarm" "ascii" nil))
      ("Delegation Flows"))
    (is-carrying (text (cell-entry "nodecode-myswarm" "myswarm" "nosuchverb" nil))
      ("is no verb of this cell"))))

(deftest myswarm-cell-a-folder-with-no-agents-md-refuses-naming-it ()
  (with-swarm ()
    (let ((condition (signals-error nodecode-myswarm:myswarm-error (nodecode-myswarm:list-agents))))
      (is condition "no agents.md is a refusal, not an empty table")
      (when condition
        (is-carrying (text (princ-to-string condition)) ("no agents.md"))))))
