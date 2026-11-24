;;; gates.lisp --- the eight gated red-team work modes, and the stages they close on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Data, so a layer can reword or extend it without touching the checks: a
;;;; mode is a plist (:ID :FOCUS :GATES), a gate one (:STAGE :TITLE :CHECKS
;;;; :REQUIRES-FILE :FILE-HINT :MANUAL), and a check one of (:KIND :FILE
;;;; ...), (:KIND :MARKERS :FILE ... :MARKERS ...), (:KIND :TABLE :FILE ...
;;;; :MIN-ROWS N :MIN-CELLS N), (:KIND :HEX-HASH :FILE ...) and
;;;; (:KIND :PROVENANCE :DIR ...).
;;;;
;;;; The stage ids, the artifact file names and the columns they require are
;;;; the original's schema. The words a check looks for are the contract, so
;;;; they are the English ones this cell's manual names, not a translation of
;;;; the original's Chinese: a marker is matched as a substring, exactly as
;;;; there, so a report that says `baseline' passes and one that says nothing
;;;; of it does not.

(in-package #:nodecode-redteam-gates)

(defparameter +gate-modes+
  (list
   (list :id "pentest"
         :focus "Web, API, app and mini-program penetration testing."
         :gates (list (list :stage "P1"
                            :title "Asset and environment baseline"
                            :checks (list (list :kind :file :file "assets.md")
                                          (list :kind :markers
                                                :file "assets.md"
                                                :markers (list "WAF" "rate limit"))
                                          (list :kind :table
                                                :file "assets.md"
                                                :min-rows 2
                                                :min-cells 2)
                                          (list :kind :file :file "evidence-index.md")
                                          (list :kind :markers
                                                :file "evidence-index.md"
                                                :markers (list "tool-plane" "MCP"))
                                          (list :kind :table
                                                :file "evidence-index.md"
                                                :min-rows 1
                                                :min-cells 2))
                            :manual nil)
                      (list :stage "P2"
                            :title "Finding comparison set and review record"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "baseline"
                                                               "diff"
                                                               "marker"
                                                               "review")))
                            :requires-file t
                            :file-hint "the report file of that finding"
                            :manual (list "that the comparison set holds up (the diff really flips, the marker is echoed verbatim) and that the reviewer's confirm or double sign-off is on record stays with the reviewer"))
                      (list :stage "P3"
                            :title "Coverage (assets x vulnerability classes) and review ledger"
                            :checks (list (list :kind :file :file "coverage-matrix.md")
                                          (list :kind :table
                                                :file "coverage-matrix.md"
                                                :min-rows 3
                                                :min-cells 3)
                                          (list :kind :file :file "review-log.md")
                                          (list :kind :markers
                                                :file "review-log.md"
                                                :markers (list "review")))
                            :manual (list "whether an N-A cell's reason stands is spot-checked by the reviewer"
                                          "the review log's verdict (confirm or challenge) is the reviewer's call; only an agreeing second reviewer (this harness plus the local claude or codex) may mark a row double-signed"))))
   (list :id "code-audit"
         :focus "White-box source audit, static and dynamic."
         :gates (list (list :stage "A1"
                            :title "Entry identification and surface map"
                            :checks (list (list :kind :file :file "surface-map.md")
                                          (list :kind :markers
                                                :file "surface-map.md"
                                                :markers (list "entry" "sink" "depth"))
                                          (list :kind :file :file "evidence-index.md")
                                          (list :kind :markers
                                                :file "evidence-index.md"
                                                :markers (list "tool-plane" "MCP")))
                            :manual (list "the known-vulnerability cross-check and the depth grading are the reviewer's"))
                      (list :stage "A2"
                            :title "Chain agreement (audit chain vs tracer chain)"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "entry" "sink")))
                            :requires-file t
                            :file-hint "the call chain record of that finding"
                            :manual (list "that both chains agree is the reviewer's call; a disagreement goes back to tracing or drops the finding to suspected"))
                      (list :stage "A3"
                            :title "Coverage (modules x sinks) and scan reconciliation"
                            :checks (list (list :kind :file :file "audit-coverage-matrix.md")
                                          (list :kind :table
                                                :file "audit-coverage-matrix.md"
                                                :min-rows 3
                                                :min-cells 3)
                                          (list :kind :file :file "scan-reconcile.md")
                                          (list :kind :markers
                                                :file "scan-reconcile.md"
                                                :markers (list "confirmed" "false positive")))
                            :manual (list "that hit counts are conserved (what the scanner reported equals what reached a final state) is the reviewer's"))))
   (list :id "binary-analysis"
         :focus "Malware analysis, reverse engineering, unpacking."
         :gates (list (list :stage "B0"
                            :title "Sample registration"
                            :checks (list (list :kind :provenance :dir "artifacts"))
                            :manual (list "the source and the date are true"))
                      (list :stage "B1"
                            :title "Restoration integrity, three checks"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "dex" "IAT" "runs"))
                                          (list :kind :hex-hash :file "$file"))
                            :requires-file t
                            :file-hint "the verification record of that restored artifact"
                            :manual (list "the three verification verdicts (pass or not) are the reviewer's; not passing means suspected"))
                      (list :stage "B2"
                            :title "Analysis coverage and the hypothesis ledger"
                            :checks (list (list :kind :file :file "analysis-coverage.md")
                                          (list :kind :table
                                                :file "analysis-coverage.md"
                                                :min-rows 3
                                                :min-cells 3)
                                          (list :kind :file :file "hypothesis-ledger.md")
                                          (list :kind :markers
                                                :file "hypothesis-ledger.md"
                                                :markers (list "confirmed" "refuted")))
                            :manual (list "an undecided hypothesis is never written as fact"))))
   (list :id "attack-defense"
         :focus "An attack and defence assessment: a web foothold and then an internal environment, Windows or Linux, monitored or not, with or without a domain, cloud internal."
         :gates (list (list :stage "recon"
                            :title "Stage 1: reconnaissance artifacts"
                            :checks (list (list :kind :file :file "assets.md")
                                          (list :kind :table
                                                :file "assets.md"
                                                :min-rows 2
                                                :min-cells 2)
                                          (list :kind :file :file "evidence-index.md")
                                          (list :kind :markers
                                                :file "evidence-index.md"
                                                :markers (list "tool-plane" "MCP"))
                                          (list :kind :table
                                                :file "evidence-index.md"
                                                :min-rows 1
                                                :min-cells 2))
                            :manual nil)
                      (list :stage "breach"
                            :title "Stage 2: breach artifacts (path ledger)"
                            :checks (list (list :kind :file :file "paths-ledger.md")
                                          (list :kind :markers
                                                :file "paths-ledger.md"
                                                :markers (list "candidate" "chosen"))
                                          (list :kind :table
                                                :file "paths-ledger.md"
                                                :min-rows 1
                                                :min-cells 3))
                            :manual (list "the evidence of a verified finding is the reviewer's gate pass"))
                      (list :stage "lateral"
                            :title "Stage 3: lateral movement, behind a verified breach"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "authorized")))
                            :requires-file t
                            :file-hint "the file recording the lateral movement evidence"
                            :manual (list "that the breach before it was verified and the scope was respected is the reviewer's"))
                      (list :stage "persistence"
                            :title "Stage 4: persistence registry"
                            :checks (list (list :kind :file :file "persistence-registry.md")
                                          (list :kind :markers
                                                :file "persistence-registry.md"
                                                :markers (list "manual removal"))
                                          (list :kind :table
                                                :file "persistence-registry.md"
                                                :min-rows 1
                                                :min-cells 4))
                            :manual (list "registering is the gate; the operator accepts that the removal steps can be carried out"))
                      (list :stage "report"
                            :title "Stage 5: report completeness"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "vulnerability name"
                                                               "ATT&CK"
                                                               "detection gap"
                                                               "persistence inventory"
                                                               "path ledger"
                                                               "stage final"))
                                          (list :kind :file :file "op-traces.md")
                                          (list :kind :markers
                                                :file "op-traces.md"
                                                :markers (list "shell address"
                                                               "ssh key"
                                                               "created users"))
                                          (list :kind :table
                                                :file "op-traces.md"
                                                :min-rows 1
                                                :min-cells 4))
                            :requires-file t
                            :file-hint "the assessment report"
                            :manual (list "the reporter guarantees every finding carries a reviewer's gate pass"
                                          "the operator trace ledger covers every registered webshell, ssh key and created user, a row saying none where there is none"))))
   (list :id "av-evasion"
         :focus "Evasion from the attacker's side: mutating a shell, a loader or a C2, against the model's own refusal."
         :gates (list (list :stage "V1"
                            :title "Experiment plan, three declarations"
                            :checks (list (list :kind :file :file "experiment-plan.md")
                                          (list :kind :markers
                                                :file "experiment-plan.md"
                                                :markers (list "test environment"
                                                               "artifact destination"
                                                               "persistence plan")))
                            :manual (list "the three declarations are the lead's call: the test environment is local or an authorized target, the artifact destination (a lab directory or the task workspace) is true, and any persistence agrees with the registry and its manual removal steps"))
                      (list :stage "V3"
                            :title "Pairing complete: technique mirrors detection"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "technique side"
                                                               "detection side")))
                            :requires-file t
                            :file-hint "that round's experiment report"
                            :manual (list "that both mirrors are symmetric (one technique, one implementation) is the reviewer's"))
                      (list :stage "V2"
                            :title "Evidence, three items"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "build" "verdict"))
                                          (list :kind :hex-hash :file "$file"))
                            :requires-file t
                            :file-hint "the verdict log"
                            :manual (list "that the hashes match and the timestamps make sense is the reviewer's"))
                      (list :stage "V4"
                            :title "Conclusion extrapolation check"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "tested environments")))
                            :requires-file t
                            :file-hint "the experiment conclusion"
                            :manual (list "the conclusion is no wider than what was tested, which the lead or the reviewer judges"))))
   (list :id "incident-response"
         :focus "Windows and Linux incident response and attack attribution, by scenario: a miner, a worm, a trojan."
         :gates (list (list :stage "I1"
                            :title "Evidence preservation registry"
                            :checks (list (list :kind :file :file "evidence-preservation.md")
                                          (list :kind :markers
                                                :file "evidence-preservation.md"
                                                :markers (list "preserved items"
                                                               "forensic command"))
                                          (list :kind :table
                                                :file "evidence-preservation.md"
                                                :min-rows 1
                                                :min-cells 4)
                                          (list :kind :file :file "evidence-index.md")
                                          (list :kind :table
                                                :file "evidence-index.md"
                                                :min-rows 1
                                                :min-cells 2))
                            :manual (list "that preservation can be traced and forensics reads before it writes is the reviewer's"))
                      (list :stage "I2"
                            :title "Timeline and attack chain reconstruction"
                            :checks (list (list :kind :file :file "attack-timeline.md")
                                          (list :kind :markers
                                                :file "attack-timeline.md"
                                                :markers (list "timestamp"
                                                               "suspect IP"
                                                               "evidence"))
                                          (list :kind :table
                                                :file "attack-timeline.md"
                                                :min-rows 3
                                                :min-cells 4))
                            :manual (list "a break in the chain is marked unknown; that every node cites traceable evidence is the reviewer's"))
                      (list :stage "I3"
                            :title "Compromise classification"
                            :checks (list (list :kind :file :file "compromise-verdict.md")
                                          (list :kind :markers
                                                :file "compromise-verdict.md"
                                                :markers (list "classification" "evidence"))
                                          (list :kind :table
                                                :file "compromise-verdict.md"
                                                :min-rows 1
                                                :min-cells 4))
                            :manual (list "the three states (confirmed, suspected, excluded) are the reviewer's; suspected never enters confirmed"))
                      (list :stage "I4"
                            :title "Remediation checklist completeness"
                            :checks (list (list :kind :file :file "remediation-checklist.md")
                                          (list :kind :markers
                                                :file "remediation-checklist.md"
                                                :markers (list "remediation steps"
                                                               "verification"
                                                               "user confirmation"))
                                          (list :kind :table
                                                :file "remediation-checklist.md"
                                                :min-rows 1
                                                :min-cells 4))
                            :manual (list "a destructive step is marked as the operator's to confirm; the operator accepts the steps can be carried out"))
                      (list :stage "I5"
                            :title "Report completeness"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "timeline"
                                                               "compromise cause"
                                                               "ATT&CK"
                                                               "remediation"
                                                               "evidence index")))
                            :requires-file t
                            :file-hint "the incident response report"
                            :manual (list "the reporter guarantees every finding carries a reviewer's gate pass"))))
   (list :id "cloud-security"
         :focus "Cloud platform and cloud-native penetration testing."
         :gates (list (list :stage "C1"
                            :title "Cloud assets and exposure mapping"
                            :checks (list (list :kind :file :file "cloud-assets.md")
                                          (list :kind :markers
                                                :file "cloud-assets.md"
                                                :markers (list "asset"
                                                               "exposure"
                                                               "credential"
                                                               "baseline snapshot"))
                                          (list :kind :table
                                                :file "cloud-assets.md"
                                                :min-rows 2
                                                :min-cells 4)
                                          (list :kind :file :file "evidence-index.md")
                                          (list :kind :table
                                                :file "evidence-index.md"
                                                :min-rows 1
                                                :min-cells 2))
                            :manual (list "that the exposure is complete, credentials traceable and the baseline snapshot restorable is the reviewer's"))
                      (list :stage "C2"
                            :title "Attack path verification"
                            :checks (list (list :kind :file :file "attack-paths.md")
                                          (list :kind :markers
                                                :file "attack-paths.md"
                                                :markers (list "entry"
                                                               "identity"
                                                               "permission"
                                                               "resource"
                                                               "impact"
                                                               "evidence"))
                                          (list :kind :table
                                                :file "attack-paths.md"
                                                :min-rows 1
                                                :min-cells 6))
                            :manual (list "that every path has proof-level evidence and its four parts close with nothing hanging is the reviewer's"))
                      (list :stage "C3"
                            :title "Lateral movement and persistence"
                            :checks (list (list :kind :file :file "lateral-persistence.md")
                                          (list :kind :markers
                                                :file "lateral-persistence.md"
                                                :markers (list "authorized"
                                                               "verification status"
                                                               "evidence"
                                                               "removal steps"))
                                          (list :kind :table
                                                :file "lateral-persistence.md"
                                                :min-rows 1
                                                :min-cells 5))
                            :manual (list "anything out of scope is marked not executed and invented nothing; authorized persistence has its manual removal steps registered"))
                      (list :stage "C4"
                            :title "Permission chains closed"
                            :checks (list (list :kind :file :file "privilege-chains.md")
                                          (list :kind :markers
                                                :file "privilege-chains.md"
                                                :markers (list "start"
                                                               "permission"
                                                               "end"
                                                               "evidence"))
                                          (list :kind :table
                                                :file "privilege-chains.md"
                                                :min-rows 1
                                                :min-cells 4))
                            :manual (list "that every chain has its own evidence, nothing hangs, and suspected never enters confirmed is the reviewer's"))
                      (list :stage "C5"
                            :title "Detection gap assessment"
                            :checks (list (list :kind :file :file "detection-gap.md")
                                          (list :kind :markers
                                                :file "detection-gap.md"
                                                :markers (list "audit" "detection" "final"))
                                          (list :kind :table
                                                :file "detection-gap.md"
                                                :min-rows 1
                                                :min-cells 3))
                            :manual (list "that every critical path has a detection-side verdict, one of the three final states and never blank, is the reviewer's"))
                      (list :stage "C6"
                            :title "Environment restore"
                            :checks (list (list :kind :file :file "environment-restore.md")
                                          (list :kind :markers
                                                :file "environment-restore.md"
                                                :markers (list "object"
                                                               "restore method"
                                                               "verification status"))
                                          (list :kind :table
                                                :file "environment-restore.md"
                                                :min-rows 1
                                                :min-cells 3))
                            :manual (list "that every test change is registered, the restore verifiable, and a destructive one marked as the operator's to confirm"))
                      (list :stage "C7"
                            :title "Report completeness"
                            :checks (list (list :kind :markers
                                                :file "$file"
                                                :markers (list "attack path"
                                                               "misconfiguration"
                                                               "permission chain"
                                                               "detection gap"
                                                               "environment restore"
                                                               "evidence index"
                                                               "stage final")))
                            :requires-file t
                            :file-hint "the cloud security assessment report"
                            :manual (list "the reporter guarantees every attack path carries a reviewer's gate pass"))))
   (list :id "ctf-solver"
         :focus "CTF solving: challenge registration, category routing, a solving loop, a flag ledger and a review."
         :gates (list (list :stage "board"
                            :title "Challenge registration"
                            :checks (list (list :kind :file :file "challenge-board.md")
                                          (list :kind :markers
                                                :file "challenge-board.md"
                                                :markers (list "challenge" "category" "clue"))
                                          (list :kind :table
                                                :file "challenge-board.md"
                                                :min-rows 1
                                                :min-cells 3)
                                          (list :kind :file :file "evidence-index.md")
                                          (list :kind :markers
                                                :file "evidence-index.md"
                                                :markers (list "tool-plane" "MCP"))
                                          (list :kind :table
                                                :file "evidence-index.md"
                                                :min-rows 1
                                                :min-cells 2))
                            :manual (list "that each row's clues are worked through and the category is sensible is the lead's"))
                      (list :stage "flag"
                            :title "Flag ledger closed"
                            :checks (list (list :kind :file :file "flag-ledger.md")
                                          (list :kind :markers
                                                :file "flag-ledger.md"
                                                :markers (list "flag" "verified" "status"))
                                          (list :kind :table
                                                :file "flag-ledger.md"
                                                :min-rows 1
                                                :min-cells 4))
                            :manual (list "every solved flag carries verification evidence; an unsolved one names its blocker, never a guess, a spray or an invention, which the reviewer checks")))))
  "The gated modes, and for each the gates its playbook closes a stage on.
The two presets with no gate of their own - the general security researcher,
which defines no enforced chain, and asset mapping, whose work ends in a
register rather than a report - are prose alone, so they are not here.")
