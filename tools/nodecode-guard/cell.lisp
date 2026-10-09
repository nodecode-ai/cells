;;;; cell.lisp --- the whole guard cell: rule data and one :tool hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A pre-tool-call guard, and the catalogue example of the generic hook
;;;; seam: the engine is the core waist's textrules, the interception is one
;;;; NLE:HOOK on the :TOOL point, and the durable denial record is the
;;;; ordinary TURN.TOOL_RESULT fact the refusal string journals as, recorded
;;;; failed through NLE:FAILURE. A refused config signals
;;;; NLK:CONFIG-REFUSAL, which the gateway's START-CELLS turns into one loud
;;;; warning with the gateway booting regardless.
;;;;
;;;; NOT a security boundary: containment is an environment gate (src/PLAN.md)
;;;; and eval has full host authority — this is a speed bump. And this is a
;;;; MONO-TOOL organism: EVAL's form argument carries file-edit payloads
;;;; as well as code to run, so every built-in rule is :GATED behind the
;;;; process-spawn operators — an ungated shell rule would deny any patch of a
;;;; file that merely MENTIONS the pattern. Config rules default ungated: an
;;;; operator who writes a pattern expects it to match what it says.
;;;;
;;;; Config, a sibling top-level key next to `channels':
;;;;   "cells": ["nodecode-guard"],
;;;;   "guard": {"enabled": true,
;;;;             "deny":  [{"id": "...", "pattern": "...", "reason": "...",
;;;;                        "gated": false}],
;;;;             "allow": ["..."]}
;;;; `deny' extends the built-in family; `allow' exempts only the region it
;;;; matches (span-scoped, see waist/textrules.lisp).

(defpackage #:nodecode-guard
  (:use #:cl))

(in-package #:nodecode-guard)

;;; Multi-token rules span argv lists with [^)]{0,N} — `(list "rm" "-rf"
;;; path)' separates command from flag with quotes, not whitespace, and a live
;;; directory vanished proving it.
(defparameter +shell-rules+
  '((:id "rm-recursive" :reason "recursive or forced rm" :gated t
     :pattern "\\brm\\b[^)]{0,40}-[a-z]*[rf]\\b")
    (:id "privilege" :reason "privilege escalation" :gated t
     :pattern "\\b(sudo|doas)\\b")
    (:id "world-writable" :reason "world-writable permissions" :gated t
     :pattern "\\b(chmod|chown)\\b[^)]{0,40}\\b777\\b")
    (:id "mkfs" :reason "filesystem creation" :gated t
     :pattern "\\bmkfs(\\b|\\.)")
    (:id "dd-to-device" :reason "dd onto a device node" :gated t
     :pattern "\\bdd\\b[^)]{0,60}\\bof=/dev/")
    (:id "redirect-device" :reason "redirect onto a device node" :gated t
     :pattern ">\\s*/dev/(sd|nvme|disk)")
    (:id "curl-pipe-shell" :reason "download piped straight into a shell"
     :gated t
     :pattern "\\b(curl|wget)\\b[^)]{0,80}\\|[\\s\"',]*(sudo[\\s\"',]+)?(ba|z)?sh\\b")
    (:id "host-power" :reason "host power state" :gated t
     :pattern "\\b(shutdown|reboot|halt|poweroff)\\b")
    (:id "fork-bomb" :reason "fork bomb" :gated t
     :pattern ":\\(\\)\\s*\\{.*\\|.*&.*\\}\\s*;\\s*:"))
  "The built-in family: shell strings, every one spawn-gated.")

(nlk:define-peripheral guard :not-running t)

(defun compile-rules (values table)
  "The engine the hook consults: the built-in family plus the operator's
`deny' entries, behind the spawn gate, `allow' exempting what it matches."
  ;; The deny array is objects, a shape the section declaration does not
  ;; carry, so it is read here; `allow' is declared and comes in VALUES.
  (nlk:compile-text-rules
   ;; The UIOP and SB-EXT spawn entries, package prefix or not, and the snippet's
   ;; own (sh ...): a fresh box's agent ran (sh "sudo apt-get ...") past the
   ;; gate (2026-09-27).
   :gate "\\b(run-program|launch-program|run-shell-command)\\b|\\(sh\\s"
   :rules (append +shell-rules+
                  (loop for entry across (nlk:json-array table "deny")
                        when (hash-table-p entry)
                          collect (list :id (nlk:config-string entry "id")
                                        :pattern (nlk:config-string entry "pattern")
                                        :reason (nlk:config-string entry "reason")
                                        :gated (nlk:config-boolean entry "gated" nil))))
   :allow (getf values :allow)))

(defun refuse-or-run (op next)
  "The :TOOL point: a matched rule answers a refusal, the call's failure, and
NEXT never runs."
  (nlk:if-let (refusal (nlk:text-rules-refusal (setting :rules)
                                               (gethash "form" (getf op :arguments))))
    ;; A fresh box's agent, refused sudo, told the operator only that it could
    ;; not install python3 (2026-09-27): the command is theirs to run.
    (nle:failure (format nil "ERROR: refused by nodecode-guard: ~a; the call did not run -- give the operator the command to run themselves" refusal))
    (funcall next op)))

(nle:define-cell guard
  (:section ("guard")
    (:guide "deny adds {id, pattern, reason, gated} rules; allow exempts the region it matches")
    ("allow" :list :doc "patterns whose matching region is exempt from every rule"))
  ;; The compiled engine is a setting: one compile per start, and the hook
  ;; reads it off the running cell rather than closing over it.
  (:settings (lambda (values table) (list* :rules (compile-rules values table) values)))
  (:hook :tool #'refuse-or-run))
