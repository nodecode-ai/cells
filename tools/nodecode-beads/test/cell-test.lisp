;;;; cell-test.lisp --- the six tools, the argv each asks for, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring against a stubbed bd: that each
;;;; verb asks bd the argv the plugin asked it (the fixed ready argv, the
;;;; guard pair on every write, a caller value that names a flag refused
;;;; before any spawn), that a claim is checked against its read-back and a
;;;; lost one names the holder, that a stale guard writes nothing, and that a
;;;; store that is not one is refused in words. Nothing here needs bd
;;;; installed or a network.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------------

(define-cell-lifecycle-tests "beads"
  (:help :beads)
  (:command "beads")
  (:running
   (is (= 100 (nodecode-beads::setting :limit)) "a hundred rows by default")
   (is (= 1000 (nodecode-beads::setting :max-rows)) "bd's own row bound, as the plugin set it")
   (dolist (name '("beads_smoke" "beads_frontier" "beads_show"
                   "beads_claim" "beads_update" "beads_comment"))
     (is (and (nle::find-tool name)
              (hash-table-p (nlk:contract-schema (second (nle::find-tool name)))))
         (format nil "~a is registered against a contract" name))))
  (:refused ("bd" "definitely-no-such-program-xyz")
            ("store" "/definitely-no-such-store-xyz"))
  (:idle beads:beads-error
         (beads:frontier "/tmp/beads-nowhere")
         (beads:show "/tmp/beads-nowhere" "bd-1")
         (beads:claim "/tmp/beads-nowhere" "bd-1")
         (beads:comment "/tmp/beads-nowhere" "bd-1" :text "hello")))

;;; --- the read verbs ------------------------------------------------------------------------

(deftest beads-cell-frontier-asks-bd-the-plugin-s-ready-argv ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("ready" 0 "[{\"id\":\"bd-1\",\"status\":\"open\",\"priority\":1,\"title\":\"fix the parser\"}]"))
        (let ((text (beads:frontier store)))
          (is (search "1 ready" text) "the count comes first")
          (is (search "bd-1" text) "then the row's id")
          (is (search "fix the parser" text) "and its title")
          (is (equal '("ready" "--json" "--exclude-type=epic" "-n" "100" "--max-rows" "1000")
                     (last-command)) "the plugin's fixed ready argv, epics excluded"))
        (beads:frontier store :label "ui")
        (is (equal '("--label" "ui") (tail-of (last-command) 2)) "a label is scoped in")))))

(deftest beads-cell-an-empty-frontier-is-a-sentence-and-a-breached-bound-is-not ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("ready" 0 "[]"))
        (is (search "nothing ready" (beads:frontier store)) "an empty read says so in words"))
      (with-bd-stub (("ready" 2 "" "bound"))
        (is (search "max-rows" (refusal (beads:frontier store))) "exit 2 is a breached bound, never an empty list"))
      (with-bd-stub (("ready" 0 "not json"))
        (is (search "not JSON" (refusal (beads:frontier store))) "output that does not parse is a refusal, not an empty result")))))

(deftest beads-cell-show-renders-one-bead-from-the-array-bd-answers ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("show" 0 "[{\"id\":\"bd-1\",\"status\":\"open\",\"priority\":2,\"title\":\"fix the parser\",\"notes\":\"seen twice\"}]"))
        (let ((text (beads:show store "bd-1")))
          (is (search "bd-1" text) "the id")
          (is (search "P2" text) "the priority")
          (is (search "seen twice" text) "and the notes")
          (is (equal '("show" "bd-1" "--json") (last-command)) "one exact-id argv")))
      (with-bd-stub (("show" 0 "[]"))
        (is (search "knows no bead" (beads:show store "bd-9")) "an unknown id is said plainly")))))

;;; --- the writes ------------------------------------------------------------------------------

(deftest beads-cell-claim-reads-back-and-a-lost-claim-names-the-holder ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("update" 0 "{}")
                     ("show" 0 "[{\"id\":\"bd-1\",\"status\":\"in_progress\",\"assignee\":\"ada\",\"title\":\"fix the parser\"}]"))
        (is (search "claimed bd-1 as ada" (beads:claim store "bd-1" :actor "ada")) "the claim is answered as verified")
        (is (equal '("update" "bd-1" "--claim" "--json") (argv-of "update")) "the claim argv"))
      (with-bd-stub (("update" 0 "{}")
                     ("show" 0 "[{\"id\":\"bd-1\",\"status\":\"open\",\"assignee\":\"bob\"}]"))
        (is (search "not ownership" (refusal (beads:claim store "bd-1" :actor "ada"))) "a read-back that disagrees is not treated as ownership"))
      (with-bd-stub (("update" 1 "" "issue bd-1 already claimed by bob"))
        (let ((text (refusal (beads:claim store "bd-1" :actor "ada"))))
          (is (search "already claimed by bob" text) "a lost claim names the holder")
          (is (not (argv-of "show")) "and nothing is retried"))))))

(deftest beads-cell-update-writes-under-the-guard-pair-and-checks-the-read-back ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("update" 0 "{}")
                     ("show" 0 "[{\"id\":\"bd-1\",\"status\":\"in_progress\",\"assignee\":\"ada\",\"priority\":2}]"))
        (let ((text (beads:update store "bd-1" :actor "ada" :if-assignee "ada"
                                   :if-status "in_progress"
                                   :fields (nlk:make-json-object "priority" 2))))
          (is (search "updated bd-1" text) "the write is answered")
          (is (search "priority=2" text) "with what was written")
          (is (equal '("update" "bd-1" "--if-assignee" "ada" "--if-status" "in_progress"
                       "--json" "--priority" "2")
                     (argv-of "update")) "one guarded argv, the guard pair first")))
      (with-bd-stub (("update" 0 "{}") ("show" 0 "[]"))
        (is (search "guarded field group"
                    (refusal (beads:update store "bd-1" :actor "ada" :if-assignee "ada"
                                            :if-status "open"
                                            :fields (nlk:make-json-object "status" "closed")))) "a field outside the allowlist is refused, not dropped")
        (is (search "status" (refusal (beads:update store "bd-1" :actor "ada"
                                                      :if-assignee "ada" :if-status "open"
                                                      :fields (nlk:make-json-object "status" "closed")))) "and the refusal names it"))
      (with-bd-stub (("update" 0 "{}") ("show" 0 "[{\"id\":\"bd-1\",\"priority\":3}]"))
        (is (search "read-back" (refusal (beads:update store "bd-1" :actor "ada"
                                                        :if-assignee "ada" :if-status "open"
                                                        :fields (nlk:make-json-object "priority" 2)))) "a read-back that disagrees is a refusal")))))

(deftest beads-cell-a-stale-guard-writes-nothing-and-says-so ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("update" 13 "" "guard_mismatch")
                     ("show" 0 "[{\"id\":\"bd-1\",\"status\":\"open\",\"assignee\":\"bob\"}]"))
        (let ((text (refusal (beads:update store "bd-1" :actor "ada" :if-assignee "ada"
                                            :if-status "in_progress"
                                            :fields (nlk:make-json-object "priority" 2)))))
          (is (search "STALE" text) "exit 13 reads as stale")
          (is (search "bob" text) "and the re-read names who holds it now")
          (is (= 1 (length (remove "update" *calls* :key #'first :test #'string=))) "the same guard is never retried"))))))

(deftest beads-cell-comment-keeps-a-dash-leading-text-positional ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("comments" 0 "[]"))
        (is (search "commented on bd-1" (beads:comment store "bd-1" :actor "ada" :text "--db=/x")) "the comment is answered")
        (is (equal '("comments" "add" "bd-1" "--" "--db=/x") (last-command)) "text that names a flag rides after --")))))

;;; --- the boundary -------------------------------------------------------------------------

(deftest beads-cell-a-value-that-names-a-flag-is-refused-before-any-spawn ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("show" 0 "[]") ("ready" 0 "[]"))
        (is (search "refused before running" (refusal (beads:show store "--db=/x"))) "a hostile id never reaches bd")
        (is (null *calls*) "nothing was run at all")
        (is (null (refusal (beads:frontier store :label "-C/etc"))) "a value right after a value-taking flag is that flag's value, as the plugin decided")))))

(deftest beads-cell-a-directory-that-is-not-a-store-is-refused-in-words ()
  (with-cell-stop ((beads-start))
    (with-bd-stub ()
      (is (search "no such directory" (refusal (beads:frontier "/definitely-no-such-directory"))) "a missing directory says so")
      (let ((plain (merge-pathnames "beads-not-a-store/" (uiop:temporary-directory))))
        (ensure-directories-exist (merge-pathnames "keep" plain))
        (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree plain :validate t)))
          (is (search ".beads/" (refusal (beads:frontier (uiop:native-namestring plain)))) "a directory with no .beads/ says to run bd init")))
      (is (null *calls*) "and bd was never run"))))

;;; --- the tools -----------------------------------------------------------------------------

(deftest beads-cell-a-tool-answers-through-its-contract ()
  (with-store (store)
    (with-cell-stop ((beads-start))
      (with-bd-stub (("show" 0 "[{\"id\":\"bd-1\",\"status\":\"open\",\"title\":\"fix the parser\"}]"))
        (let* ((entry (nle::find-tool "beads_show"))
               (contract (second entry))
               (call (nlk:contract-decode contract (nlk:make-json-object "bead" "bd-1"
                                                                          "workspace" store))))
          (is (search "bd-1" (funcall (third entry) call)) "the body answers the tool")
          (is (equal '("show" "bd-1" "--json") (last-command)) "with the plugin's argv"))))))
