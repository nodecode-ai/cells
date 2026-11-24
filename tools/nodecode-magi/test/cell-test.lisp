;;;; cell-test.lisp --- lifecycle, the queue, the store, the tools, /magi.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over scratch stores: that a
;;;; start installs the five tools, /magi and the manual, and a stop takes
;;;; every one of them back; that a bad setting refuses; that a proposal is
;;;; staged, listed, read and either applied to the store or dropped; that a
;;;; proposal naming an entry the store does not hold is refused at once; that
;;;; an unset store refuses every verb by naming its setting; and that a tool
;;;; call reaches the same code through the dispatch seam.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "magi"
  (:config "store" (magi-scratch-store))
  (:help :magi)
  (:command "magi")
  (:running (is (nle::find-tool "magi_pending") "the queue tool is registered")
            (is (nle::find-tool "magi_show") "the show tool is registered")
            (is (nle::find-tool "magi_stage") "the stage tool is registered")
            (is (nle::find-tool "magi_decide") "the decide tool is registered")
            (is (nle::find-tool "magi_memory") "the store tool is registered")
            (is (equal 4000 (nodecode-magi::setting :char-limit))))
  (:refused ("char_limit" 0) ("page_size" 0) ("store" 5))
  (:stopped (is (null (nle::find-tool "magi_pending")) "stop unregisters the tools")
            (is (null (nle::find-tool "magi_decide")))))

;;; --- the queue and the store ------------------------------------------------------

(deftest magi-cell-a-staged-write-becomes-durable-only-when-approved ()
  (let ((store (magi-scratch-store)))
    (with-magi ("store" store)
      (let ((id (nodecode-magi::stage "memory" "add" "the build runs with just")))
        (is (equal 1 (length (magi-pending-files store))) "one record on disk")
        (is (null (magi-store-entries store)) "and nothing in the store yet")
        (is (search "the build runs with just" (nodecode-magi::list-pending)) "the queue lists it")
        (is (search "+ the build runs with just" (nodecode-magi::show-record id)) "its diff adds the entry")
        (is (search "applied" (nodecode-magi::decide id "approve")))
        (is (equal '("the build runs with just") (magi-store-entries store)) "approval wrote the store")
        (is (null (magi-pending-files store)) "and took the record away")))))

(deftest magi-cell-a-rejected-write-leaves-the-store-alone ()
  (let ((store (magi-scratch-store)))
    (magi-seed-store store "memory" "kept before")
    (with-magi ("store" store)
      (let ((id (nodecode-magi::stage "memory" "add" "never approved")))
        (is (search "rejected" (nodecode-magi::decide id "reject")))
        (is (equal '("kept before") (magi-store-entries store)))
        (is (null (magi-pending-files store)) "rejection removes the record too")))))

(deftest magi-cell-replace-and-remove-name-a-stored-entry ()
  (let ((store (magi-scratch-store)))
    (magi-seed-store store "user" "likes tea" "works in the morning")
    (with-magi ("store" store)
      (let ((id (nodecode-magi::stage "user" "replace" "likes green tea" "likes tea")))
        (is (equal '("likes green tea" "works in the morning")
                   (progn (nodecode-magi::decide id "approve")
                          (magi-store-entries store "user"))) "the named entry alone was rewritten"))
      (let ((id (nodecode-magi::stage "user" "remove" nil "works in the morning")))
        (nodecode-magi::decide id "approve")
        (is (equal '("likes green tea") (magi-store-entries store "user")) "a remove takes one entry away"))
      (is (search "no stored entry matches"
                  (refusal-text nodecode-magi::magi-error
                    (nodecode-magi::stage "user" "remove" nil "a thing never stored"))) "refused at staging time, nothing queued")
      (is (null (magi-pending-files store)) "a refused proposal writes nothing")
      (is (null (magi-pending-files store)) "and leaves nothing in the queue"))))

(deftest magi-cell-the-entries-a-store-holds-carry-the-budget ()
  (let ((store (magi-scratch-store)))
    (magi-seed-store store "memory" "one fact" "another fact")
    (with-magi ("store" store "char_limit" 100)
      (let ((text (nodecode-magi::entries "memory")))
        (is (equal '("one fact" "another fact") text) "both entries, in file order"))
      (let ((listing (nodecode-magi::entries-text "memory" (nodecode-magi::entries "memory"))))
        (is (search "memory: 2 entries" listing))
        (is (search "/100 characters used" listing) "the section's ceiling"))
      (is (equal "stored in memory (3 entries)"
                 (nodecode-magi::add-entry "memory" "a third fact")) "an entry added by hand is durable at once"))))

;;; --- the tools through the real dispatch seam ----------------------------------------

(deftest magi-cell-the-tools-answer-through-the-dispatch-seam ()
  (let ((store (magi-scratch-store)))
    (with-magi ("store" store)
      (let ((result (execute-wire-call "magi-1" "magi_stage"
                                       "{\"target\":\"memory\",\"action\":\"add\",\"content\":\"from the model\"}")))
        (is (search "staged" (nle::message-content result)) "a staging call answers the id"))
      (let ((result (execute-wire-call "magi-2" "magi_pending" "{}")))
        (is (search "from the model" (nle::message-content result)) "the queue lists it"))
      (let ((refused (execute-wire-call "magi-3" "magi_show" "{}")))
        (is (search "id" (nle::message-content refused)) "a missing member is refused"))
      (let ((result (execute-wire-call "magi-4" "magi_memory" "{\"target\":\"memory\"}")))
        (is (search "no stored entries" (nle::message-content result)) "the store is still empty")))))

(deftest magi-cell-an-unset-store-refuses-by-naming-it ()
  (with-magi ("store" "")
    (dolist (verb (list (lambda () (nodecode-magi::list-pending))
                        (lambda () (nodecode-magi::show-record "anything"))
                        (lambda () (nodecode-magi::stage "memory" "add" "x"))
                        (lambda () (nodecode-magi::decide "anything" "approve"))
                        (lambda () (nodecode-magi::entries "memory"))))
      (let ((answer (handler-case (funcall verb)
                      (nodecode-magi::magi-error (condition) (format nil "~a" condition)))))
        (is (search "magi.store is not set" answer) "every verb names the setting it needs")))
    (is (search "no store set" (nodecode-magi::summary-line)))
    (is (search "magi: magi.store is not set" (nodecode-magi::run-slash "pending" "s1")) "and /magi says the same")))

;;; --- /magi ---------------------------------------------------------------------------

(deftest magi-cell-slash-reads-the-queue-and-approves-by-name ()
  (let ((store (magi-scratch-store)))
    (magi-seed-store store "memory" "kept")
    (with-magi ("store" store)
      (let ((line (nodecode-magi::run-slash "" "s1")))
        (is (search "0 pending" line) "nothing pending yet"))
      (is (search "no proposals are waiting" (nodecode-magi::run-slash "pending" "s1")))
      (let ((id (nodecode-magi::stage "memory" "add" "written from /magi")))
        (is (search "written from /magi" (nodecode-magi::run-slash "pending" "s1")))
        (is (search "+ written from /magi" (nodecode-magi::run-slash (format nil "show ~a" (subseq id 0 6)) "s1")) "a prefix of the id is enough")
        (is (search "applied" (nodecode-magi::run-slash (format nil "approve ~a" id) "s1")))
        (is (equal '("kept" "written from /magi") (magi-store-entries store))))
      (is (search "is not one of its choices" (nodecode-magi::run-slash "bogus" "s1")))
      (multiple-value-bind (text dialog) (nodecode-magi::run-slash "" "s1")
        (declare (ignore text))
        (is (= 3 (length (getf dialog :rows))) "bare /magi answers a dialog of three asks")))))
