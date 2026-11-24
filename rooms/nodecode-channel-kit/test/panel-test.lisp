;;;; panel-test.lisp --- what /channels answers typed alone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A command is its name: typed alone in a channel's room, /channels answers
;;;; a panel whose rows run its lines or ask for the one text a line takes, so
;;;; nobody spells a verb after the name. Loads after operator-test: reuses
;;;; OPERATOR-HOST and AGENTS-AND-ROUTES. /sethome's and /agent's dialogs are
;;;; operator-test's.

(in-package #:nodecode.test)

(defun panel-lines (panel key)
  "PANEL's rows' lines: KEY :run what Enter runs, :ask what an ask runs its
text after."
  (remove nil (mapcar (lambda (row) (if (eq key :run)
                                        (getf (getf row :run) :enter)
                                        (getf (getf row :ask) :line)))
                      (getf panel :rows))))

(deftest channel-panel-channels-typed-alone-offers-setup-pairing-and-each-pairing ()
  (with-temp-store ()
    (let ((host (operator-host :name "panel")))
      (nlk:with-cleanup ((nck:stop-host host))
        (nck::save-paired host (list (nlk:json-object "id" "u7" "name" "sai")))
        (multiple-value-bind (text panel) (nck::channels-command "" "chat-100")
          (is (search "test" text) "the status report, as before")
          (is (equal '("/channels setup" "/channels paired" "/channels unpair u7") (panel-lines panel :run)))
          (is (equal '("/channels pair") (panel-lines panel :ask)) "a row asks for a pairing code"))
        (is (not (member "/channels setup" (panel-lines (nth-value 1 (nck::channels-command "" "s-shell")) :run)
                         :test #'equal)) "a shell is no channel's room to set up")
        (is (search "frobnicate is not one of its choices" (nck::channels-command "frobnicate" "chat-100")))))))
