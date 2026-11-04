;;;; panel-test.lisp --- what /channels, /sethome and /agent answer typed alone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A command is its name: typed alone in a channel's room, each answers a
;;;; panel whose rows run its lines or ask for the one text a line takes, so
;;;; nobody spells a verb after the name. Loads after operator-test: reuses
;;;; OPERATOR-HOST and AGENTS-AND-ROUTES.

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

(deftest channel-panel-sethome-typed-alone-marks-where-home-is ()
  (with-temp-store ()
    (let ((host (operator-host :name "panel-home")))
      (nlk:with-cleanup ((nck:stop-host host))
        (with-stubbed-fdefinition (nle:notice (text &rest keys) nil)
          (flet ((marks (panel) (mapcar (lambda (row) (getf row :mark)) (getf panel :rows))))
            (multiple-value-bind (text panel) (nck::sethome-entry "" "chat-100")
              (is (equal "No home channel" text))
              (is (equal '("/sethome here" "/sethome off") (panel-lines panel :run)))
              (is (equal '("" "●") (marks panel)) "none stands marked")
              (is (null (nck::home-target host)) "typed alone, it sets nothing"))
            (is (search "this is the home channel now" (nck::sethome-entry "here" "chat-100")) "a row's line sets it")
            (multiple-value-bind (text panel) (nck::sethome-entry "" "chat-100")
              (is (equal "Home is here" text))
              (is (equal '("●" "") (marks panel))))
            (is (equal "Home is another channel" (nck::sethome-entry "" "chat-200")))
            (is (search "no home channel now" (nck::sethome-entry "off" "chat-100")))
            (is (search "test: no home channel" (nck::sethome-entry "" "s-shell")) "a shell hears where each home is")))))))

(deftest channel-panel-agent-typed-alone-offers-each-agent-and-the-routes ()
  (with-temp-store ()
    (multiple-value-bind (agents routes) (agents-and-routes "{\"coder\": {}, \"support\": {}}")
      (let* ((host (test-host :name "panel-agent" :agents agents :routes routes))
             (nck::*hosts* (list host)))
        (multiple-value-bind (text panel) (nck::agent-entry "" "chat-100")
          (is (search "answered by the channel itself" text) "who answers, as before")
          (is (equal (list "/agent coder" "/agent support" (format nil "/agent ~a" nck::+own-agent+))
                     (panel-lines panel :run)) "a row per agent, then the routes"))))))
