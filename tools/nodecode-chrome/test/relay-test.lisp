;;;; relay-test.lisp --- a second process on the machine: the relay (nodecode#169).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One image plays both processes. The owner is a real bridge on an
;;;; ephemeral port, which needs no *BRIDGE*: its acceptor closes over its own
;;;; record. The second process is OPEN-BRIDGE on that same port, which finds
;;;; it served and relays. The fake extension polls the owner, so it names
;;;; its secret and Origin itself: *BRIDGE* is the relay, which has neither.

(in-package #:nodecode.test)

(defmacro with-owner ((port-var owner-var &key (secret '(chrome::make-bridge-secret))) &body body)
  "BODY with OWNER-VAR a bridge on an ephemeral port, PORT-VAR (the one
bound, else a fresh one), playing the process that serves it."
  `(let* ((,port-var (or ,port-var (temp-gateway-port)))
          (,owner-var (chrome::start-bridge :port ,port-var :version "0.15.51.5" :secret ,secret)))
     (nlk:with-cleanup ((chrome::stop-bridge ,owner-var))
       ,@body)))

(defmacro with-relay ((relay-var port secret) &body body)
  "BODY with RELAY-VAR the second process's OPEN-BRIDGE on PORT, as
*BRIDGE*; closed on unwind."
  `(with-saved-globals (chrome::*bridge*)
     (let ((,relay-var (chrome::open-bridge :port ,port :secret ,secret :version "0.15.51.5")))
       (nlk:with-cleanup ((chrome::stop-bridge ,relay-var) (chrome::close-bridge))
         (setf chrome::*bridge* ,relay-var)
         ,@body))))

(defun owner-extension (port owner handler)
  "START-FAKE-EXTENSION polling OWNER on PORT with OWNER's secret."
  (start-fake-extension port handler :secret (chrome::bridge-secret owner) :origin +fake-origin+))

(deftest chrome-cell-relay-sends-through-the-owner ()
  (let ((port nil))
    (with-owner (port owner)
      (with-relay (relay port (chrome::bridge-secret owner))
        (is (chrome::relay-p relay) "a served port: the second process relays")
        ;; Nothing polls yet: the owner's own timeout, in the owner's words.
        (is-present (condition (signals-error chrome:chrome-timeout
                                 (chrome::bridge-send relay "tab.list" nil :timeout 0.5)))
          "a relayed send times out as the owner's does"
          (is (search "not polling" (princ-to-string condition))))
        (with-cell-stop ((owner-extension port owner #'echo-handler))
          (let ((result (chrome::bridge-send relay "tab.list" (nlk:json-object "k" "v") :timeout 10)))
            (is (equal (nlk:json-value result :text "action") "tab.list"))
            (is (equal (nlk:json-value result :text "params" "k") "v")))
          ;; The model's verbs, as a session of the second process calls them.
          (is (search "\"tab.list\"" (chrome:send "tab.list")))
          (let ((text (chrome:status)))
            (is (search "served by another process; this one relays" text))
            (is (search "advertising extension 0.15.51.5" text))
            (is (search "extension: connected (Nodecode Chrome Connector fake" text)))
          (is (search "relayed through the process serving" (chrome::connection-line)))
          (is (search "extension connected" (chrome::connection-line))))))))

(deftest chrome-cell-relay-keeps-the-owners-failure-class ()
  (let ((port nil))
    (with-owner (port owner)
      (with-relay (relay port (chrome::bridge-secret owner))
        (with-cell-stop ((owner-extension port owner (lambda (command)
                                                       (declare (ignore command))
                                                       (values nil "boom"))))
          (is-present (condition (signals-error chrome:chrome-command-failed
                                   (chrome::bridge-send relay "page.click" nil :timeout 10)))
            "ok:false through the owner: CHROME-COMMAND-FAILED"
            (is (search "boom" (princ-to-string condition)))
            (is (search "page.click" (princ-to-string condition)))
            (is (not (search "page.click failed: page.click" (princ-to-string condition))))))))))

(deftest chrome-cell-relay-refused-secret-is-said-by-the-command ()
  ;; Another home's nodecode serves the port: no Chrome here, said where it is asked.
  (let ((port nil))
    (with-owner (port owner)
      (with-relay (relay port (chrome::make-bridge-secret))
        (with-cell-stop ((owner-extension port owner #'echo-handler))
          (is-present (condition (signals-error chrome:chrome-offline
                                   (chrome::bridge-send relay "tab.list" nil :timeout 5)))
            "the owner refuses this home's secret: CHROME-OFFLINE"
            (is (search "refuses this home's secret" (princ-to-string condition)))
            (is (search (princ-to-string port) (princ-to-string condition))))
          (is (search "refuses this home's secret" (chrome:status)))
          (is (search "refuses this home's secret" (chrome::connection-line)))
          (is (eq chrome::*bridge* relay) "a refusal is no takeover"))))))

(deftest chrome-cell-relay-takes-the-port-when-the-owner-exits ()
  (with-saved-globals ((chrome::*reclaim-seconds* 0.1))
    (let ((port nil)
          (secret (chrome::make-bridge-secret)))
      (with-owner (port owner :secret secret)
        (with-relay (relay port secret)
          (sleep 0.4)
          (is (eq chrome::*bridge* relay) "while the owner serves, the relay relays")
          (chrome::stop-bridge owner)
          (is (await (:timeout 5) (chrome::bridge-p chrome::*bridge*)))
          (is (eql port (chrome::bridge-port chrome::*bridge*)))
          (is (chrome::relay-stopping-p relay) "the relay stood down")
          (is-values (status body) (bridge-http port :get "/status")
            (status eql 200) ((nlk:json-value body :text "mode") "server"))
          ;; The extension of the same home moves the commands here now.
          (with-cell-stop ((start-fake-extension port #'echo-handler :secret secret :origin +fake-origin+))
            (is (equal "tab.list" (nlk:json-value (chrome::bridge-send chrome::*bridge* "tab.list" nil
                                                                       :timeout 10)
                                                  :text "action")))))))))

(deftest chrome-cell-relay-send-takes-over-a-port-left-free ()
  ;; The watcher has not looked yet: the send that finds the owner gone takes the port.
  (with-saved-globals ((chrome::*reclaim-seconds* 600))
    (let ((port nil)
          (secret (chrome::make-bridge-secret)))
      (with-owner (port owner :secret secret)
        (with-relay (relay port secret)
          (chrome::stop-bridge owner)
          (with-cell-stop ((start-fake-extension port #'echo-handler :secret secret :origin +fake-origin+))
            (is (equal "tab.list" (nlk:json-value (chrome::bridge-send relay "tab.list" nil :timeout 10)
                                                  :text "action")))
            (is (chrome::bridge-p chrome::*bridge*))
            (is (eql port (chrome::bridge-port chrome::*bridge*)))))))))

(deftest chrome-cell-a-second-process-starts-and-relays (with-chrome-port (port))
  ;; nodecode#169: a port another process of this home serves is no refusal.
  ;; The cell starts, every session reads the manual, and Chrome works.
  (with-owner (port owner :secret (chrome::bridge-token))
    (with-cell-stop ((chrome-start "background" nil))
      (is (chrome::relay-p chrome::*bridge*) "the cell started, relaying")
      (is (equal chrome::+primer+ (nle:help :chrome)))
      (with-cell-stop ((owner-extension port owner #'echo-handler))
        (is (search "\"tab.list\"" (chrome:send "tab.list")))
        (is (search "relayed through the process serving" (slash-chrome "status" nil)))
        (is (search "extension connected" (slash-chrome "status" nil)))))
    (is (null chrome::*bridge*) "stop takes the relay down")
    (is-values (status) (bridge-http port :get "/status")
      (status eql 200))))
