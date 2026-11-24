;;;; nodecode-mqtt.asd --- a session's door to an MQTT broker.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of JasonSwindle/hermes-mqtt. Optional ASDF cell, not part of
;;;; the organism core: the folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/, and the gateway calls START-CELL after
;;;; recovery, which reads the `mqtt' section and registers mqtt_publish,
;;;; mqtt_read, /mqtt and (help :mqtt). No client library: the MQTT 3.1.1
;;;; protocol over usocket, which the image already has.

(defsystem "nodecode-mqtt"
  :description "MQTT: publish an event to a broker, read a topic, from a session"
  :license "MIT"
  :author "JasonSwindle (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mqtt/test"))))

(defsystem "nodecode-mqtt/test"
  :description "MQTT tests. Registered into the shared nodecode.test registry; RUN-MQTT-TESTS filters by the MQTT-CELL- name prefix."
  :license "MIT"
  :author "JasonSwindle (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mqtt" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mqtt-tests)))
