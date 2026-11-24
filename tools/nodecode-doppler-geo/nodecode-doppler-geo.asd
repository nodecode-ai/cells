;;;; nodecode-doppler-geo.asd --- flyover Doppler tones, and where they were heard.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `doppler-geo' section, registers the two tools
;;;; (doppler_detect, doppler_locate), puts /doppler and the manual on
;;;; (help :doppler-geo).
;;;;
;;;; Everything here is plain Lisp over the files the user names: a WAV and an
;;;; ADS-B CSV. No numpy, no ffmpeg, no network.

(defsystem "nodecode-doppler-geo"
  :description "Aircraft flyover Doppler tones in a recording, and the ground area they were heard from"
  :license "MIT"
  :version "0.1.0"
  :author "mojomast (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wav")
               (:file "dsp")
               (:file "detect")
               (:file "locate")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-doppler-geo/test"))))

(defsystem "nodecode-doppler-geo/test"
  :description "The cell's test slice: a synthetic flyover, its track, every refusal."
  :license "MIT"
  :version "0.1.0"
  :author "mojomast (original), Nodecode (rewrite)"
  :depends-on ("nodecode-doppler-geo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (declare (ignore operation component))
                    (uiop:symbol-call '#:nodecode.test '#:run-doppler-geo-tests)))
