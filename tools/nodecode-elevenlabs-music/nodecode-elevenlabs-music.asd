;;;; nodecode-elevenlabs-music.asd --- ElevenLabs Music as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `elevenlabs-music' section, registers the eight tools the
;;;; model calls, and puts (help :elevenlabs-music) on the manual.
;;;;
;;;; Ported from NousResearch's hermes-official-plugins (see NOTICE). Every
;;;; dependency rides with nodecode: dexador through NLK:HTTP, and chipz,
;;;; which inflates the ZIP archive the stem endpoint answers with.

(defsystem "nodecode-elevenlabs-music"
  :description "ElevenLabs Music: songs, edits, stems and video scores from the ElevenLabs Music API"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "api")
               (:file "music")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-elevenlabs-music/test"))))

(defsystem "nodecode-elevenlabs-music/test"
  :description "The ElevenLabs Music cell's tests: registered into the shared nodecode.test registry, filtered by the ELEVENLABS-MUSIC-CELL- name prefix."
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-elevenlabs-music" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-elevenlabs-music-tests)))
