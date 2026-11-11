;;;; nodecode-t3mp3st.asd --- a running T3MP3ST War Room, for the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Drives the War Room of T3MP3ST (github.com/elder-plinius/T3MP3ST, by
;;;; Pliny the Elder), which is a separate program under AGPL-3.0. This cell
;;;; ships none of its code, and speaks to it over the HTTP API its own UI
;;;; reads. See NOTICE.
;;;;
;;;; Two files beside this one: WARROOM.LISP, the one network call and the
;;;; five verbs, and CELL.LISP, the one declaration. The .asd sits INSIDE its
;;;; folder: install by putting -- or symlinking -- this directory under
;;;; ~/.nodecode/cells/. Presence is enabled; the package is named after the
;;;; system, which is how the loader finds START-CELL.

(defsystem "nodecode-t3mp3st"
  :description "Drive a running T3MP3ST War Room: launch a mission, watch it, read findings"
  :license "MIT"
  :author "Pliny the Elder (original), Nodecode (this cell)"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "warroom")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-t3mp3st/test"))))

(defsystem "nodecode-t3mp3st/test"
  :description "t3mp3st cell tests. Registered into the shared nodecode.test registry; RUN-T3MP3ST-TESTS filters by the T3MP3ST- name prefix."
  :license "MIT"
  :depends-on ("nodecode-t3mp3st" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-t3mp3st-tests)))
