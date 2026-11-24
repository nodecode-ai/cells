;;;; nodecode-defuddle.asd --- a page as clean article markdown, by the defuddle CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `defuddle' section, registers the defuddle_extract tool and the
;;;; /defuddle command, and puts the manual on (help :defuddle).
;;;;
;;;; The one thing outside this folder is the defuddle program itself, a Node
;;;; CLI the user installs with `npm i -g defuddle' (https://www.npmjs.com/
;;;; package/defuddle), which reads a page and prints its article as JSON.
;;;; See NOTICE: this folder is a rewrite of annalinneajohansson82/hermes-defuddle.

(defsystem "nodecode-defuddle"
  :description "Defuddle: clean article markdown and metadata from any URL, through the defuddle CLI"
  :license "MIT"
  :version "0.1.0"
  :author "annalinneajohansson82 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-defuddle/test"))))

(defsystem "nodecode-defuddle/test"
  :description "Defuddle tests. Registered into the shared nodecode.test registry; RUN-DEFUDDLE-TESTS filters by the DEFUDDLE-CELL- name prefix."
  :license "MIT"
  :author "annalinneajohansson82 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-defuddle" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-defuddle-tests)))
