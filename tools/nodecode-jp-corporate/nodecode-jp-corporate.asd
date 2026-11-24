;;;; nodecode-jp-corporate.asd --- Japanese corporate lookup in gBizINFO, as two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `jp-corporate' section, registers the two tools cell.lisp lists
;;;; and puts the primer on the manual as (help :jp-corporate).
;;;;
;;;; A rewrite of TakeshiTGAL/hermes-plugin-jp-corporate (see NOTICE): the
;;;; plugin registered jp_corp_search and jp_corp_profile over the gBizINFO
;;;; REST API v2 of Japan's Ministry of Economy, Trade and Industry. This cell
;;;; speaks the same two tools to the same API, so the model's verbs transfer.

(defsystem "nodecode-jp-corporate"
  :description "Japanese corporate lookup in gBizINFO: find a corporation by name or 法人番号, and read its registry, subsidy, contract, award, certification, finance, patent and workplace record"
  :author "TakeshiTGAL (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "normalize")
               (:file "gbiz")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jp-corporate/test"))))

(defsystem "nodecode-jp-corporate/test"
  :description "jp-corporate cell tests. Registered into the shared nodecode.test registry; RUN-JP-CORPORATE-TESTS filters by the JP-CORPORATE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-jp-corporate" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jp-corporate-tests)))
