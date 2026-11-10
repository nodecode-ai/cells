;;;; package.lisp --- the NODECODE-SONG-GIFT package: the MCP client and the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; SONG-GIFT is a nickname, and the slash command
;;;; and the manual topic are spelled the same. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; tools.lisp read the section through it. FAIL and SONG-GIFT-ERROR come
;;;; from NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be
;;;; made.

(defpackage #:nodecode-song-gift
  (:use #:cl)
  (:nicknames #:song-gift)
  (:export #:song-gift-error #:song-gift-error-detail))

(in-package #:nodecode-song-gift)

(nlk:define-peripheral song-gift :not-running t)
