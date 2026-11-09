;;;; contracts.lisp --- the two tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.
;;;;
;;;; The argument names, their meanings and the defaults are the original
;;;; plugin's; a range the original clamped is said in a member's :DOC and
;;;; clamped in the body, because a contract states a shape and a clamp is a
;;;; rule of the verb. An unknown `view' is not a filter but a note, as the
;;;; original had it, so it stays a plain string rather than a fixed set.

(in-package #:nodecode-windows-apps)

(nlk:define-contract app-table
  (:doc "Answer 'what is installed on this Windows machine': the software inventory behind Apps & Features, swept from the four Uninstall registry keys (the machine's 64-bit and 32-bit views plus the user's), so a program is found even when the Settings page hides it. Each row carries version, publisher, size, install date and which view it lives in; the same name in two views is a 32/64-bit dual install, marked instead of listed twice. Entries the publisher asked to hide (SystemComponent: runtimes, update rollups) are filtered out unless asked for. Read-only; uninstall commands are reported, never run. Windows only.")
  ("query" :string :default ""
   :doc "case-insensitive substring of the program's name or its publisher, e.g. 'visual c++', 'python', 'obs'")
  ("view" :string :default ""
   :doc "only one view: machine-64, machine-32, user-64 or user-32; an unknown value is not a filter, and the notes say so")
  ("include_system_components" :boolean :default nil
   :doc "include entries hidden from Apps & Features (runtimes, updates; default false)")
  ("limit" :integer :default 100
   :doc "rows to return, 1 to 400 (clamped, default 100)"))

(nlk:define-contract app-detail
  (:doc "One installed program in full: every field its publisher wrote -- version, publisher, size, install date, install location and whether that folder is still on disk, help and about links, comments -- plus a reading of the uninstall command (an MSI product code, the publisher's setup executable, an appx removal or a script), so the next step is known before anything is run. A program registered in several views is shown once with all its occurrences (the 32/64-bit dual install case). Use it after win_app_table names a program. Read-only; the uninstall string is classified and quoted, never executed. Windows only.")
  ("name" :text :required t
   :doc "the program's DisplayName as win_app_table showed it, e.g. 'Python 3.13' or 'OBS Studio' (case-insensitive)"))
