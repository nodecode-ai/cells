;;;; contracts.lisp --- the define_term tool's arguments, one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for the tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it. It loads before cell.lisp, whose body reads the record.

(in-package #:nodecode-insight)

(nlk:define-contract term-definition
  (:doc "Define a word, phrase, acronym, API, function or idea for a reader who asked what it means. The cell's own provider and format template write it, in markdown, with a source cited by URL when one was searched for and handed over — so the answer keeps its citations, which your own prose may not. Use it for a definition the user wants explained rather than looked up in their own files, and quote its definition rather than paraphrasing it away.")
  ("term" :text :required t
   :doc "the term to define, as the user wrote it (idempotent, CBOR, race condition, what a monad is)")
  ("template" :string :default ""
   :doc "a format template to use instead of the cell's configured one; empty, the cell's default. It is the whole system instruction, and {term} in it is replaced by the term"))
