;;;; contracts.lisp --- one declaration per tool that takes arguments.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, and the value a body runs on is
;;;; materialized from them. The member names are the original plugin's own,
;;;; so a prompt written against it still fits.

(in-package #:nodecode-caduceus)

(nlk:define-contract search-request
  (:doc "Search the user's private Caduceus health vault on this machine: DNA annotations, gene-drug results, imported notes and medications. Answers cited snippets; fetch one record in full with ref. Never open the user's DNA or health files yourself: pass the file to caduceus_capture op=import and ask these tools.")
  ("query" :string :default ""
   :doc "words to find, for example 'simvastatin' or 'lactose'")
  ("kinds" :array :of :string :default nil
   :doc "only these record kinds: annotation, medication, journal, source, research")
  ("ref" :string :default ""
   :doc "fetch one record in full by the ref a search returned, such as annotation:rs4988235")
  ("limit" :integer :default 8
   :doc "how many hits to answer, at most 25"))

(nlk:define-contract genome-request
  (:doc "The user's own genotypes and what they mean, from the DNA raw data imported into the local vault. op=lookup (rsids), gene (a gene symbol), pgx (a drug name: gene-drug evidence with each marker called, absent or not on the chip), derived (APOE and the enzymes a chip can call), summary (what is imported, top findings), filter (findings by magnitude, repute or topic), report (rebuild a local HTML variant report). Consumer chips miss rare variants and can be wrong; results say so.")
  ("op" :string :required t
   :options '("lookup" "gene" "pgx" "derived" "summary" "filter" "report")
   :doc "what to answer: lookup, gene, pgx, derived, summary, filter or report")
  ("rsids" :array :of :string :default nil
   :doc "the markers to look up, for example rs4988235")
  ("gene" :string :default ""
   :doc "a gene symbol, for example SLCO1B1")
  ("drug" :string :default ""
   :doc "a generic or brand drug name, for example simvastatin or Zocor")
  ("topic" :string :default ""
   :doc "words to filter findings by")
  ("min_magnitude" :number :default 0
   :doc "keep only findings at or above this magnitude")
  ("repute" :string :default "any" :options '("any" "good" "bad" "neutral")
   :doc "keep only findings of this repute")
  ("limit" :integer :default 25
   :doc "how many findings to answer, at most 100"))

(nlk:define-contract trend-request
  (:doc "Deterministic numbers from the Apple Health samples imported into the local vault. op=metrics (what exists), series (values bucketed by day, week or month), compare (before against after a date, or a medication's start), labs (one measurement over time), check (the latest day against the user's own range for it). Every answer carries units, source and coverage; quote numbers exactly.")
  ("op" :string :required t
   :options '("metrics" "series" "compare" "labs" "check")
   :doc "what to answer: metrics, series, compare, labs or check")
  ("metric" :string :default ""
   :doc "a metric name, for example steps, weight_kg, resting_heart_rate, sleep_hours")
  ("lab" :string :default ""
   :doc "a measurement name or part of one, for example glucose or hba1c")
  ("start" :string :default ""
   :doc "first day to include, YYYY-MM-DD")
  ("end" :string :default ""
   :doc "last day to include, YYYY-MM-DD")
  ("bucket" :string :default "day" :options '("day" "week" "month")
   :doc "how the series is bucketed")
  ("pivot" :string :default ""
   :doc "the day to compare around, YYYY-MM-DD")
  ("drug" :string :default ""
   :doc "compare around this medication's start date instead")
  ("before_days" :integer :default 30
   :doc "how many days before the pivot the comparison reaches")
  ("after_days" :integer :default 30
   :doc "how many days after the pivot the comparison reaches"))

(nlk:define-contract capture-request
  (:doc "Add to or manage the user's Caduceus vault. op=import copies a DNA raw data file (23andMe, AncestryDNA .txt), a Promethease report table (.tsv), an Apple Health export.xml, or FHIR Observation JSON into the vault and imports it; pass the file's absolute path. op=status shows what is imported. op=medication records a start, dose change or stop: show the user the details and set confirm=true only after they agree. op=journal stores a note. op=settings shows the privacy settings. op=sources lists imported files. op=brief writes a local HTML health brief.")
  ("op" :string :required t
   :options '("import" "status" "medication" "journal" "settings" "brief" "sources" "watch_run")
   :doc "what to do: import, status, medication, journal, settings, brief, sources or watch_run")
  ("path" :string :default ""
   :doc "the absolute path of the file to import")
  ("drug" :string :default ""
   :doc "the medication's generic name")
  ("brand" :string :default ""
   :doc "its brand name, when the user gave one")
  ("event" :string :default "start" :options '("start" "dose_change" "stop")
   :doc "what happened: start, dose_change or stop")
  ("dose" :string :default ""
   :doc "the dose as the user wrote it, for example 0.5 mg weekly")
  ("date" :string :default ""
   :doc "the day it happened, YYYY-MM-DD")
  ("text" :string :default ""
   :doc "the note to store, for op=journal")
  ("topics" :array :of :string :default nil
   :doc "optional topics to file the note under")
  ("confirm" :boolean :default nil
   :doc "true only once the user has agreed to the medication event as stated"))
