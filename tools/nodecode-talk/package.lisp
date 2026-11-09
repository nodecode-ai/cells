;;;; package.lisp --- the TALK package: the voice verbs, and what they reach.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed TALK so an eval form reads
;;;; (talk:speak "hello").
;;;;
;;;; Rewritten from TheSmokeDev's hermes-talk (see NOTICE): the two OpenAI
;;;; audio providers that plugin carries -- speech out and transcribe in --
;;;; as the verbs a model here can call.

(defpackage #:nodecode-talk
  (:use #:cl)
  (:nicknames #:talk)
  (:export #:speak #:transcribe #:listen #:talk-error #:talk-error-detail))

(in-package #:nodecode-talk)

;;; FAIL, SETTING and RUNNING-SETTINGS come from here; NLE:DEFINE-CELL in
;;; cell.lisp declares the section, the three tools and the /talk command.
(nlk:define-peripheral talk :not-running t)

(defparameter +version+ "0.1.0"
  "What talk_status reports, as the original plugin's own status tool did.")

(define-condition talk-error (error)
  ((detail :initarg :detail :reader talk-error-detail))
  (:report (lambda (condition stream)
             (write-string (talk-error-detail condition) stream)))
  (:documentation "A verb that cannot go on. DETAIL is one spoken sentence."))

;;; --- what the original plugin's OpenAI lanes offer ---------------------------

(defparameter +voices+
  '("alloy" "ash" "ballad" "coral" "echo" "sage" "shimmer" "verse" "marin" "cedar")
  "The voices the original plugin lists for its OpenAI speech provider.
CEDAR is its own default.")

(defparameter +formats+ '("mp3" "wav" "opus" "flac" "aac")
  "The response formats /v1/audio/speech serves.")

(defparameter +default-base-url+ "https://api.openai.com/v1"
  "OpenAI's API root; the section may name another.")

(defparameter +players+
  '(("afplay" "afplay")
    ("paplay" "paplay")
    ("aplay" "aplay" "-q")
    ("ffplay" "ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet")
    ("mpv" "mpv" "--really-quiet"))
  "The CLIs that play a file, first found wins: (NAME . ARGV-PREFIX).")

;;; --- the two seams a test binds ----------------------------------------------

(defun run-command (argv)
  "Run ARGV, no shell => (values OUTPUT OK)."
  (handler-case
      (multiple-value-bind (out err code)
          (uiop:run-program argv :output :string :error-output :string
                                 :ignore-error-status t)
        (values (or (and out (plusp (length out)) out) err "") (eql 0 code)))
    (error (condition)
      (values (princ-to-string condition) nil))))

(defparameter *run* #'run-command
  "The one place a CLI is run: a function of ARGV answering (values OUTPUT OK);
a test binds it so nothing is played or recorded.")

(defun dial (&rest arguments)
  "NLK:HTTP, resolved at call time: a test stubs it and every caller follows."
  (apply #'nlk:http arguments))

(defparameter *http* #'dial
  "The one place the wire is dialled: NLK:HTTP's own signature; a test binds
it (or stubs NLK:HTTP) so nothing leaves the machine.")
