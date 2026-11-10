;;;; cell-test.lisp --- the lifecycle, the four tools, /on-brand, the gate, the section.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved: the cell starts and stops; a section that names no folder
;;;; refuses by naming the member; each tool answers through the seam the model
;;;; calls it by; the same copy always scores the same; a profile is a file, so
;;;; it outlives a restart; /on-brand switches and edits a brand; the active
;;;; brand's rules ride every request; an outbound call carrying off-brand copy
;;;; is refused before it sends and clean copy goes through; a voiceover script
;;;; has no markdown and spells its numbers. Every call runs against a folder
;;;; inside the test's own home, and no test dials anything: a check is a
;;;; linter, not a service.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defun on-brand-folder ()
  "The folder one test's profiles are kept in, inside the test's own home."
  (namestring (nlk:home "on-brand-test/")))

(defun on-brand-empty ()
  "One test's folder emptied, so it starts from no profiles and no log."
  (ignore-errors (delete-file (merge-pathnames "state.lisp" (on-brand-folder)))))

(defun on-brand-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun on-brand-answer (text)
  "TEXT -- a tool's answer -- decoded, as the model reads it."
  (handler-case (nlk:decode-json text)
    (error (condition) (error "~a: ~a" condition text))))

(defun on-brand-save (&rest pairs)
  "Save a profile through the tool seam. => the decoded answer."
  (on-brand-answer (on-brand-tool "onbrand_rules" (apply #'nlk:make-json-object pairs))))

(defun on-brand-check (&rest pairs)
  "Check copy through the tool seam. => the decoded answer."
  (on-brand-answer (on-brand-tool "onbrand_check" (apply #'nlk:make-json-object pairs))))

(defun on-brand-violations (answer)
  "ANSWER's violations, as a list of detail strings."
  (loop for violation across (nlk:json-value answer :array "violations")
        collect (nlk:json-value violation :string "detail")))

(defun on-brand-call (name arguments)
  "Run the :TOOL point hook with a call to NAME carrying ARGUMENTS.
=> (values ANSWER RAN-P), RAN-P true when the call went through to NEXT."
  (let ((hook (cell-hook "nodecode-on-brand" :tool))
        (ran nil))
    (values (funcall hook (list :name name :arguments arguments :call-id "c-1")
                     (lambda (op) (declare (ignore op)) (setf ran t) "SENT"))
            ran)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "on-brand" "ON-BRAND-CELL-" :start nodecode-on-brand:start-cell)

(define-cell-lifecycle-tests "on-brand"
  (:config "store_dir" (on-brand-folder))
  (:hooks :tool 'nle::read-harness-sections)
  (:command "on-brand")
  (:help :on-brand)
  (:running (is (nle::find-tool "onbrand_check") "onbrand_check is registered")
            (is (nle::find-tool "onbrand_rules"))
            (is (nle::find-tool "onbrand_learn"))
            (is (nle::find-tool "onbrand_voiceover"))
            (is (pathnamep (nodecode-on-brand::state-file)) "the store file is resolved")
            (is (eq t (nodecode-on-brand::setting :gate)) "the gate is on unless the section says otherwise"))
  (:stopped (is (null (nle::find-tool "onbrand_check")) "stop unregisters the tools"))
  (:refused ("gate" t))
  (:idle nodecode-on-brand:on-brand-error (nodecode-on-brand::run-command "" nil)))

(deftest on-brand-cell-a-missing-store-dir-refuses-by-name ()
  (on-brand-empty)
  (is-carrying (refusal (refusal-text nlk:config-refusal (on-brand-start "gate" t)))
               "store_dir"
               "the folder the brand profiles are kept in")
  (is (null (nle::find-tool "onbrand_check")) "a refused start installed nothing"))

;;; --- the linter, through the tool the model calls ------------------------------

(deftest on-brand-cell-a-check-scores-copy-and-names-every-broken-rule ()
  (on-brand-empty)
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (let ((saved (on-brand-save "brand" "nimbus" "display_name" "Nimbus Protocol" "person" "third"
                                "banned" #("to the moon" "LFG")
                                "preferred" (nlk:json-object "NIMBUS" "Nimbus")
                                "packs" #("crypto")
                                "max_chars" 280 "max_hashtags" 1 "max_emojis" 1 "max_exclamations" 0)))
      (declare (ignore saved))
      (is (equal "Nimbus" (cdr (assoc "NIMBUS" (nodecode-on-brand::profile-field
                                                   (nodecode-on-brand::find-profile "nimbus")
                                                   :preferred)
                                    :test #'equal))) "the saved profile keeps the spelling it was given"))
    (let* ((answer (on-brand-check "text" "We're going to the moon!!! GUARANTEED returns, zero risk. LFG #crypto #moon"
                                   "brand" "nimbus"))
           (details (on-brand-violations answer)))
      (is (equal "Nimbus Protocol" (nlk:json-value answer :string "brand")))
      (is (null (nlk:json-value answer :boolean "passed")) "a hard rule broken does not pass")
      (is (< (nlk:json-value answer :integer "score") 100) "and the score says so")
      (is (some (lambda (detail) (search "first person" detail)) details) "the third-person rule is named")
      (is (some (lambda (detail) (search "banned word 'LFG'" detail)) details) "the banned word is named")
      (is (some (lambda (detail) (search "exclamation" detail)) details) "the exclamation limit is named")
      (is (some (lambda (detail) (search "claims no risk" detail)) details) "the compliance pack is named"))
    (let ((clean (on-brand-check "text" "Nimbus Protocol opened staking to its community today. NFA."
                                 "brand" "nimbus")))
      (is (eq t (nlk:json-value clean :boolean "passed")) "clean copy passes")
      (is (equal 100 (nlk:json-value clean :integer "score")))
      (is (= 0 (length (on-brand-violations clean))) "with nothing to fix"))
    (let* ((before (on-brand-check "text" "NIMBUS shipped it" "brand" "nimbus"))
           (fixed (on-brand-check "text" "NIMBUS shipped it" "brand" "nimbus" "fix" t)))
      (is (some (lambda (detail) (search "should be 'Nimbus'" detail)) (on-brand-violations before)) "the pinned spelling is a soft rule")
      (is (equal "Nimbus shipped it" (nlk:json-value fixed :string "fixed_text")) "and fix=true repairs it")
      (is (< (nlk:json-value before :integer "score") (nlk:json-value fixed :integer "fixed_score")) "the repaired copy scores higher"))))

(deftest on-brand-cell-a-verdict-is-the-same-every-time ()
  (on-brand-empty)
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (on-brand-save "brand" "acme" "person" "third" "banned" #("synergy") "max_emojis" 0)
    (let* ((one (on-brand-check "text" "We bring synergy. 🚀" "brand" "acme"))
           (two (on-brand-check "text" "We bring synergy. 🚀" "brand" "acme")))
      (is (equal (on-brand-violations one) (on-brand-violations two)) "the same copy gets the same verdict")
      (is (= (nlk:json-value one :integer "score") (nlk:json-value two :integer "score"))))))

;;; --- the store survives a restart ----------------------------------------------

(deftest on-brand-cell-a-profile-is-a-file-and-outlives-the-cell ()
  (on-brand-empty)
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (on-brand-save "brand" "pixel-pals" "display_name" "Pixel Pals" "person" "first" "tone" "playful")
    (is (probe-file (nodecode-on-brand::state-file)) "the profile is a file in the folder"))
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (let ((listed (nodecode-on-brand::run-command "list" nil)))
      (is-carrying (text listed) ("Pixel Pals (pixel-pals)")))
    (is-carrying (text (nodecode-on-brand::run-command "show pixel-pals" nil))
                 ("Pixel Pals")
                 ("person: first")
                 ("tone: playful"))))

;;; --- /on-brand -----------------------------------------------------------------

(deftest on-brand-cell-the-command-switches-and-edits-a-brand ()
  (on-brand-empty)
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (is-carrying (text (nodecode-on-brand::run-command "starter" nil)) ("Nimbus Protocol" "nimbus"))
    (is-carrying (text (nodecode-on-brand::run-command "use nimbus" nil)) ("ON for Nimbus Protocol"))
    (is-carrying (text (nodecode-on-brand::run-command "" nil)) ("ON for Nimbus Protocol") ("gate=on"))
    (is-carrying (text (nodecode-on-brand::run-command "list" nil)) ("* Nimbus Protocol (nimbus)"))
    (is-carrying (text (nodecode-on-brand::run-command "ban nimbus unlock" nil)) ("unlock"))
    (is-carrying (text (nodecode-on-brand::run-command "check We will unlock the future" nil))
                 ("Nimbus Protocol:") ("banned word 'unlock'"))
    (is-carrying (text (nodecode-on-brand::run-command "set nimbus max_chars 40" nil))
                 ("max_chars: 40"))
    (is-carrying (text (nodecode-on-brand::run-command "off" nil)) ("On-Brand is off."))
    (is (null (nodecode-on-brand::active-profile)) "and nothing is enforced")
    (is-carrying (text (nodecode-on-brand::run-command "delete nimbus" nil)) ("Deleted nimbus."))
    (is (null (nodecode-on-brand::find-profile "nimbus")) "the profile is gone")))

;;; --- the standing section, and the gate ----------------------------------------

(deftest on-brand-cell-the-active-brand-s-rules-ride-every-request ()
  (on-brand-empty)
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (is (null (assoc "on-brand" (nle::read-harness-sections "s1") :test #'equal))
        (format nil "nothing rides a request while no brand is on; active=~s profiles=~s"
                (nodecode-on-brand::active-name)
                (mapcar (lambda (one) (nodecode-on-brand::profile-field one :name))
                        (nodecode-on-brand::profiles))))
    (on-brand-save "brand" "nimbus" "display_name" "Nimbus Protocol" "person" "third"
                   "banned" #("LFG") "max_chars" 280)
    (nodecode-on-brand::use-brand "nimbus")
    (let ((section (cdr (assoc "on-brand" (nle::read-harness-sections "s1") :test #'equal))))
      (is (stringp section) "the section is there once a brand is on")
      (is-carrying (text section)
                   ("Nimbus Protocol")
                   ("Never 'we', 'us' or 'our'")
                   ("LFG")
                   ("```copy" "where the copy goes")))))

(deftest on-brand-cell-the-gate-refuses-an-off-brand-call-and-passes-clean-copy ()
  (on-brand-empty)
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (on-brand-save "brand" "nimbus" "display_name" "Nimbus Protocol" "person" "third"
                   "banned" #("LFG") "packs" #("crypto"))
    (nodecode-on-brand::use-brand "nimbus")
    (is (nodecode-on-brand::outbound-p "send_message") "the call's name reads as outbound")
    (is (nodecode-on-brand::active-profile) "a brand is active")
    (multiple-value-bind (refusal ran)
        (on-brand-call "send_message" (nlk:json-object "text" "We are going to the moon. LFG"))
      (is (null ran) "the call never reached the tool")
      (is-carrying (text refusal)
                   ("ERROR: on-brand refused send_message")
                   ("Nimbus Protocol's brand rules")
                   ("banned word 'LFG'")
                   ("call send_message again")))
    (multiple-value-bind (answer ran)
        (on-brand-call "send_message"
                       (nlk:json-object "text" "Nimbus Protocol opened staking to its community. NFA."))
      (is ran "clean copy goes through")
      (is (equal "SENT" answer)))
    (multiple-value-bind (answer ran)
        (on-brand-call "read_file" (nlk:json-object "path" "We are going to the moon. LFG"))
      (is ran "a tool that sends nothing is never gated")
      (is (equal "SENT" answer)))
    (is-carrying (text (nodecode-on-brand::run-command "log" nil)) ("blocked") ("send_message"))))

;;; --- learn, and the voiceover ---------------------------------------------------

(deftest on-brand-cell-learn-drafts-a-profile-from-approved-posts ()
  (on-brand-empty)
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (let* ((answer (on-brand-answer
                    (on-brand-tool "onbrand_learn"
                                    (nlk:json-object
                                     "brand" "rialo"
                                     "display_name" "Rialo"
                                     "samples" #("Rialo opened its testnet today. #Rialo"
                                                 "Rialo's team shipped staking. The price is what it is. #Rialo #DeFi 🚀"
                                                 "Rialo is live. #Rialo")))))
           (saved (nlk:json-value answer :object "saved")))
      (is (equal "Rialo" (nlk:json-value saved :string "display_name")))
      (is (equal "third" (nlk:json-value saved :string "person")) "the brand talks about itself by name")
      (is (= 1 (nlk:json-value saved :integer "max_emojis")) "the ceiling is the highest any post used")
      (is (= 2 (nlk:json-value saved :integer "max_hashtags")))
      (is (equal '("crypto") (coerce (nlk:json-value saved :array "packs") 'list)) "a post about a price brings the crypto pack")
      (is (search "eview" (nlk:json-value answer :string "review")) "and the profile is for review"))
    (is (signals-error nodecode-on-brand:on-brand-error
          (on-brand-tool "onbrand_learn" (nlk:json-object "brand" "solo" "samples" #("one post")))) "one post is not enough to learn a voice")))

(deftest on-brand-cell-a-voiceover-script-has-no-markdown-and-spells-numbers ()
  (on-brand-empty)
  (with-cell-stop ((on-brand-start "store_dir" (on-brand-folder)))
    (let* ((answer (on-brand-answer
                    (on-brand-tool "onbrand_voiceover"
                                    (nlk:json-object "text" "## Nimbus
**Nimbus Protocol** shipped it to 1,200 people. See [the post](https://example.com/x). 🚀" "ssml" t))))
           (script (nlk:json-value answer :string "script")))
      (is-carrying (text script)
                   ("Nimbus Protocol shipped it to one thousand two hundred people")
                   (:absent "**" "no markdown left")
                   (:absent "##")
                   (:absent "example.com")
                   (:absent "🚀" "no emoji read aloud"))
      (is (search "break time" script) "ssml adds the pauses it was asked for")
      (is (plusp (nlk:json-value answer :integer "words")) "the script counts its words"))))
