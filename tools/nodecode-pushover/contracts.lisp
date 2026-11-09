;;;; contracts.lisp --- the one tool's arguments.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for the tool's shape: the JSON Schema the provider is
;;;; shown is projected from this, the value the body runs on is materialized
;;;; from it, and a call that does not satisfy it is refused before the body
;;;; sees it.

(in-package #:nodecode-pushover)

(nlk:define-contract notification
  (:doc "Send a push notification to the operator's phone or desktop through Pushover. Use it for something the person should know now and is not watching the session for: a long job finished, a deploy went out, a check failed, a decision is waiting. The message is the notification's body — plain text, no markdown, at most 1024 characters, and it is read on a lock screen, so lead with the fact. Do not narrate: the transcript already has everything, so send only what is worth an interruption. Priority 2 repeats until the person acknowledges it, so keep it for something that cannot wait.")
  ("message" :text :required t
   :doc "the notification's body, plain text, at most 1024 characters: the fact, first")
  ("title" :string :default ""
   :doc "a short headline over the body (at most 250 characters); the cell's own title when empty")
  ("priority" :integer
   :doc "-2 no banner or sound, -1 quiet, 0 normal (the default), 1 high, 2 emergency: repeats every retry seconds until acknowledged and needs retry and expire")
  ("device" :string :default ""
   :doc "deliver to one of the account's devices by name; empty means every device it is installed on")
  ("sound" :string :default ""
   :doc "Pushover's name for the sound to play (pushover, bike, cosmic, siren...); empty means the device's own choice")
  ("url" :string :default ""
   :doc "a web address the notification opens when it is tapped")
  ("url_title" :string :default ""
   :doc "the label for that link; empty shows the address itself")
  ("retry" :integer
   :doc "priority 2 only: seconds between repeats, 30 or more; 60 when empty")
  ("expire" :integer
   :doc "priority 2 only: seconds before the repeats stop, at most 10800; 3600 when empty"))
