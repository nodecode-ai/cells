;;;; contracts.lisp --- the two boundaries: one command, and the machines.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; DECLARED BEFORE THE CODE THAT READS THEM, so a compiled reader of a call's
;;;; members meets the structure it reads (see src/CELLS.md, "Declaring a
;;;; boundary"). MACHINE.LISP builds the argv and the answer from these members;
;;;; CELL.LISP registers the tools against them.

(in-package #:nodecode-smol)

(nlk:define-contract smol-run
  (:doc "Run a shell command in a smol machine: a microVM on this computer with its own
Linux kernel, booted from an OCI image. The command's files, processes and packages are
the machine's, and it sees none of this computer's files, so this is the place for a
build, a package install, an untrusted script, or a destructive command you would rather
not run here. Read exit_code and output: a command that failed still answers, with ok
false. Name machine, or set keep, to find the same machine and its files on the next
call; leave both out for a machine removed when the command exits.")
  ("command" :text :required t
   :doc "the shell command, run by the machine's bash -c; a pipe, a loop and a heredoc are one command here")
  ("machine" :string
   :doc "the machine to run in, made from image when this is the first call to name it; the same name finds the same files next time")
  ("image" :string
   :doc "the OCI image a machine boots from, e.g. alpine or python:3.12-slim; the smol section's image by default")
  ("keep" :boolean
   :doc "keep this session's machine for its next call; naming a machine implies it")
  ("no_network" :boolean
   :doc "boot the machine with its network off, for a command that must reach nothing; the smol section's network setting otherwise")
  ("timeout_seconds" :integer
   :doc "this command's ceiling in seconds, the smol section's timeout_seconds by default; past it the command is killed and answers exit_code 124"))

(nlk:define-contract smol-inventory
  (:doc "List the smol machines on this computer: every machine the engine holds, one
name each, with the CLI's own listing in raw. Read it to find a machine an earlier call
made, or to see what a delete would free.")
  ("name" :string
   :doc "one machine to report on; every machine by default"))

(in-package #:nodecode-smol)
