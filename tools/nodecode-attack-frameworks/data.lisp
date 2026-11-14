;;;; data.lisp --- five LLM-driven attack frameworks, and what they leave in a log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every fact here is the DFIR Radar post of 13 November 2026 (the frameworks,
;;;; their default posture, the hunt strings, the revocation note) read against
;;;; each repository's own GitHub metadata on 14 November 2026 (stars, licence,
;;;; language, last push). Nothing is inferred and nothing is a vouch: a fork
;;;; can change any default in any release, which the post says itself.

(in-package #:nodecode-attack-frameworks)

(defparameter +source+
  "DFIR Radar (@DFIR_Radar) post 2121287724430774519, 13 Nov 2026, read 14 Nov 2026"
  "Where the frameworks, their posture and the hunt strings come from.")

(defparameter +captured+ "14 Nov 2026"
  "The day the post was read and the repositories' metadata taken.")

(defparameter +thesis+
  "The tools are ordinary: scanners, curl, shells, cloud CLIs. What is new is a model planning each
step and splitting the work across agents, so the run looks like a person's session at machine speed.
None of the five enforces authorization in code by default, and three tell the model it is already
authorized."
  "The post's own reading, which is why the cell is worth having.")

(defparameter +revocation+
  "Blocking a source is not containment. ARTEX and CyberStrikeAI change route when blocked, and GTIG
relaying Wiz Research on UNC7228 reports a fallback to open proxies. Revocation also lags: Entra
tokens issued before a disable typically live 60 to 90 minutes without continuous access evaluation,
and AWS role-session revocation leaves IAM users' long-term keys alone. After any source block, revoke
the identity's sessions and tokens and then confirm that the next request fails."
  "The post's containment note, which outlives any one framework.")

(defparameter +takeaway+
  "Detect tempo and sequence per identity: count distinct sensitive actions per identity and issuing
role in short windows, then join discovery, secret access and remote execution under one role or
source address across sessions, with a long lookback, because an operation can run for hours.
The post's ES|QL hunt flags one-minute windows with three or more client families (curl, Python,
browser, AWS client) and a request burst -- three families and 20 requests a minute. Baseline build
systems, admin AWS CLI use and shared NAT first, or the hunt is all noise."
  "What a defender does with the table below.")

(defparameter +rot+
  "Hunt strings rot. ARTEX logged about 390 commits in ten and a half weeks, and several frameworks
write or install tools mid-task, so a User-Agent or a port may move between releases. What stayed
stable in the post's observation is behaviour: credential access, remote execution, and offensive
tools started by a service."
  "How much weight to put on the strings below.")

(defparameter +tell-text+
  "What to do with it: count distinct sensitive actions per identity per short window, and join
discovery, secret access and remote execution under one role or source address (see hunt, and the
detection note in each entry)."
  "The one line that turns the table into a hunt.")

(defparameter +frameworks+
  (list
   (list :name "PentAGI"
         :repo "vxcontrol/pentagi"
         :url "https://github.com/vxcontrol/pentagi"
         :stars 34149
         :licence "MIT"
         :language "Go"
         :pushed "2026-11-13"
         :created "2025-01-06"
         :what "A fully autonomous multi-agent pentest system: a coordinated set of agents that plan, run the usual tooling and write the report."
         :authorization "Its Compose file sets ASK_USER to false, so no step waits for a person's yes. It tells the model it is already authorized."
         :evidence "GTIG and Mandiant observed PentAGI in incident-response engagements."
         :hunt (list (cons "network" "connections to TCP 28000 to 29999 on the PentAGI host")
                     (cons "network" "pages requested twice at once")
                     (cons "network" "from a PentAGI installation on your network, check-ins to update.pentagi[.]com every three hours")))
   (list :name "HexStrike"
         :repo "0x4m4/hexstrike-ai"
         :url "https://github.com/0x4m4/hexstrike-ai"
         :stars 13588
         :licence "MIT"
         :language "Python"
         :pushed "2026-08-03"
         :created "2025-07-10"
         :what "An MCP server: it hands an agent 150+ security tools to call, so the model's reach is the tool list."
         :authorization "The post names no approval gate: what an agent may do is whatever the MCP tool surface exposes."
         :evidence "GTIG and Mandiant observed HexStrike in incident-response engagements."
         :hunt (list (cons "user-agent" "HexStrike-HTTP-Framework/1.0")
                     (cons "user-agent" "HexStrike-BrowserAgent/1.0")
                     (cons "marker" "<hexstrikeXSSTest123>")
                     (cons "process" "a Python server on port 8888 parenting the security tools it starts")))
   (list :name "CyberStrikeAI"
         :repo "AIPentest/CyberStrikeAI"
         :url "https://github.com/AIPentest/CyberStrikeAI"
         :stars 8629
         :licence "Apache-2.0"
         :language "Go"
         :pushed "2026-11-14"
         :created "2025-11-08"
         :what "An AI-native security platform with agent skills, a CVE corpus and its own tooling, driven from a web console."
         :authorization "It ships with approval mode off, so nothing asks before a step runs. A blocklist for government domains is the only default target restriction."
         :evidence "Elastic Security Labs traced an AI-assisted AWS intrusion to a CyberStrikeAI server."
         :hunt (list (cons "user-agent" "Mozilla/5.0 (compatible; CyberStrikeAI-WebShell/1.0), sent with a pass form field")
                     (cons "process" "a web server process running echo :OSPROBE_%OS%:END to fingerprint the host")
                     (cons "persistence" "a file or service named SystemUpdate, or com[.]apple.systemupdate")
                     (cons "persistence" "a .bashrc line ending in # cs")
                     (cons "persistence" "an [@]reboot cron entry")))
   (list :name "Cairn"
         :repo "oritera/Cairn"
         :url "https://github.com/oritera/Cairn"
         :stars 5144
         :licence "AGPL-3.0"
         :language "Python"
         :pushed "2026-09-07"
         :created "2026-04-19"
         :what "A general-purpose AI state-space search engine, validated first on autonomous penetration testing: it searches for a path to the goal rather than running a fixed script."
         :authorization "Its dispatcher sidelines a refusing worker for five seconds and hands the task to another, so a refusal costs the run five seconds and no more."
         :evidence "GTIG and Mandiant observed Cairn in incident-response engagements."
         :hunt (list (cons "network" "reverse shells and callbacks to the operator host's own address")))
   (list :name "ARTEX"
         :repo "jiwoochris/artex-global"
         :url "https://github.com/jiwoochris/artex-global"
         :stars 3298
         :licence "AGPL-3.0"
         :language "Go"
         :pushed "2026-11-09"
         :created "2026-10-03"
         :what "An LLM multi-agent autonomous penetration-testing framework in Go. The original repository came down in October 2026 and the forks carry it on; this is the most-starred of them."
         :authorization "It runs agents in a bypass permission mode and tells the model it is already authorized. Blocklists for government and education domains are the only default target restrictions."
         :evidence "GTIG and Mandiant observed ARTEX in incident-response engagements, and Microsoft attributed its use to Storm-0940."
         :hunt (list (cons "user-agent" "artex-enrich/1.0")
                     (cons "user-agent" "norma/0.4")
                     (cons "process" "nuclei, httpx, katana, ffuf and sqlmap traffic from one source in quick succession")
                     (cons "timing" "bursts of 2 to 25 seconds rather than a person's pacing")
                     (cons "accounts" "new application accounts followed by multi-session sign-ins")))))
