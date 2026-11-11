;;;; data.lisp --- the list, as the poster published it on 10 November 2026.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A snapshot, not a feed: the poster's own categories and tiers, in the
;;;; poster's own words where a note is given. Nothing here is a vouch. These
;;;; are places where people sell and talk about malware, stolen data, carding
;;;; and cracking, and most of them trade on trust between strangers.

(in-package #:nodecode-forum-index)

(defparameter +source+
  "Dread /d/hacking post b2a0fc81f18dbd6f2068, 10 Nov 2026, relayed on X by @solminingpunk"
  "Where the list comes from, as a person would cite it.")

(defparameter +legend+
  '(("malware" . "high-quality discussion and services about malware")
    ("leaks" . "extensive data leaks in the community")
    ("fraud" . "quality vendors and discussion involved in fraud")
    ("hacking" . "high-quality discussion and services for hacking")
    ("cracking" . "discussion and services for cracking software")
    ("seo" . "search-engine work: accounts, links, ranking")
    ("development" . "development and reverse engineering")
    ("accounts" . "accounts as the trade: mail, social, streaming"))
  "What each category in the list means, in the poster's sense.")

(defparameter +tiers+
  '((:established . "around for a significant period and considered active; not vouched, and no less likely to be seized")
    (:rising . "established, but issues or concerns have been raised: treat with more care")
    (:other . "listed without a tier; not vouched")
    (:dangerous . "the poster marks these dangerous outright"))
  "The list's own tiers, as a sentence each.")

(defparameter +forums+
  (list
   ;; --- established -------------------------------------------------------------
   (list :name "Exploit" :tier :established
         :categories '("fraud" "leaks" "malware")
         :urls '("https://exploit.in" "https://exploit.biz")
         :onion "https://exploitivzcm5dawzhe6c32bbylyggbjvh5dyvsvb5lkuz5ptmunkmqd.onion"
         :note "a registration fee to join")
   (list :name "BHF" :tier :established
         :categories '("fraud" "leaks")
         :urls '("https://bhf.pro")
         :onion "http://bhf2b5nb3lb2kxpaoyqz7cuk2dkgej5n2refuffxzyhldwt4d7de4zqd.onion"
         :note "was BadHackerForum; a fraud and leaks market")
   (list :name "Lolz Forum" :tier :established
         :categories '("leaks" "accounts")
         :urls '("https://lolz.live")
         :onion nil
         :note "the accounts market: mail and social accounts by the thousand, and the forum the poster's list leaves under-weight")
   (list :name "BDF" :tier :established
         :categories '("fraud")
         :urls '("https://bdfclub.com")
         :onion nil
         :note nil)

   ;; --- rising, with concerns ------------------------------------------------------
   (list :name "Darkforums" :tier :rising
         :categories '("fraud" "leaks")
         :urls '("https://darkforums.su")
         :onion "http://darkfoqaxhfpxrbt7vxns2zu2k72sgmqbzeorupaioftw3ecm2wgyd.onion"
         :note "compromised in 2023, its original administrators identified by security companies, and the current owner is probably the original owner; an inexperienced administration team")
   (list :name "DamageLib" :tier :rising
         :categories '("leaks")
         :urls '()
         :onion "http://damaga377yvydeqeuiqvxl6g5sbmipxob5sne6gpj3sisbnsbhvrdq.onion/forum/"
         :note "no marketplace; most of XSS's users moved here, and it is still short of what XSS was before the seizure")

   ;; --- other forums ---------------------------------------------------------------
   (list :name "Black Hat Pro Tools" :tier :other :categories '("seo") :urls '("https://www.blackhatprotools.info") :onion nil :note nil)
   (list :name "BlackHat World" :tier :other :categories '("seo") :urls '("https://www.blackhatworld.com") :onion nil :note nil)
   (list :name "Crackia" :tier :other :categories '("leaks" "hacking") :urls '("https://crackia.com") :onion nil :note nil)
   (list :name "CrackingX" :tier :other :categories '("leaks" "hacking") :urls '("https://crackingx.com") :onion nil :note nil)
   (list :name "Crd Crew" :tier :other :categories '("fraud") :urls '("https://crdcrew.cc") :onion nil :note nil)
   (list :name "CrdPro" :tier :other :categories '("fraud") :urls '("https://crdpro.cc") :onion nil :note nil)
   (list :name "Cyberarsenal" :tier :other :categories '("hacking") :urls '("https://cyberarsenal.org") :onion nil :note nil)
   (list :name "Cyberforum RU" :tier :other :categories '("leaks" "hacking") :urls '("https://www.cyberforum.ru") :onion nil :note nil)
   (list :name "Cyber Nulled" :tier :other :categories '("hacking") :urls '("https://cybernulled.com") :onion nil :note nil)
   (list :name "DarkForest" :tier :other :categories '("hacking") :urls '()
         :onion "http://dkforestseeaaq2dqz2uflmlsybvnq2irzn4ygyvu53oazyorednviid.onion" :note nil)
   (list :name "DarkStash" :tier :other :categories '("fraud") :urls '("https://darkstash.com") :onion nil :note nil)
   (list :name "Enclave" :tier :other :categories '("fraud") :urls '("https://www.enclave.cc") :onion nil :note nil)
   (list :name "ExeTools" :tier :other :categories '("development") :urls '("https://forum.exetools.com") :onion nil :note nil)
   (list :name "EZ Carder" :tier :other :categories '("fraud") :urls '("https://ezcarder.cc") :onion nil :note nil)
   (list :name "HackTown" :tier :other :categories '("hacking") :urls '()
         :onion "http://hacktowns3sba2xavxecm23aoocvzciaxirh3vekg2ovzdjgjxedfvqd.onion" :note nil)
   (list :name "Go4Expert" :tier :other :categories '("development") :urls '("https://www.go4expert.com/forums") :onion nil :note nil)
   (list :name "Indetectables" :tier :other :categories '("hacking") :urls '("https://indetectables.net/index.php") :onion nil :note nil)
   (list :name "LeakZone" :tier :other :categories '("hacking") :urls '("https://leakzone.net") :onion nil :note nil)
   (list :name "LeakedBB" :tier :other :categories '("leaks") :urls '("https://leakedbb.com") :onion nil :note nil)
   (list :name "Legit Carder" :tier :other :categories '("fraud") :urls '("https://legitcarder.ru") :onion nil :note nil)
   (list :name "Memory Hackers" :tier :other :categories '("hacking") :urls '("https://memoryhackers.org") :onion nil :note nil)
   (list :name "Niflheim" :tier :other :categories '("hacking" "fraud") :urls '("https://niflheim.world/") :onion nil :note nil)
   (list :name "Noirth" :tier :other :categories '("leaks" "hacking") :urls '("https://noirth.com") :onion nil :note nil)
   (list :name "Nsane Forums" :tier :other :categories '("development") :urls '("https://nsaneforums.com") :onion nil :note nil)
   (list :name "Patched" :tier :other :categories '("hacking" "cracking") :urls '("https://patched.to") :onion nil :note nil)
   (list :name "Voided" :tier :other :categories '("hacking" "cracking") :urls '("https://voided.to") :onion nil :note nil)
   (list :name "Piratehub" :tier :other :categories '("hacking") :urls '("https://go.piratehub.bz") :onion nil :note nil)
   (list :name "PwnForums" :tier :other :categories '("hacking") :urls '("https://pwnforums.st") :onion nil :note nil)
   (list :name "RomHacking" :tier :other :categories '("hacking") :urls '("https://www.romhacking.net/forum") :onion nil :note nil)
   (list :name "RST Forums" :tier :other :categories '("hacking") :urls '("https://rstforums.com/forum/") :onion nil :note nil)
   (list :name "Sinisterly" :tier :other :categories '("hacking") :urls '("https://www.sinister.ly") :onion nil :note nil)
   (list :name "SEO Pirat" :tier :other :categories '("seo") :urls '("https://www.seopirat.club") :onion nil :note nil)
   (list :name "Spear" :tier :other :categories '("hacking") :urls '("https://www.spear.cx") :onion nil :note nil)
   (list :name "SoldierX" :tier :other :categories '("hacking") :urls '("https://www.soldierx.com/bbs") :onion nil :note nil)
   (list :name "SzeneBox" :tier :other :categories '("hacking") :urls '("https://www.szenebox.org") :onion nil :note nil)
   (list :name "TurkHacks" :tier :other :categories '("hacking") :urls '("https://www.turkhacks.com/") :onion nil :note nil)
   (list :name "Underc0de" :tier :other :categories '("hacking") :urls '("https://underc0de.org/foro") :onion nil :note nil)
   (list :name "Valid Market" :tier :other :categories '("fraud") :urls '("https://validmarket.io/") :onion nil :note nil)
   (list :name "VeryLeak's" :tier :other :categories '("leaks") :urls '("https://veryleaks.cz") :onion nil :note nil)
   (list :name "Wilder Security" :tier :other :categories '("hacking") :urls '("https://www.wilderssecurity.com/") :onion nil :note nil)
   (list :name "YouHack" :tier :other :categories '("hacking" "fraud") :urls '("http://www.youhack.vip") :onion nil :note nil))
  "Every forum the post lists, with the poster's tier, categories and note.")
