;;;; package.lisp --- the VIDIQ package: the verbs, their condition, the settings.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed VIDIQ so an eval form reads
;;;; (vidiq:channel-videos "UC..."). The service's own tools are named
;;;; vidiq_channel_videos; each verb here is one of them.
;;;;
;;;; NLK:DEFINE-PERIPHERAL declares VIDIQ-ERROR, FAIL, *VIDIQ*,
;;;; RUNNING-SETTINGS and DEFINE-VERB, which the files after this one read.
;;;;
;;;; Rewritten from vid-io/vidiq-mcp's Hermes plugin (see NOTICE): its MCP
;;;; connection (.mcp.json) became mcp.lisp, and the tool vocabulary its
;;;; twelve skills call became the verbs of verbs.lisp.

(defpackage #:nodecode-vidiq
  (:use #:cl)
  (:nicknames #:vidiq)
  (:export #:user-channels #:balance #:channel-search #:channel-videos #:channel-analytics
           #:channel-trends #:outliers #:outlier-search #:trending-videos #:keyword-research
           #:video-comments #:video-transcript #:video-stats #:similar-channels
           #:videos-by-ids #:job-poll #:jobs-list #:generate-titles #:generate-thumbnail
           #:score-title #:score-thumbnail #:update-competitors
           #:call-tool #:tools #:tools-list #:endpoint #:vidiq-error))

(in-package #:nodecode-vidiq)

(nlk:define-peripheral vidiq :not-running t)

(defparameter +endpoint+ "https://mcp.vidiq.com/mcp"
  "The hosted MCP endpoint, what the section defaults to: the plugin's only outbound host.")

(defparameter +protocol-version+ "2025-06-18"
  "What the initialize handshake asks for, and the header every later call carries.")

(defparameter +client-version+ "0.1.0"
  "What the client info and the user agent name, as the plugin's VERSION does.")

(defparameter +sign-in+ "https://vidiq.com/mcp/"
  "Where a user reads how to connect a vidIQ account.")
