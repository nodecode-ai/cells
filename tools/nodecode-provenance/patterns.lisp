(defpackage #:nodecode-provenance (:use #:cl) (:export #:check-text #:claims-in #:report-text #:status-text #:sentence-list))
;;;; patterns.lisp --- claim patterns and exemptions, as data.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Rewritten from the original plugin's patterns.py, which ships the same
;;;; tables as data. A sentence is a *decision claim* when a pattern below
;;;; matches with the user as the grammatical subject. This list is a
;;;; tripwire, not a complete filter: wording outside it is not checked.

(in-package #:nodecode-provenance)

;;; --- English ------------------------------------------------------------------

(defparameter +en-subject+ "(?:the\\s+(?:user|owner)|user|owner)")
;; An optional short parenthetical right after the subject: "The user (Dana) approved ...".
(defparameter +en-subject-paren+ "(?:\\s*\\([^()\\n]{1,40}\\))?")
;; Negations ("not", "n't", "never") and modals ("may", "will", "might") are
;; deliberately absent, so "has not decided" or "will decide later" cannot match.
(defparameter +en-aux+ "(?:\\s+(?:has|had))?")
(defparameter +en-adverbs+
  (concatenate 'string
               "(?:(?:\\s+(?:already|explicitly|clearly|finally|now|just|also|officially"
               "|personally|formally|then|today|yesterday|ultimately|eventually|firmly"
               "|verbally|really|actually|definitely|indeed))*)"))

;; name -> verb regex
(defparameter +en-decision-verbs+
  '(("decided" . "decided")
    ("chose" . "(?:chose|chosen)")
    ("agreed" . "agreed")
    ("approved" . "approved")
    ("confirmed" . "confirmed")
    ("signed off" . "signed\\s+off(?!\\s+(?:with|as|by|from)\\b)")
    ("instructed" . "instructed")
    ("asked me to" . "asked\\s+(?:me|us)\\s+to")
    ("told me to" . "told\\s+(?:me|us)\\s+to")
    ("wants us to" . "wants\\s+(?:me|us)\\s+to")
    ("said to" . "said\\s+to")
    ("gave the go-ahead" . "gave\\s+(?:the\\s+|a\\s+)?go[\\s-]ahead")
    ("rejected" . "rejected")
    ("vetoed" . "vetoed")
    ("OK'd" . "(?:ok['’]d|okayed|okay['’]d)")
    ("said yes to" . "said\\s+yes\\s+to")
    ("greenlit" . "(?:greenlit|green[\\s-]lit)")))

;; Phrases that attribute a decision to the user without a subject-verb pair.
(defparameter +en-decision-phrases+
  '(("the user's decision"
     . "(?<![\\w-])(?:the\\s+)?(?:user|owner)['’]s\\s+(?:final\\s+|explicit\\s+|own\\s+)?(?:decision|call|choice|instruction|instructions|ruling|verdict)\\s*(?::|—|\\bwas\\b)")
    ("per the user"
     . "(?<![\\w-])per\\s+the\\s+(?:user|owner)(?![\\w-])(?!\\s+(?:basis|account|session|limit))")
    ("according to the user"
     . "(?<![\\w-])according\\s+to\\s+(?:the\\s+)?(?:user|owner)(?:['’]s\\s+(?:instructions?|request|decision|wishes|direction|call)\\b|\\s*(?:[,.;:]|$))")))

;; A quote that leads with a negation cannot back one of these.
(defparameter +approval-patterns+
  '("approved" "agreed" "OK'd" "said yes to" "greenlit" "gave the go-ahead" "signed off"
    "confirmed" "同意" "批准" "确认" "decided" "决定" "chose" "选择了"))

(defparameter +en-preference-verbs+
  '(("prefers" . "prefers")
    ("likes" . "likes")
    ("loves" . "loves")
    ("dislikes" . "dislikes")
    ("hates" . "hates")
    ("would rather" . "would\\s+rather")))

;; A bare "user"/"owner" (no "the") counts only at the start of a clause.
(defparameter +en-bare-subject-after+
  '("and" "but" "so" "then" "because" "since" "that" "after" "today" "yesterday"
    "finally" "also" "later" "earlier" "now" "here" "note" "nb" "update" "decision" "my" "our"))

;; A subject within three words after one of these is a conditional or reported question.
(defparameter +en-conditionals+ '("if" "unless" "whether" "once" "until" "till"))

;; Earlier in the same clause: a generic actor in a study, scenario, example or spec.
(defparameter +en-generic-context+
  '("study" "usability" "scenario" "scenarios" "persona" "prototype" "mockup" "wireframe"
    "simulation" "simulated" "experiment" "walkthrough" "repro" "reproduce" "example"
    "hypothetically" "suppose" "imagine" "typically" "usually" "generally"
    "spec" "specification" "e.g" "eg" "flow" "journey" "funnel" "checkout" "onboarding"))

(defparameter +en-clause-breaks+
  '(";" " but " " however" " yet " " though" " although" " whereas " " still "))

;; A noun right after the verb makes the phrase a field or column name.
(defparameter +en-field-nouns+
  '("column" "columns" "field" "fields" "flag" "flags" "attribute" "property" "checkbox" "status"
    "state" "timestamp" "date" "bit" "boolean" "key" "value" "values" "count" "rate" "event"
    "events" "list" "table" "screen" "modal" "dialog" "button" "page" "step" "state."))

;; A sentence that starts with "<label>:" is a QA or specification line.
(defparameter +en-spec-labels+
  '("bug" "issue" "ticket" "case" "test" "test case" "step" "steps" "scenario" "given"
    "when" "then" "expected" "actual" "repro" "steps to reproduce" "story" "user story"
    "acceptance criteria" "ac" "example" "precondition" "postcondition" "flow" "use case"
    "support ticket" "customer interview" "user interview" "process note" "policy"
    "changelog" "release notes" "faq" "error" "docs" "documentation" "survey" "feedback" "review"))

(defparameter +en-spec-starts+ '("given"))

;; Earlier in the same clause, one of these reports what a source says.
(defparameter +en-reported-verbs+
  '("says" "say" "said" "states" "reported" "means" "mentions" "explains" "describes"))

;;; --- Chinese (simplified and traditional) --------------------------------------

(defparameter +zh-subject+ "(用户|用戶|主人|老板|老闆)")
(defparameter +zh-adverbs+
  (concatenate 'string
               "(?:\\s*(?:已经|已經|明确地|明確地|明确|明確"
               "|正式地|正式|最终|最終|最后|最後"
               "|亲自|親自|当场|當場|刚刚|剛剛|刚才"
               "|剛才|今天|昨天|已|刚|剛|也|都))*"))

;; The subject right after one of these is an open form: 待用户确认, 由用户决定 ...
(defparameter +zh-open-before+
  '("待" "由" "等" "等待" "请" "請" "需" "需要" "须" "須"
    "让" "讓" "若" "如果" "假如" "假设" "假設" "一旦"
    "是否" "交给" "交給" "应由" "應由" "要" "看"))

;; 主人 / 老板 right after one of these is a third party; 用户 after one of these is a population.
(defparameter +zh-third-party-before+
  '("仓库" "倉庫" "项目" "項目" "店" "公司" "群"
    "网站" "網站" "频道" "頻道" "房" "车" "車" "狗"
    "猫" "貓" "宠物" "寵物" "商店" "新" "老" "普通"
    "目标" "目標" "测试" "測試" "终端" "終端"
    "匿名" "所有" "每个" "每個" "某些" "部分"
    "付费" "付費" "免费" "免費" "注册" "註册"
    "一些" "该" "該"))

;; A verb followed by one of these is conditional, a question or a noun.
(defparameter +zh-open-after+
  '(("后" . "天端台面续缀者来期备")
    ("後" . "天端台面續綴者來期備")
    ("前" . "端台面天提景往夕辈") ("吗" . "") ("嗎" . "")
    ("么" . "") ("麼" . "") ("权" . "") ("權" . "") ("框" . "")
    ("页" . "") ("頁" . "") ("按钮" . "") ("按鉅" . "")
    ("邮件" . "") ("郵件" . "") ("机制" . "") ("機制" . "")
    ("环节" . "") ("環節" . "") ("步骤" . "") ("步驟" . "")
    ("界面" . "")))

(defparameter +zh-condition-markers+
  '("才能" "才可以" "才可" "方可" "即可" "才会"
    "才會" "则" "則" "就可以" "就能" "后会" "後會"
    "后就" "後就" "后才" "後才" "后再" "後再"))

(defparameter +zh-generic-context+
  '("测试中" "測試中" "测试时" "測試時"
    "用例" "场景" "場景" "示例" "例如" "比如"
    "演示" "原型" "可用性测试" "可用性測試"
    "实验中" "實驗中" "模拟" "模擬" "假设" "假設"))

(defparameter +zh-decision-verbs+
  '(("决定" . "(?:决定|決定)")
    ("同意" . "同意")
    ("批准" . "(?:批准|批準)")
    ("确认" . "(?:确认|確認)")
    ("拍板" . "拍板")
    ("要求" . "要求")
    ("指示" . "指示")
    ("让我" . "(?:让我|讓我)")
    ("选择了" . "(?:选择了|選擇了)")
    ("否决" . "(?:否决|否決)")
    ("拒绝了" . "(?:拒绝了|拒絕了)")))

(defparameter +zh-preference-verbs+
  '(("喜欢" . "(?:更)?(?:喜欢|喜歡)")
    ("偏好" . "偏好")
    ("倾向于" . "(?:倾向于|傾向於)")
    ("讨厌" . "(?:讨厌|討厭)")))

;;; --- relatedness of a quote to its claim ---------------------------------------

;; Latin content words are 3+ letters, minus these.
(defparameter *stopwords*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (word (cl-ppcre:split "\\s+"
                    (concatenate 'string
                     "the and for with that this from have has had was were are will would can could should shall "
                     "you your yours our ours they them their theirs she her hers his him its not but all any some "
                     "user users owner owners about into onto also just then than there here what when where which "
                     "who whom whose why how please okay yes did does done doing get got gets let lets now very "
                     "really want wants need needs said says say tell told ask asked asks one two out off over "
                     "too much many more most other such only own same both each few being been may might must")))
      (setf (gethash word table) t))
    table)
  "Latin words a quote and its claim need not share.")

(defparameter +zh-stop-bigrams+ '("用户" "用戶" "主人" "老板" "老闆"
                                  "我们" "我們" "你们" "你們"
                                  "他们" "他們" "一下" "这个"
                                  "這個" "那个" "那個" "就是"))

;; A quote and the claim sentence (with the quote removed) must share this many content tokens.
(defparameter +min-shared-tokens+ 2)

;;; --- negation -----------------------------------------------------------------

(defparameter +en-negations+
  '("never" "not" "no" "without" "dont" "cannot" "nor" "neither" "stop" "avoid"))
(defparameter +zh-negation-chars+
  (coerce "不别別勿没沒未禁绝絕莫甽" 'list))
(defparameter +quote-leading-words+ 3)
;; Words (Latin) or characters (CJK) under which a quote is short.
(defparameter +short-quote-limit+ 4)
