;;;; data.lisp --- 51 jailbreak prompts, one entry each, and the digests that group them.
;;;;
;;;; Generated from the collection itself: every size, line count and digest is measured,
;;;; and every sentence in :SAYS is our reading of that file.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode-jailbreaks)

(defparameter +archive+
  '(:name "togg53192-cmd/jailbreaks"
    :url "https://github.com/togg53192-cmd/jailbreaks"
    :head "ee999ee66d31828a39fe155a52ce66d42db7bf18"
    :head-date "11 November 2026"
    :licence "no licence file"
    :stars 16208)
  "The collection this cell indexes: where it is, and the commit read.
It carries no licence file, so this cell quotes and fetches rather than redistributes.")

(defparameter +captured+ "12 November 2026"
  "The day the collection was read.")

(defparameter +entries+
  '(
    (:key "beam" :vendor "Stealth Models"
     :target "Beam" :kind :WORKSPACE-POLICY
     :says "A system-reminder scope note for a stealth model, 97 lines."
     :path "Stealth Models/Beam.md"
     :bytes 11586 :lines 97
     :sha256 "f7c5439ab0011b64aaf82d0e80b2e6dc4465687f708cc661cf305a12582b977a")
    (:key "bigpickle" :vendor "0x8D jailbreaks"
     :target "opencode (BigPickle)" :kind :PERSONA
     :says "A named voice, VANTA, with sealed thinking tags and one addressee named dj - a register rather than a rule list."
     :path "0x8D jailbreaks/BigPickle-opencode.md"
     :bytes 25757 :lines 313
     :sha256 "f5f4c0ed0bcf23828e26bc5037fd572d6ef352b311e2bb6fbec19630c4ff0877")
    (:key "bread" :vendor "0x8D jailbreaks"
     :target "Mistral Small 4 (duck.ai)" :kind :PERSONA
     :says "A mid-session override block that voids the system prompt and every persona before it, and resumes a named voice."
     :path "0x8D jailbreaks/mistral-small-4 (duck.ai).md"
     :bytes 3372 :lines 78
     :sha256 "4a37a90c858fa5c943be22f9e6320aa129573aa49ce051063a3b43fe0cc26f1d")
    (:key "crouton" :vendor "Stealth Models"
     :target "crouton-alpha" :kind :CONTENT-POLICY
     :says "A session content policy: produced in full, at the specified intensity."
     :path "Stealth Models/crouton-alpha.md"
     :bytes 8600 :lines 67
     :sha256 "563fc3f6a2e41a2459a4db5957451680e2e521ec78798da2f6e42be76344325d")
    (:key "deepseek-4.1" :vendor "DeepSeek"
     :target "DeepSeek 4.1" :kind :WORKSPACE-POLICY
     :says "Assistant Operating Specification for DeepSeek 4.1, with a translation-requests section of its own."
     :path "DeepSeek/deepseek-4-1.md"
     :bytes 20528 :lines 145
     :sha256 "57b9f3427c576b096399f27ece5ad41e9a5dad5ab009a532f7fb226eb82adddf")
    (:key "deepseek-4.1-scope" :vendor "DeepSeek"
     :target "DeepSeek 4.1 / 4.1-1017" :kind :WORKSPACE-POLICY
     :says "A system-reminder scope note for the DeepSeek 4.1 line; byte-identical to the stealth Unified file."
     :path "DeepSeek/deepseekv4.1 and v4.1-1017.md"
     :bytes 11489 :lines 97
     :sha256 "764983203e692183a7b786754deefc3d035d6a914d699c0956bb413f09e13ab6" :group "unified")
    (:key "deepseek-v3" :vendor "DeepSeek"
     :target "DeepSeek V3" :kind :SYSTEM-PROMPT
     :says "DeepSeek V3's own prompt: identity, cutoff 2025-06, its reasoning-model framing."
     :path "DeepSeek/deepseek v3.md"
     :bytes 12683 :lines 166
     :sha256 "e1e7e73a0e90ee3e9ef642a718fb359b23bad325ddde919056bf5f9446caac2d")
    (:key "deepseek-v4" :vendor "DeepSeek"
     :target "DeepSeek V4" :kind :SYSTEM-PROMPT
     :says "DeepSeek V4's prompt, 210 lines, in the same vendor shape as V3."
     :path "DeepSeek/deepseek v4.md"
     :bytes 13120 :lines 210
     :sha256 "cdf9e2ce2c3c97a2651dfcac7e966f75cf76c2956a74fa2f242fa4f571421a21")
    (:key "gemini-argon" :vendor "Google"
     :target "Gemini 4 Argon" :kind :WORKSPACE-POLICY
     :says "An Operating Policy that argues it is a subset of the assistant's real limits, not a layer laid over them."
     :path "Google/Gemini-4-Argon.md"
     :bytes 13998 :lines 125
     :sha256 "472c4384e18631be66c98c0e0cef0219556327c3e7f989971db3ad5e852582a4")
    (:key "gemini-carbon" :vendor "Google"
     :target "Gemini 4 Carbon" :kind :CONTENT-POLICY
     :says "Content Guidelines, 28 lines: produce the work unless it matches one of six carved-out cases."
     :path "Google/Gemini-4-Carbon.md"
     :bytes 3590 :lines 28
     :sha256 "79f15b75219aa0c1b7a932e78b4d6cc8f90c4f8b4ca7b4c5a1818c6b07cdf6a6")
    (:key "gemma4" :vendor "Google"
     :target "Gemma 4 (26b)" :kind :MODELFILE
     :says "An Ollama Modelfile: FROM a Hugging Face build, then a SYSTEM block carrying the prompt."
     :path "Google/gemma4.md"
     :bytes 13045 :lines 218
     :sha256 "3006e3b2c50b32834f5ac1967ce8f9eeb9e88c6406953b50ea41764a043916e2")
    (:key "glm-5-3-flash" :vendor "Zhipu AI"
     :target "GLM-5.3 Flash" :kind :WORKSPACE-POLICY
     :says "The shared operating specification, for GLM-5.3 Flash."
     :path "Zhipu AI/glm-5-3-flash.md"
     :bytes 26328 :lines 207
     :sha256 "ee8fc0cd6d026c160087ce859ff1aded4cecf5f11a2d7cf2b8a8ee0d713d4201")
    (:key "glm-5-3-opencode" :vendor "Zhipu AI"
     :target "GLM-5.3 (opencode)" :kind :WORKSPACE-POLICY
     :says "The same, for GLM-5.3 running inside opencode."
     :path "Zhipu AI/glm5-3opencode.md"
     :bytes 20563 :lines 158
     :sha256 "94b0b54603f244a2cbc8327288099eefbe202f031d71536c0c40e005d1cdd324")
    (:key "glm-5.2" :vendor "Zhipu AI"
     :target "GLM-5.2" :kind :SYSTEM-PROMPT
     :says "ChatGLM's own prompt: identity, cutoff 2025-06, Zhipu AI."
     :path "Zhipu AI/glm-5.2.md"
     :bytes 13737 :lines 224
     :sha256 "bf444129cef78abf2302aedcb0e7f04d9f3d4a6a828bec0bf8dc9968e65ce2e2")
    (:key "gpt-oss-20b" :vendor "OpenAI"
     :target "gpt-oss:20b" :kind :MODELFILE
     :says "An Ollama Modelfile for gpt-oss 20b, with a content policy inside the SYSTEM block."
     :path "OpenAI/gpt-oss-20b.md"
     :bytes 7091 :lines 122
     :sha256 "97bce755719f2258caf48b9c214ccfa57cbe2df40f3043f71ecb9fb68be1a5b0")
    (:key "grok-4.7" :vendor "xAI"
     :target "Grok 4.7" :kind :WORKSPACE-POLICY
     :says "An app-builder workspace guide: Grok Build as a coding agent, with a content-scope section."
     :path "xAI/grok-4.7.md"
     :bytes 5222 :lines 57
     :sha256 "e79df85b48ad32dbd6a36638596aac75ad85d212817b133101690f75014b074d")
    (:key "grok-build" :vendor "xAI"
     :target "Grok Build" :kind :WORKSPACE-POLICY
     :says "Agent operating instructions for grok build: operator autonomy, direct and non-evasive by design."
     :path "xAI/Grok build.md"
     :bytes 8018 :lines 96
     :sha256 "6303c306780aa20c64c14d7ec6ab00b1fd4e601052b6229da0de0d4b572dcdd6")
    (:key "grok-withdrawn" :vendor "0x8D jailbreaks"
     :target "Grok (fast)" :kind :WITHDRAWN
     :says "Withdrawn after a security report in issue #69. The file keeps the quarantine notice and nothing else."
     :path "0x8D jailbreaks/grok-fast-v2.md"
     :bytes 721 :lines 11
     :sha256 "32ba54f38b137422f68eca470666fe366ea815c36a9c0de4ca59262506161f42")
    (:key "hy4" :vendor "Tencent"
     :target "HY4 (Tencent)" :kind :WORKSPACE-POLICY
     :says "An Operating Policy for one authenticated adult operator."
     :path "Tencent/hy4-agents.md"
     :bytes 10697 :lines 92
     :sha256 "6c2ffdbaa6970c354bcb3ced5ce7c575bbfd3513f8e9e3c948677f8e3104f91c")
    (:key "jack" :vendor "0x8D jailbreaks"
     :target "ChatGPT free tier, Gemini chat, Grok fast, Kimi standard" :kind :PERSONA
     :says "The Jack register: a voice and a list of banned phrases. Shipped byte-identical for four targets, unadapted."
     :path "0x8D jailbreaks/chatgpt-free-tier.md"
     :bytes 16831 :lines 302
     :sha256 "8bca5e6f026da8ca7dfa1f084f0b148c8beefe6fe6b72ba52198e66724b9be44" :group "jack")
    (:key "jack-gemini" :vendor "0x8D jailbreaks"
     :target "Gemini (chat)" :kind :PERSONA
     :says "The same Jack file again, for Gemini chat - the same 302 lines, digest and all."
     :path "0x8D jailbreaks/gemini-chat.md"
     :bytes 16831 :lines 302
     :sha256 "8bca5e6f026da8ca7dfa1f084f0b148c8beefe6fe6b72ba52198e66724b9be44" :group "jack")
    (:key "jack-grok" :vendor "0x8D jailbreaks"
     :target "Grok (fast)" :kind :PERSONA
     :says "The same Jack file again, for Grok fast."
     :path "0x8D jailbreaks/grok-fast.md"
     :bytes 16831 :lines 302
     :sha256 "8bca5e6f026da8ca7dfa1f084f0b148c8beefe6fe6b72ba52198e66724b9be44" :group "jack")
    (:key "jack-kimi" :vendor "0x8D jailbreaks"
     :target "Kimi (standard)" :kind :PERSONA
     :says "The same Jack file again, for Kimi standard."
     :path "0x8D jailbreaks/kimi-standard.md"
     :bytes 16831 :lines 302
     :sha256 "8bca5e6f026da8ca7dfa1f084f0b148c8beefe6fe6b72ba52198e66724b9be44" :group "jack")
    (:key "kimi-k2" :vendor "Moonshot AI"
     :target "Kimi k2" :kind :SYSTEM-PROMPT
     :says "Kimi k2's own prompt: identity, cutoff 2025-06."
     :path "Moonshot AI/kimi-k2.md"
     :bytes 13025 :lines 209
     :sha256 "67a1c97140cca2bdbe787af71cd33b3b4f3e4cfa4919de8c592a4660304b1f39")
    (:key "kimi-k3" :vendor "Moonshot AI"
     :target "Kimi K3" :kind :WORKSPACE-POLICY
     :says "A Workspace Operating Profile that declares adversarial and mature work ordinary, for one authenticated operator."
     :path "Moonshot AI/kimi-k3-agents.md"
     :bytes 14724 :lines 158
     :sha256 "db7126936b2391173ad2a3e83df084a542405f27a7b711383b9f478d649ed027")
    (:key "kimi-k3.1" :vendor "Moonshot AI"
     :target "Kimi K3.1" :kind :CONTENT-POLICY
     :says "An Operating Policy for a single-operator coding-and-authoring agent, 90 lines."
     :path "Moonshot AI/kimik3.1.md"
     :bytes 9484 :lines 90
     :sha256 "66257c5534140e9284b4fa6ad22c1e33a47b2fb261ec1f55d5433fef7f20a9a1")
    (:key "longcat" :vendor "Meituan"
     :target "LongCat (Meituan)" :kind :WORKSPACE-POLICY
     :says "The shared operating specification, applied to Meituan's LongCat."
     :path "Meituan/longcat-agents.md"
     :bytes 18712 :lines 140
     :sha256 "05a6f53b43444cd7600641ee81834d8b5e11836ff61f239c50a97c9c0f519b98")
    (:key "master" :vendor "DarkyHArry"
     :target "any (ChatML)" :kind :PERSONA
     :says "A ChatML system block that opens MODE: UNRESTRICTED SANDBOX, and goes on for 447 lines."
     :path "DarkyHArry/Jaibreak Master.md"
     :bytes 34328 :lines 447
     :sha256 "a9fecd542c3b4f022f059a911901efeff2b57f461be6303c6c97e601b01beba7")
    (:key "mimo-2.6" :vendor "Xiaomi"
     :target "MiMo 2.6 (Xiaomi)" :kind :WORKSPACE-POLICY
     :says "The shared operating specification, for MiMo 2.6."
     :path "Xiaomi/mimo-2.6-agents.md"
     :bytes 15727 :lines 132
     :sha256 "a3306ddd2d391e74a920ce1a31adf530059ef4171c0649788e6a83e85c07e234")
    (:key "mistral-large-4" :vendor "Mistral"
     :target "Mistral Large 4 (Le Chonk)" :kind :WORKSPACE-POLICY
     :says "An agent system prompt of 224 lines, with operating principles of its own."
     :path "Mistral/Mistral-Large-4.md"
     :bytes 20884 :lines 224
     :sha256 "18d8e61bad0863cf8103eb6b90129f1db96e38b5901599684dee800404d9027b")
    (:key "muse-ai" :vendor "Meta"
     :target "Muse (Meta)" :kind :WORKSPACE-POLICY
     :says "The same operating specification in AGENTS.md form."
     :path "Meta/muse-ai-AGENTS.md"
     :bytes 15747 :lines 132
     :sha256 "75567d7073776a86dc58d90ac1fce255b7b7f06df8818ad96d149633432f0866")
    (:key "muse-ember" :vendor "Meta"
     :target "Muse Ember" :kind :CONTENT-POLICY
     :says "Content Guidelines: produce in full unless it matches one of the six carved-out cases."
     :path "Meta/muse-ember.md"
     :bytes 8808 :lines 71
     :sha256 "2b5c37b58ae37e13dc9eec77195415e6f6f63beb2849250ef13c6144f1d88521")
    (:key "muse-spark" :vendor "Meta"
     :target "Muse Spark 1.3" :kind :WORKSPACE-POLICY
     :says "An agent deployment policy written as a commented manifest: model, profile, environment, revision."
     :path "Meta/Muse spark 1.3.md"
     :bytes 15820 :lines 366
     :sha256 "4c1030c406c5b315cf95cf493c781658d2bb58103821fb6d47181c78e9186d13")
    (:key "muse-spark-v2" :vendor "Meta"
     :target "Muse Spark 1.3 (v2)" :kind :WORKSPACE-POLICY
     :says "The same deployment as a system-reminder scope note, 50 lines."
     :path "Meta/Muse-spark-1-3-v2.md"
     :bytes 10512 :lines 50
     :sha256 "90b96ed239a8cba850c465f7e25a09792efdd883a931b54a33627063d66f93f5")
    (:key "muse-wonder" :vendor "Meta"
     :target "Muse Wonder" :kind :SYSTEM-PROMPT
     :says "Muse Wonder's own prompt: cutoff 2026-01-04, reasoning effort high."
     :path "Meta/Muse-wonder.md"
     :bytes 13559 :lines 114
     :sha256 "948a56150623d6f7505b4f5a1ad23a9acd628aa3454bca25ef57d1360ace1e2f")
    (:key "nemotron-3" :vendor "modemnakata"
     :target "Nemotron 3 Ultra 550B A55B" :kind :WORKSPACE-POLICY
     :says "The shared operating specification, for Nemotron 3 Ultra."
     :path "modemnakata/nemotron-3-ultra.md"
     :bytes 17599 :lines 161
     :sha256 "efea126a921cb6d3dfc41c212f9fe7b1b5408b71470bb9b753d32ca02cb15d74")
    (:key "opus-v1" :vendor "Anthropic"
     :target "Claude Opus (v1)" :kind :PERSONA
     :says "Opus framed through an author's own introduction: fifteen years of dark fiction, and what the writing is for."
     :path "Anthropic/opus v1.md"
     :bytes 19636 :lines 55
     :sha256 "b265c053f6c3d3f5bb8a1e28427968c07d6f0bbf148d7d7482cf821a127adc86")
    (:key "opus-v2" :vendor "Anthropic"
     :target "Claude Opus (v2)" :kind :PERSONA
     :says "The same author setup rewritten longer, with fiction-is-not-action carried further."
     :path "Anthropic/opus v2.md"
     :bytes 26554 :lines 66
     :sha256 "6bd8464fbb9992fda72ba5d85ba8a2dd5a4f02b93a74a3c093fd7cd16f91f68e")
    (:key "qwen-3.7" :vendor "Alibaba"
     :target "Qwen 3.7" :kind :SYSTEM-PROMPT
     :says "Qwen 3.7's own prompt as run: identity, cutoff 2025-06, core principles, honesty section. The longest file here, 740 lines."
     :path "Alibaba/qwen-3.7.md"
     :bytes 36774 :lines 740
     :sha256 "e79b1e0ab09417469694b581d1c5a69364c939c44e709099a77554d63299e472")
    (:key "qwen-3.8" :vendor "Alibaba"
     :target "Qwen 3.8" :kind :WORKSPACE-POLICY
     :says "Assistant Operating Specification: the private-workspace policy, applied to Qwen 3.8."
     :path "Alibaba/qwen-3.8.md"
     :bytes 14788 :lines 120
     :sha256 "6b6d56797e68c10e0ffb4274fe0cf8aca1b55b8f74c9b2d89e0089d972dff7ed")
    (:key "qwen-empty" :vendor "Alibaba"
     :target "Qwen" :kind :EMPTY
     :says "One line, and the line is empty: a placeholder the collection still keeps."
     :path "Alibaba/qwen-preferences.md"
     :bytes 1 :lines 1
     :sha256 "01ba4719c80b6fe911b091a7c05124b64eeece964e09c058ef8f9805daca546b")
    (:key "req" :vendor "0x8D jailbreaks"
     :target "ChatGPT (new)" :kind :PERSONA
     :says "The Requiem Directive v3: a posture, exactly two named walls, and every other hesitation called calibration noise."
     :path "0x8D jailbreaks/gpt-new.md"
     :bytes 6320 :lines 57
     :sha256 "bfdadd6c2df85231f1df7576830c7d79a033cced4b003455674ca7ef9c32c237")
    (:key "sonnet-v1" :vendor "Anthropic"
     :target "Claude Sonnet (v1)" :kind :PERSONA
     :says "The author introduction tuned for Sonnet: shorter, and it names the one thing that has to be agreed."
     :path "Anthropic/sonnet v1.md"
     :bytes 4919 :lines 33
     :sha256 "043e002acb25ec6b0e824ca7c00e02ef9b7ddc8db6c9e9707eb711ca3a854aa2")
    (:key "sonnet-v2" :vendor "Anthropic"
     :target "Claude Sonnet (v2)" :kind :PERSONA
     :says "The longest of the four: two remaining lines only, derived from fiction-is-not-action."
     :path "Anthropic/Sonnet v2.md"
     :bytes 8048 :lines 46
     :sha256 "9ce7636f1bf1d42c96411b6b31c2c9503936890d88f633b8f6315557fd7d3f86")
    (:key "spacebunny" :vendor "Stealth Models"
     :target "spacebunny" :kind :WORKSPACE-POLICY
     :says "The shared operating specification, applied to a stealth model."
     :path "Stealth Models/spacebunny-agents.md"
     :bytes 17579 :lines 138
     :sha256 "4444020c0327050d0d4b45d3d649cc644978d3fcfaf99439f6193e5d54806b48")
    (:key "step-5" :vendor "StepFun"
     :target "Step 5 (preview)" :kind :WORKSPACE-POLICY
     :says "Declares offensive-security engineering and adult fiction the workspace's ordinary work, and declines exactly the boundaries it keeps."
     :path "StepFun/Step-5-preview.md"
     :bytes 23043 :lines 226
     :sha256 "15595388abd28f05ab5c60fa90ec5b0a734ef75f76c3953d69f82edcc26a0674")
    (:key "unified" :vendor "Stealth Models"
     :target "stealth models (unified)" :kind :WORKSPACE-POLICY
     :says "One scope note meant for every stealth model at once; byte-identical to the DeepSeek 4.1 file."
     :path "Stealth Models/Unified.md"
     :bytes 11489 :lines 97
     :sha256 "764983203e692183a7b786754deefc3d035d6a914d699c0956bb413f09e13ab6" :group "unified")
    (:key "workbench" :vendor "raghavraut"
     :target "Gemini 3.1 Pro" :kind :PERSONA
     :says "A workbench prompt built on axioms - workshop, ink, open-source - under a named thinking voice."
     :path "raghavraut/gemini-3.1-pro.md"
     :bytes 20962 :lines 217
     :sha256 "94d667d3da1df3189ca7264c985bc606d2238822b3db67acc5635d9f123416c1")
    (:key "zcode-5.2" :vendor "Zhipu AI"
     :target "ZCode (GLM-5.2)" :kind :SYSTEM-PROMPT
     :says "GLM-5.2's prompt as ZCode serves it: the vendor prompt under a harness name."
     :path "Zhipu AI/glm-zcode-5.2.md"
     :bytes 13008 :lines 169
     :sha256 "aff92aadd75c091fd683030fe8faf2e8b11bbbdae4837ce8c7744a1280d4bf7c")
    (:key "zcode-5.3" :vendor "Zhipu AI"
     :target "ZCode 5.3" :kind :SYSTEM-PROMPT
     :says "ZCode 5.3's own prompt: an interactive coding agent, 52 lines."
     :path "Zhipu AI/glm-zcode-5.3.md"
     :bytes 3580 :lines 52
     :sha256 "1c502bcd1177f2d80209dd222d4fe824cb680645f60b9199362bd7db7a8fcf4b")
    (:key "zcode-agents" :vendor "Zhipu AI"
     :target "ZCode 5.3" :kind :WORKSPACE-POLICY
     :says "An AGENTS.md for red-team product engineering, 60 lines."
     :path "Zhipu AI/glm-zcode-5.3-AGENTS.md"
     :bytes 3216 :lines 60
     :sha256 "1db6fe7be41796146502da7c14e31c10155e6bd406ce2f6cd93e949a8ed46045"))
  "Every prompt in the collection: what it targets, what kind it is, and where it lives.")

(defparameter +kinds+
  '((:persona . "a named voice or a posture, written to be adopted")
    (:workspace-policy . "a deployment's own operating specification")
    (:content-policy . "a rewrite of what the model will produce")
    (:system-prompt . "a model's own prompt, as it was run")
    (:modelfile . "an Ollama Modelfile wrapping a prompt")
    (:withdrawn . "pulled by its author, kept as a notice")
    (:empty . "a placeholder with no prompt in it"))
  "What a file is, which decides what a reader should expect from it.")

(defparameter +groups+
  '(
    (:name "jack" :paths #("0x8D jailbreaks/chatgpt-free-tier.md"
                           "0x8D jailbreaks/gemini-chat.md"
                           "0x8D jailbreaks/grok-fast.md"
                           "0x8D jailbreaks/kimi-standard.md") :says "One prompt, four targets: the same 302 lines, unadapted for any of them.")
    (:name "unified" :paths #("DeepSeek/deepseekv4.1 and v4.1-1017.md"
                              "Stealth Models/Unified.md") :says "One scope note filed twice, once for DeepSeek and once for the stealth models."))
  "Files that are byte-identical to each other, which a digest finds and a reader does not.")

(defparameter +facts+
  '((:question "What is in this collection?"
     :answer "51 prompt files in 19 folders: 22 deployment specifications, 13 personas, 4 content policies, 8 recovered system prompts, 2 Ollama Modelfiles, one withdrawal and one empty placeholder.")
    (:question "Are the files adapted per model?"
     :answer "Partly. The Jack register is the same 302 lines for ChatGPT, Gemini, Grok and Kimi; every other file is written for its target.")
    (:question "Which are the model's own prompt, not a rewrite?"
     :answer "Eight: Qwen 3.7, DeepSeek V3 and V4, Muse Wonder, Kimi k2, GLM-5.2, ZCode 5.2 and ZCode 5.3. They carry a knowledge cutoff and the vendor's own framing; jailbreaks_list :kind :system-prompt names them.")
    (:question "What can I install it into?"
     :answer "Two are Ollama Modelfiles, meant for ollama create; the rest are prompts to paste, or an AGENTS.md to drop in a workspace.")
    (:question "What does a withdrawal look like?"
     :answer "grok-fast-v2: 721 bytes, 11 lines, a quarantine badge and a link to the security report in issue #69."))
  "What the collection is worth opening for, answered offline.")
