;;;; check.lisp --- the rule catalog, the masking, the surfaces and the score.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of TakeshiTGAL/hermes-plugin-ja-writing-guard's
;;;; detector, rules and surfaces: the same rule ids, labels, fix hints and
;;;; weights, the same masking (code fences, inline code, URLs, quoted lines,
;;;; HTML), the same 「」 skip, the same surface profiles and the same
;;;; saturation curve. Pure data and regexes: no network, no model.

(in-package #:nodecode-ja-writing-guard)

;;; --- the rule catalog ---------------------------------------------------------

(defstruct (rule (:constructor make-rule))
  id category pattern weight label hint (max 3) (min 1) (scored t) (in-quotes t) raw scanner)

(defun rule (id category pattern weight label hint
             &key (max 3) (min 1) (scored t) (in-quotes t) raw multiline)
  (make-rule :id id :category category :pattern pattern :weight (float weight)
             :label label :hint hint :max max :min min :scored scored
             :in-quotes in-quotes :raw raw
             :scanner (ppcre:create-scanner pattern :multi-line-mode multiline)))

(defparameter +emoji-class+
  (format nil "[✅✨✔❌⚠⭐~c-~c]" (code-char #x1F300) (code-char #x1FAFF))
  "The emoji a business text does not want, as a regex character class.")

(defparameter +object-char+ (string (code-char #xFFFC)))

(defparameter +categories+
  '(("stock" . "定型句・常套句")
    ("inflated" . "誇張・空虚な言葉")
    ("translationese" . "英語の直訳調")
    ("template" . "型どおりの構成")
    ("format" . "記号・書式")
    ("rhythm" . "文末の単調さ")
    ("contrast" . "対比の繰り返し")
    ("sycophancy" . "おもねり・決まり文句の応対")
    ("register" . "場面に合わない文体"))
  "The report's category order and names.")

(defparameter +rules+
  (list
   ;; ---------------------------------------------------------------- stock
   (rule "stock.ieru_deshou" "stock" "と(?:言|い)える(?:でしょう|だろう)" 3.0
         "「〜と言えるでしょう」の結び"
         "推量の結びを外し、言い切るか、言い切れない理由を具体的に書く。")
   (rule "stock.ieru" "stock" "と(?:言|い)えます(?:[。ね]|$)" 1.5
         "「〜と言えます」の結び" "「〜です」で言い切る。根拠があるなら根拠を先に書く。")
   (rule "stock.nodewa" "stock" "のではないでしょうか[。？?]" 1.5
         "「〜のではないでしょうか」の問いかけ" "意見なら「〜と思います」、事実なら言い切る。" :max 2)
   (rule "stock.ikaga" "stock" "いかがでした(?:でしょう)?か" 4.0
         "「いかがでしたか」の締め" "締めの問いかけを削り、本文の最後の事実や次の行動で終える。")
   (rule "stock.zehi_close" "stock"
         "ぜひ(?:一度)?(?:参考|試し|活用|お試し|ご活用|ご参考|チェック)(?:に)?(?:して|ください|になさって)|ぜひ(?:一度)?(?:[^。！!\\n]{1,12})?試してみてください"
         2.0 "「ぜひ〜してみてください」の締め" "勧める理由を1つ具体的に書くか、締めごと削る。" :max 1)
   (rule "stock.matomeru_to" "stock"
         "(?:^|[。\\n])[ \\t]*(?:まとめると|総じて|要するに|結論として)、" 2.0
         "「まとめると、」「総じて、」の前置き" "前置きを削り、まとめの中身から書く。" :max 3 :multiline t)
   (rule "stock.ketsuron" "stock" "結論から(?:言うと|申し上げますと|いうと)" 2.0
         "「結論から言うと」の前置き" "前置きを削り、結論そのものを1文目に置く。")
   (rule "stock.juyou_nowa" "stock" "(?:最も|もっとも|特に)?(?:重要|大切|大事)なのは" 1.5
         "「重要なのは」の前置き" "評価を述語へ移す（「〜が大事です」）。前置きだけなら削る。")
   (rule "stock.juuyou_desu" "stock"
         "(?:する|しておく|おく)?ことが(?:非常に|とても|極めて)?(?:重要|大切|肝要|不可欠|鍵)(?:です|となります|になります|だ|である)"
         1.5 "「〜することが重要です」の連発" "何がどう困るのかを書けば「重要」は要らない。1文書に1回までにする。" :min 2)
   (rule "stock.mitekimashou" "stock"
         "(?:見て|解説して|紹介して|深掘りして|掘り下げて|整理して)いきましょう|(?:解説|紹介|深掘り)していきます"
         2.0 "「〜していきましょう」の予告" "予告を削り、中身から始める。" :max 2)
   (rule "stock.ni_tsuite_kaisetsu" "stock"
         "(?:について|を)(?:詳しく)?(?:解説|ご紹介|紹介|ご説明|説明)(?:します|いたします|していきます)" 1.5
         "「〜について解説します」の予告" "予告の文を削り、説明の1文目から書く。" :max 1)
   (rule "stock.ichigai" "stock" "一概には言え(?:ません|ない)|個人差がありますが|あくまで一例(?:です|ですが)" 1.0
         "予防線の言い回し" "言い切れない条件を具体的に書くか削る。" :max 2)
   (rule "stock.kagi" "stock" "(?:が|は)(?:成功の)?鍵(?:となります|になります|を握ります|です)" 2.0
         "「〜が鍵となります」" "何がどう効くのかを具体的に書く。" :max 2)
   (rule "stock.iu_made" "stock" "言うまでも(?:なく|ありません)|改めて言うまでもなく" 1.0
         "「言うまでもなく」" "言うまでもないなら削る。" :max 1)
   (rule "stock.chuumoku" "stock" "(?:ここで|特に)?注目(?:すべき|したい)(?:点|の)?は" 1.5
         "「注目すべきは」の予告" "注目の重みを述語に残し、中身の文と1文にまとめる。" :max 2)
   (rule "stock.ageraremasu" "stock" "(?:が|として(?:は)?)挙げられ(?:ます|る)" 1.0
         "「〜が挙げられます」" "「〜があります」「主な理由は〜です」と主語を立てて言う。" :max 2 :min 2)
   (rule "stock.chatbot_residue" "stock"
         "ご紹介させていただきます|さらに詳しい情報が必要な場合は|他にご質問があれば|お気軽にお尋ねください" 1.5
         "チャットボットの決まり文句" "締めや前置きの決まり文句を削る。" :max 2)
   (rule "stock.cutoff" "stock"
         "知識のカットオフ|リアルタイム(?:の)?情報には?アクセスでき|最新の情報と異なる(?:場合|可能性)があります|最新の情報は公式[^。\\n]{0,10}ご確認ください"
         3.0 "知識の時点についての断り書き" "断り書きを削り、日付や出典を具体的に書く。" :max 1)
   (rule "stock.false_nuance" "stock" "より正確には|公平に見れば|もう少し掘り下げると|実はもう少し(?:複雑|微妙)" 1.0
         "見せかけの補足の前置き" "前置きを削り、補足の中身だけを書く。" :max 2)
   (rule "stock.filler" "stock" "周知の(?:通り|とおり)|疑いなく|(?:指摘|強調)すべきは|注目に値(?:する|します)" 1.0
         "中身のない強調の前置き" "前置きを削る。" :max 2)
   (rule "stock.empty_optimism" "stock"
         "今後の(?:展開|発展|動向)に(?:注目|期待)|(?:さらなる|今後の)発展が期待され|大きな可能性を秘め|輝かしい未来|無限の可能性|新たな章の幕開け|ますます(?:重要|注目)(?:になって|を集めて|性を増して)|通過点に(?:すぎ|過ぎ)ません|これからも[^。\\n]{0,20}(?:成長し続け|挑戦し続け|進化し続け)"
         1.5 "中身のない明るい見通し" "見通しを書くなら、根拠と時期を書く。なければ削る。" :max 2)
   (rule "stock.throat_clearing" "stock"
         "(?:^|[。\\n])[ \\t]*(?:正直に言うと|正直なところ|はっきり言って|本音を言えば|あえて言うと)、" 0.8
         "「正直に言うと」の前置き" "前置きを削る。" :max 1 :multiline t)
   (rule "stock.pseudo_insight" "stock" "誰も教えてくれない|ほとんどの人が(?:間違え|知らない|見落と)|みんなが見落と|誰も言わない(?:真実|こと)" 2.0
         "煽りの前置き（SNS）" "煽りを削り、中身から書く。" :max 1)
   (rule "stock.lexicon" "stock"
         "現代社会において|デジタル時代において|テクノロジーの進化(?:により|によって)|新たな可能性を切り(?:開|拓)|急速に変化する(?:現代|社会|時代|ビジネス環境)|近年、[^。\\n]{0,30}(?:急速に|ますます)|本記事では[^。\\n]{0,40}(?:解説|紹介)"
         1.5 "AI の書き出しの決まり文句" "一般論の書き出しを削り、本題の事実から書く。" :max 2)
   (rule "stock.mazu_saisho" "stock" "まず最初に" 1.0 "「まず最初に」の重複" "「まず」か「最初に」の一方にする。" :max 1)
   (rule "stock.stacked_closing" "stock"
         "よろしくお願い(?:いた)?します|よろしくお願い申し上げます|何卒よろしく|お気軽に(?:お問い合わせ|ご連絡|ご相談|お申し付け)|ご確認のほど[^。\\n]{0,10}お願い"
         2.0 "締めのあいさつの重ね" "締めのあいさつは1つにする。" :max 1 :min 3)
   (rule "stock.over_keigo" "stock" "させていただ[きくけ]" 0.8
         "「させていただきます」の重ね" "「いたします」「します」を混ぜる。" :max 3 :min 3 :scored nil)
   (rule "stock.evaluative_cliche" "stock"
         "大きな学び(?:に|と|の機会)|貴重な学び|有意義な(?:機会|時間)(?:となり|になり|でし)|肌で感じ(?:ることができ|られ)|素晴らしい(?:ご縁|機会|経験|体験)(?:を|と|に|が)|かけがえのない(?:財産|経験|時間|存在|もの)|確かな(?:変化|一歩|成長)を|奥が深いですね|奥深いですね|(?:何かの)?(?:ヒント|参考|きっかけ)になれば(?:嬉しい|うれしい|幸い)|大きなメリットをもたらし|(?:両面|双方)において大きな|必ずや|と確信して(?:おります|います)|最優先(?:事項)?と考えて|より良い[^。\\n]{0,12}(?:づくり|の実現)に(?:取り組んで|努めて)まいります|この経験を糧に|想いを胸に|という想いから|見違えるほど|複合的に絡み合|学びと気づき|気づきと学び|考えさせられる(?:一冊|一日|作品|時間|内容)|これ以上の喜びはありません|一助となって(?:いれば|いたのであれば)|心から歓迎|世代を超えて(?:愛され|受け継が)|持続可能な未来|心より楽しみにして"
         2.5 "感想・評価の決まり文句" "感想や評価の総括を削るか、起きた事実で言い直す。" :max 2)
   (rule "stock.era_opening" "stock"
         "VUCA|不確実性の(?:高い|増す)時代|変化の激しい(?:時代|現代)|先行きの見えない時代|人生100年時代において" 2.0
         "時代を語る書き出し" "時代の話を削り、本題の事実から書く。" :max 1)
   (rule "stock.both_sides" "stock"
         "(?:いずれ|どちら|それぞれ)(?:の[^。\\n]{0,10})?にも(?:メリットとデメリット|一長一短)|一長一短があり|目的(?:や状況)?に(?:応じて|合わせて)[^。\\n]{0,15}(?:選択|使い分け)することが(?:重要|大切)"
         2.0 "どちらとも言える一般論の結び" "結論を言い切るか、判断の条件を具体的に書く。" :max 1)
   ;; ------------------------------------------------------------- inflated
   (rule "inflated.buzz" "inflated"
         "シームレス(?:に|な)|包括的(?:な|に)|多角的(?:な|に)|革新的(?:な|に)|画期的(?:な|に)|新たな価値|価値を(?:創出|提供)|可能性を(?:広げ|秘め)|架け橋|第一歩を|一歩先|次のステージ|未来を(?:切り拓|創|拓)|飛躍的(?:に|な)|圧倒的(?:な|に)|最大化(?:し|する|します)|シンプルかつ(?:堅牢|強力|柔軟)|幅広い(?:ユースケース|用途|ニーズ|シーン)に対応|強力に(?:後押し|サポート)|さらなる成長を加速"
         1.5 "宣伝調の大げさな言葉" "何がどれだけ変わるのかを数字や具体物で書く。" :max 4)
   (rule "inflated.slop_nouns" "inflated" "解像度を上げ|腹落ち|手触り感|温度感|熱量を|羅針盤|起爆剤|触媒となり|真髄|極致|深淵" 1.5
         "比喩の流行語" "ふだん使う言葉に置き換える。" :max 3)
   (rule "inflated.yakuwari" "inflated"
         "(?:重要な|大きな|中心的な|不可欠な|欠かせない)役割を(?:果たし|担っ|担い|果たす|担う)" 2.0
         "「重要な役割を果たします」" "何をするのかを動詞で書く（「〜を受け付けます」）。" :max 2)
   (rule "inflated.samazama" "inflated" "(?:様々|さまざま|多様|幅広い)な" 0.7
         "「様々な」「多様な」の多用" "何が含まれるのかを2〜3個挙げる。" :max 3 :min 3)
   (rule "inflated.hijou_ni" "inflated" "(?:非常に|極めて|とても)(?:重要|大切|有効|効果的|便利|強力|魅力的)" 1.0
         "強調語の重ね" "強調語を外して根拠で示す。" :max 3 :min 2)
   (rule "inflated.kotoba_dake" "inflated" "(?:言語化|深掘り|掘り下げ)(?:する|し|して|します)|正面から(?:扱|向き合)" 1.0
         "作業したことだけを宣言する動詞" "何をどう書いたかを書く。" :max 2)
   (rule "inflated.kakasenai" "inflated" "欠かせない存在|なくてはならない存在|必要不可欠(?:な|です|となって)" 1.0
         "「欠かせない存在」" "なぜ要るのかを具体的に書く。" :max 2)
   (rule "inflated.hype" "inflated"
         "革命的な|ゲームチェンジャー|究極の|最先端の|魔法のように|奇跡的な|驚異的な|可能性を解き放|潜在能力を引き出|パラダイムシフト|業界を再定義|未来を変える|新たな基準を(?:打ち立て|設定)|根本的に変革"
         1.5 "誇大な宣伝語" "何がどれだけ変わるのかを事実で書く。" :max 3)
   (rule "inflated.significance" "inflated"
         "歴史的な転換点|新たな地平|礎を築|金字塔|の先駆けとなる|極めて重要な意義|時代を画する|新時代を切り(?:開|拓)" 1.5
         "意義を大きく言う言葉" "意義を言わず、起きたことを書く。" :max 2)
   (rule "inflated.marketing_cliche" "inflated"
         "織りなす|至福のひととき|五感で(?:楽しむ|味わ|感じ)|こだわり抜い|ワンランク上の|シナジー|強みを掛け合わせ|唯一無二の|極上の" 1.5
         "宣伝文の決まり文句" "何がどう良いのかを、素材・数字・手順で書く。" :max 2)
   (rule "inflated.pr_cliche" "inflated"
         "未来を(?:共に|ともに)?(?:創|つく|切り拓|切り開|拓)|ワンストップで(?:完結|提供|解決)|新しい価値(?:を|の)(?:創|つく|提供|生み)" 1.5
         "広報文の決まり文句" "理念の言い回しを削り、具体的に何をするのかを書く。" :max 2)
   ;; -------------------------------------------------------- translationese
   (rule "translationese.dekiru" "translationese"
         "することが(?:でき(?:る|ます|た|ません)|可能(?:です|だ|となります|になります|になる|となる|となっています|になっています))"
         1.0 "「〜することができます」" "可能形にする（「使えます」「確認できます」）。" :max 3)
   (rule "translationese.kanten" "translationese" "という(?:観点|視点)(?:から|で)|という点(?:で|において)" 1.0
         "「〜という観点から」" "「〜で見ると」「〜については」と短く言う。" :max 2)
   (rule "translationese.ni_totte" "translationese" "にとって(?:非常に)?(?:重要|不可欠|大切|欠かせない)" 1.0
         "「〜にとって重要」" "「〜には〜が要る」と必要なものを書く。" :max 2)
   (rule "translationese.motsu" "translationese" "(?:意味|意義|価値|影響|可能性|特徴|強み)を持(?:つ|ち|っています|ちます)" 1.0
         "「〜を持つ」（have の直訳）" "「〜がある」に直す。" :max 2)
   (rule "translationese.niyotte" "translationese" "することによって|することにより" 0.7
         "「〜することによって」" "「〜して」「〜すれば」にする。" :max 2)
   (rule "translationese.hokanaranai" "translationese"
         "に他な(?:らない|りません)|であることは間違いな(?:い|いでしょう)|と言っても過言では(?:ない|ありません)" 2.0
         "強い断定の決め文" "根拠を先に書き、決め文を外す。" :max 2)
   (rule "translationese.mushoku_shugo" "translationese"
         "(?:この|その)(?:結果|事実|データ|アプローチ|仕組み|機能)(?:は|が)[^。\\n]{0,20}(?:を)?(?:示して|示唆して|物語って|可能にし|実現し)"
         1.0 "無生物主語の他動詞" "人や状況を主語にする（「この結果から〜と分かります」）。" :max 2)
   (rule "translationese.okonau" "translationese" "を行(?:う|います|いました|って|い)(?:こと)?" 0.4
         "「〜を行う」（do の直訳）" "「確認を行う」→「確認する」のように動詞にする。" :max 3 :min 3)
   (rule "translationese.enable" "translationese" "を可能に(?:します|する|しました|しています)|ことを期待(?:します|しています)" 1.2
         "「〜を可能にします」（enable の直訳）" "主語を立てて「〜できるようになります」と言う。" :max 2)
   (rule "translationese.anata" "translationese"
         "あなたの(?:チーム|会社|ビジネス|プロジェクト|貢献|ご意見|アイデア|成功|キャリア|ニーズ)" 1.5
         "「あなたの〜」（your の直訳）" "「あなたの」を外すか、「皆さんの」「ご自身の」にする。" :max 2)
   (rule "translationese.let_me_know" "translationese" "遠慮なく(?:私に)?(?:お)?知らせて|私に(?:お)?知らせてください" 1.5
         "「遠慮なく知らせてください」（let me know の直訳）" "「ご連絡ください」と言う。" :max 1)
   (rule "translationese.ing_surface" "translationese"
         "示しながら|強調しつつ|反映しており|象徴するとともに|体現しつつ|物語っており|浮き彫りにし(?:ており|ています|ました)" 1.0
         "付け足しの分詞構文" "文を分け、主語と動詞をはっきりさせる。" :max 2)
   (rule "translationese.vague_source" "translationese" "専門家(?:によると|によれば|は指摘)|一部の(?:見方|専門家)では|調査結果が示すように" 0.8
         "出典のない権威づけ" "誰の何という調査かを書くか削る。" :max 2)
   (rule "translationese.stiff" "translationese" "と言わざるを得(?:ない|ません)|の感が否め(?:ない|ません)|の一助となれば幸い" 1.0
         "硬すぎる決まり文句" "ふだんの言い方にする。" :max 1)
   (rule "translationese.double_negative" "translationese"
         "ないことはない|ないこともない|ないでもない|ないわけではない|なくもない|なくはない|ないとは(?:言い切れ|限ら)ない" 0.6
         "二重否定" "肯定文で言う。" :max 2 :min 2)
   ;; ------------------------------------------------------------- template
   (rule "template.count_decl" "template"
         "(?:[2-7２-７二三四五六七]つ|いくつか)の(?:重要な|主な|大切な)?(?:ポイント|理由|ステップ|方法|コツ|観点|要素|メリット|特徴|視点|柱|原則|軸)"
         2.0 "「3つのポイント」型の数の宣言" "数を先に宣言せず、中身から書く。" :max 1)
   (rule "template.ikutsuka" "template"
         "いくつか(?:の)?(?:重要な|大切な|主な)?(?:コツ|ポイント|方法|理由|注意点|ステップ|選択肢|アプローチ)(?:が|を|に)" 1.5
         "「いくつかのポイントがあります」の前置き" "前置きを削り、1つ目の中身から始める。" :max 1)
   (rule "template.ika_matome" "template"
         "以下(?:に|の(?:とおり|通り|ように))[^。\\n]{0,15}(?:まとめ|整理|紹介|解説|示し|挙げ)" 1.5
         "「以下にまとめました」" "前置きを削り、そのまま本題に入る。" :max 1)
   (rule "template.rhetorical_q" "template" "(?:なぜ|どうして)(?:でしょうか|なのでしょうか)[？?][ \\t]*(?:それは|答えは|理由は)" 2.5
         "自問自答の型" "問いを外して理由から書く。" :max 2)
   (rule "template.sorekoso" "template" "これこそが|それこそが|これが[^。\\n]{0,12}の(?:本質|真髄|核心)です" 1.5
         "決め台詞の結び" "事実を淡々と書く。" :max 1)
   (rule "template.closing_offer" "template"
         "(?:何か|ほかに|他に)(?:ご不明な点|ご質問|気になる(?:点|こと))(?:や[^。\\n]{0,10})?があれば(?:、)?(?:お気軽に|いつでも)" 1.0
         "「何かあればお気軽に」の締め（チャット）" "締めを削るか、相手が次にすることを1つ書く。" :max 1)
   (rule "template.hope_helpful" "template"
         "お役に立て(?:れば|たら)(?:幸いです|嬉しいです|うれしいです)|参考になれば(?:幸いです|嬉しいです)" 1.5
         "「お役に立てれば幸いです」" "締めを削る。" :max 1)
   (rule "template.colon_reveal" "template" "(?:結論|オチ|ネタバレ|一番すごいのは|実は|答え)[：:][ \\t]*\\S" 1.5
         "「結論：」の種明かし" "ラベルを外して文で言う。" :max 2)
   (rule "template.anata_e" "template" "そんな(?:あなた|方)(?:へ|に)|と(?:お)?悩みの(?:方|あなた)|こんなお悩み(?:は)?ありませんか" 1.5
         "「そんなあなたへ」の呼びかけ" "呼びかけを削り、誰に何が役立つのかを書く。" :max 1)
   (rule "template.sns_cta" "template" "(?:ぜひ)?コメント(?:で|欄で)?(?:教えて|聞かせて)" 1.5
         "SNS で反応を求めるお願い" "お願いは依頼の文なので残してよい。書き直しでも消さない。" :max 1 :scored nil)
   ;; --------------------------------------------------------------- format
   (rule "format.em_dash" "format"
         "[ぁ-んァ-ヶ一-龠々ー0-9A-Za-z）」』][ \\t]*(?:——|—|―{2}|──)[ \\t]*(?:[ぁ-んァ-ヶ一-龠々ー0-9A-Za-z（「『。、]|$)"
         2.0 "語と語の間のダッシュ（——）" "読点か括弧にするか、文を分ける。" :max 3 :in-quotes nil)
   (rule "format.bold" "format" "\\*\\*[^*\\n]{1,40}\\*\\*" 1.0
         "太字（**…**）" "太字を外す。強調したい文は短く言い切る。" :max 4 :min 2 :in-quotes nil)
   (rule "format.bold_label" "format" "^[ \\t]*(?:[-*・]|[0-9]+[.)．])[ \\t]*\\*\\*[^*\\n]{1,30}\\*\\*[ \\t]*[:：]?" 2.0
         "「- **見出し語**：説明」の箇条書き" "箇条書きをやめて地の文で説明する。"
         :max 3 :in-quotes nil :multiline t)
   (rule "format.bold_heading_line" "format" "^[ \\t]*\\*\\*[^*\\n]{2,40}\\*\\*[ \\t]*[:：]?[ \\t]*$" 1.5
         "太字だけの行を見出し代わりにする" "短い返答なら段落で書く。文書なら見出しにする。"
         :max 3 :in-quotes nil :multiline t)
   (rule "format.colon_lead" "format" "[ぁ-んァ-ヶ一-龠々ー][：:][ \\t]*$" 1.0
         "文末のコロン（〜は以下のとおり：）" "句点で終えるか前置きを削る。" :max 3 :min 2 :in-quotes nil :multiline t)
   (rule "format.heading_in_chat" "format" "^#{2,4}[ \\t]+\\S" 1.0
         "Markdown の見出し" "短い返答なら見出しを使わず段落で書く。" :max 3 :in-quotes nil :multiline t)
   (rule "format.emoji_bullet" "format"
         (concatenate 'string "^[ \\t]*(?:[-*・][ \\t]*)?" +emoji-class+ "[ \\t]*\\S") 1.5
         "絵文字を頭に付けた行" "絵文字を外し、普通の文で書く。" :max 3 :in-quotes nil :multiline t)
   (rule "format.emoji" "format" +emoji-class+ 0.5
         "絵文字" "業務の文では絵文字を外す。" :max 4 :min 2 :in-quotes nil)
   (rule "format.arrow_chain" "format" "(?:→|⇒|➡)[^\\n→⇒➡]{1,30}(?:→|⇒|➡)" 0.7
         "矢印の連鎖" "矢印を文章にする（「〜すると〜になる」）。" :max 2 :in-quotes nil)
   (rule "format.exclaim" "format" "[！!]" 0.4 "感嘆符の多用" "感嘆符を句点に戻す。" :max 4 :min 3)
   (rule "format.paren_gloss" "format" "[ァ-ヶー一-龠]{2,}[（(][A-Z][A-Za-z]+(?:[ -][A-Za-z]+){1,5}[）)]" 1.0
         "英語の言い換えを添えたカッコ" "読み手に要らない英語の補足は削る。" :max 2 :min 2 :in-quotes nil)
   (rule "format.info_prefix_bold" "format"
         "\\*\\*(?:注意|重要|ポイント|メモ|参考|補足|確認|チェック|推奨|おすすめ|良い例|悪い例|結論|まとめ)(?:[：:][^*\\n]*)?\\*\\*" 1.5
         "「**ポイント**」型の太字ラベル" "ラベルを外し、文で言う。" :max 3 :in-quotes nil)
   (rule "format.ai_leakage" "format"
         (concatenate 'string ":contentReference|oaicite|oai_citation|\\bturn[0-9]+(?:search|view|news|fetch)[0-9]*\\b|"
                      +object-char+ "|utm_source=(?:chatgpt\\.com|openai\\.com|perplexity\\.ai|claude\\.ai|gemini\\.google\\.com)")
         5.0 "AI ツールの出力の痕跡" "痕跡（引用タグ・utm_source など）を消す。" :max 1 :in-quotes nil :raw t)
   (rule "format.halfwidth_mark" "format" "[ぁ-んァ-ヶ一-龠][!?](?=[ \\tぁ-んァ-ヶ一-龠]|$)" 0.5
         "和文中の半角の！？" "全角にするか句点にする。" :max 3 :min 2 :multiline t)
   (rule "format.wave_heading" "format" "＼[^／\\n]{1,30}／" 1.0
         "＼見出し／の飾り" "飾りを外す。" :max 1 :in-quotes nil)
   ;; ------------------------------------------------------------- contrast
   (rule "contrast.dewanaku" "contrast" "ではなく、?|だけでなく、?[^。\\n]{0,20}も|のではなく、?" 0.8
         "「AではなくB」の繰り返し" "誤解を正す場面以外は、素直な肯定文にする。" :max 4 :min 2)
   (rule "contrast.nominarazu" "contrast" "にとどまらず|単なる[^。\\n]{1,15}(?:ではなく|ではありません|ではない)|を超えた何か" 1.0
         "「〜にとどまらず」の対比" "対比をやめ、言いたい方だけを書く。" :max 2)
   ;; ----------------------------------------------------------- sycophancy
   (rule "sycophancy.great_q" "sycophancy"
         "(?:素晴らしい|すばらしい|いい|良い|鋭い|とても良い|非常に鋭い)(?:質問|ご質問|視点|着眼点|問い|ご指摘)(?:ですね|です)|(?:とても|非常に)?興味深いテーマですね"
         4.0 "「素晴らしい質問ですね」" "前置きを削り、答えから書く。" :max 1)
   (rule "sycophancy.empathy" "sycophancy"
         "(?:そのお気持ち|お気持ち|そう感じるのは)[^。\\n]{0,10}(?:よく分かります|よくわかります|当然です|自然なことです)|(?:悩みますよね|迷いますよね|大変ですよね)"
         2.0 "決まり文句の共感" "共感の前置きを削り、相手の状況に合う具体的な一言にする。" :max 1)
   (rule "sycophancy.kashikomari" "sycophancy" "^(?:もちろんです|承知しました|かしこまりました)[！!。]" 1.0
         "返答の頭の決まり文句" "決まり文句を削り、答えから書く。" :max 1 :multiline t))
  "The rule catalog: id, category, pattern, weight, label, fix hint. Rewritten
from nanaism/yomiyasu, coji/natural-japanese, textlint-ja presets, patina,
japanese-humanizer and humanizer-ja, and some written for the original plugin.")

;;; --- masking ------------------------------------------------------------------

(defun blank-string (text)
  "TEXT with every character but a newline turned into a space: offsets hold."
  (map 'string (lambda (ch) (if (char= ch #\Newline) ch #\Space)) text))

(defparameter +mask-scanners+
  (list (ppcre:create-scanner "(?ms)^(```|~~~).*?^\\1[^\\n]*$")
        (ppcre:create-scanner "`[^`\\n]+`")
        (ppcre:create-scanner "https?://[ \\t)）>\\]」]+")
        (ppcre:create-scanner "(?m)^[ \\t]*>.*$")
        (ppcre:create-scanner "<[^>\\n]{1,200}>"))
  "Code fences, inline code, URLs, quoted lines and HTML: masked before scanning.")

(defun mask-text (text)
  "TEXT with the ignored spans blanked, newlines and length kept."
  (reduce (lambda (subject scanner)
            (ppcre:regex-replace-all
             scanner subject
             (lambda (match &rest registers)
               (declare (ignore registers))
               (blank-string match))))
          +mask-scanners+ :initial-value (copy-seq (or text ""))))

;;; --- quotes, scripts, characters ----------------------------------------------

(defun quote-spans (text)
  "The (START . END) ranges of each outermost 「」/『』 pair in TEXT."
  (let ((spans '()) (depth 0) (start 0))
    (loop for i from 0 below (length text)
          for ch = (char text i)
          do (cond ((member ch '(#\「 #\『))
                    (when (zerop depth) (setf start i))
                    (incf depth))
                   ((and (member ch '(#\」 #\』)) (plusp depth))
                    (decf depth)
                    (when (zerop depth) (push (cons start (1+ i)) spans)))
                   ((and (char= ch #\Newline) (plusp depth) (> (- i start) 400))
                    (setf depth 0))))
    (nreverse spans)))

(defun inside-quote-p (position spans)
  (loop for (start . end) in spans thereis (<= start position (1- end))))

(defun kana-char-p (char)
  (let ((code (char-code char)))
    (or (<= #x3041 code #x3096) (<= #x30A1 code #x30FA))))

(defun ja-char-p (char)
  (let ((code (char-code char)))
    (or (kana-char-p char)
        (<= #x4E00 code #x9FFF)
        (member code '(#x3005 #x3006 #x30F5 #x30F6 #x30FC)))))

(defun japanese-p (text &key (min-kana 10))
  "True when TEXT is mainly Japanese prose: enough kana, mostly Japanese letters."
  (and (>= (count-if #'kana-char-p text) min-kana)
       (let ((letters (max 1 (count-if (lambda (ch) (or (ja-char-p ch) (alpha-char-p ch))) text))))
         (>= (/ (float (count-if #'ja-char-p text)) letters) 0.4))))

;;; --- sentences ----------------------------------------------------------------

(defstruct (sentence (:constructor make-sentence (text start end kind ending key terminated)))
  text start end kind ending key terminated)

(defparameter +polite-end+
  (ppcre:create-scanner
   "(?:です|ます|ました|でした|ません|ませんでした|でしょう|ましょう|ください|下さい|ございます|いたします|致します|願います|存じます|申し上げます)(?:ね|よ|か|よね|が|けれど|けど)?$"))

(defparameter +plain-end+
  (ppcre:create-scanner
   "(?:だ|である|であった|だった|だろう|ではない|じゃない|ない|なかった|た|る|う|く|す|つ|ぬ|む|ぶ|ぐ|い|よう|まい|ず|のだ|のである|かもしれない|と思う|と考える)(?:ね|よ|な|か|よね|わ|ぞ|さ)?$"))

(defparameter +nominal-end+
  (ppcre:create-scanner "(?:[一-龠々ァ-ヶーA-Za-z0-9０-９％%]|こと|もの|ため|とおり|通り)$"))

(defparameter +list-line+ (ppcre:create-scanner "^[ \\t]*(?:[-*+] |[0-9]+[.)．] |[・●■◆▶] ?)"))
(defparameter +md-list-line+ (ppcre:create-scanner "^[ \\t]*(?:[-*+] |[0-9]+[.)．] )"))
(defparameter +heading-line+ (ppcre:create-scanner "^[ \\t]*(?:#{1,6} |【[^】]{1,30}】[ \\t]*$|■)"))
(defparameter +table-line+ (ppcre:create-scanner "^[ \\t]*\\|"))
(defparameter +casual+ (ppcre:create-scanner "じゃん|マジ|めっちゃ|ヤバ|やば[いく]|っす(?:[。！!ね]|$)|ちゃう|だよね|でしょ[。！!]|笑[)）]|（笑）|w{2,}" :multi-line-mode t))

(defun ending-of (core)
  "The (VALUES ENDING KEY) a sentence's CORE ends in."
  (let ((trimmed (string-right-trim "。！？!?…・ 　)）" core)))
    (cond ((zerop (length trimmed)) (values "other" ""))
          ((member (char trimmed (1- (length trimmed))) '(#\」 #\』 #\")) (values "other" ""))
          ((ppcre:scan +polite-end+ trimmed)
           (let ((m (ppcre:scan-to-strings
                     "(?:て|で)?(?:い|おり|あり|し|でき|なり|られ|され)?(?:ます|ました|ません|です|でした|でしょう|ましょう|ください|ございます|いたします)(?:ね|よ|か)?$"
                     trimmed)))
             (values "polite" (if (and m (stringp m)) m (subseq trimmed (max 0 (- (length trimmed) 2)))))))
          ((ppcre:scan +nominal-end+ trimmed) (values "nominal" "N"))
          ((ppcre:scan +plain-end+ trimmed) (values "plain" (subseq trimmed (max 0 (- (length trimmed) 2)))))
          (t (values "other" (subseq trimmed (1- (length trimmed))))))))

(defun split-outside-parens (line)
  "LINE split after each 。！？ unless inside （）() or 「」『』."
  (let ((pieces '()) (depth 0) (start 0))
    (loop for i from 0 below (length line)
          for ch = (char line i)
          do (cond ((member ch '(#\（ #\( #\「 #\『)) (incf depth))
                   ((and (member ch '(#\） #\) #\」 #\』)) (plusp depth)) (decf depth))
                   ((and (member ch '(#\。 #\！ #\？ #\! #\?)) (zerop depth))
                    (push (subseq line start (1+ i)) pieces)
                    (setf start (1+ i)))))
    (when (< start (length line)) (push (subseq line start) pieces))
    (nreverse pieces)))

(defun split-sentences (scan)
  "SCAN, masked, as the sentences the rhythm and register checks read."
  (let ((out '()) (position 0))
    (dolist (line (ppcre:split "\\n" scan))
      (let ((line-start position))
        (incf position (1+ (length line)))
        (let ((stripped (string-trim '(#\Space #\Tab #\Return) line)))
          (cond
            ((zerop (length stripped)) nil)
            ((ppcre:scan +table-line+ line) nil)
            ((ppcre:scan +heading-line+ line)
             (push (make-sentence stripped line-start (+ line-start (length line))
                                   "heading" "other" "" nil) out))
            (t
             (let ((kind (if (ppcre:scan +list-line+ line) "list" "prose"))
                   (offset 0))
               (dolist (piece (split-outside-parens line))
                 (unless (zerop (length piece))
                   (let* ((start (+ line-start offset))
                          (core (string-trim '(#\Space #\Tab) piece)))
                     (incf offset (length piece))
                     (when (and (>= (length core) 4)
                                (find-if #'ja-char-p core))
                       (multiple-value-bind (ending key) (ending-of core)
                         (push (make-sentence core start (+ start (length piece)) kind ending key
                                              (find (char core (1- (length core))) "。！？!?"))
                               out))))))))))))
    (nreverse out)))

;;; --- surfaces -----------------------------------------------------------------

(defparameter +surfaces+
  '(("chat" :threshold 40 :polite nil :formal nil
     :category (("format" . 0.7) ("register" . 0.0))
     :rule (("format.heading_in_chat" . 1.2) ("format.bold" . 0.6) ("format.exclaim" . 0.5)
            ("template.closing_offer" . 1.5) ("template.hope_helpful" . 1.5)
            ("stock.over_keigo" . 0.5)))
    ("business_email" :threshold 35 :polite t :formal t
     :category (("format" . 1.3))
     :rule (("template.closing_offer" . 0.3) ("stock.ni_tsuite_kaisetsu" . 0.7)
            ("format.emoji" . 3.0) ("format.exclaim" . 1.5)))
    ("external" :threshold 40 :polite t :formal t
     :category (("inflated" . 1.4) ("format" . 1.2))
     :rule (("template.closing_offer" . 0.5) ("format.emoji" . 3.0)))
    ("sns" :threshold 35 :polite nil :formal nil
     :category (("register" . 0.0) ("template" . 1.3) ("inflated" . 1.2))
     :rule (("format.emoji" . 0.0) ("format.exclaim" . 0.0) ("format.emoji_bullet" . 1.0)
            ("rhythm.taigen_run" . 0.3)))
    ("tech_doc" :threshold 45 :polite nil :formal nil
     :category (("format" . 0.4) ("register" . 0.0))
     :rule (("format.list_heavy" . 0.0) ("format.heading_in_chat" . 0.0)
            ("translationese.okonau" . 0.3) ("format.paren_gloss" . 0.5))))
  "Surface profiles: the same sentence judged by where it goes.")

(defparameter +surface-labels+
  '(("chat" . "チャット")
    ("business_email" . "業務メール")
    ("external" . "社外文（お知らせ・リリース・案内）")
    ("sns" . "SNS 投稿")
    ("tech_doc" . "技術文書")))

(defparameter +surface-aliases+
  '(("email" . "business_email") ("mail" . "business_email") ("press" . "external")
    ("public" . "external") ("x" . "sns") ("twitter" . "sns") ("tech" . "tech_doc")
    ("docs" . "tech_doc") ("doc" . "tech_doc")))

(defun surface-names () (mapcar #'car +surfaces+))

(defun surface-profile (name)
  "The profile NAME names; an unknown name refuses with a ValueError's words."
  (let* ((key (string-downcase (substitute #\_ #\- (string-trim '(#\Space #\Tab) (or name "chat")))))
         (key (or (cdr (assoc key +surface-aliases+ :test #'string=)) key))
         (found (assoc key +surfaces+ :test #'string=)))
    (or found
        (error "unknown surface ~s; choose one of ~{~a~^, ~}" name (surface-names)))))

(defun surface-value (profile key) (getf (cdr profile) key))

(defun surface-weight (profile rule-id category base)
  (let ((rule-scale (assoc rule-id (surface-value profile :rule) :test #'string=)))
    (* base (if rule-scale
                (cdr rule-scale)
                (or (cdr (assoc category (surface-value profile :category) :test #'string=)) 1.0)))))

;;; --- the report ---------------------------------------------------------------

(defstruct (finding (:constructor make-finding))
  rule category label hint start end line col excerpt weight counted source)

(defstruct (report (:constructor make-report))
  surface score threshold over-threshold japanese chars findings register-issues stats)

(defun line-col (text position)
  (values (1+ (count #\Newline text :end position))
          (1+ (- position (or (position #\Newline text :end position :from-end t) -1)))))

(defun excerpt-of (text start end &key (pad 12))
  (let* ((s (max 0 (- start pad)))
         (e (min (length text) (+ end pad)))
         (snippet (substitute #\⏎ #\Newline (subseq text s e))))
    (concatenate 'string (if (plusp s) "…" "") snippet (if (< e (length text)) "…" ""))))

(defparameter +score-k+ 4.0)
(defparameter +length-unit+ 500)

(defun score-of (points ja-chars)
  "The original's curve: weighted points saturate 0-100, scaled by text length."
  (let ((norm (sqrt (max 1.0 (/ ja-chars (float +length-unit+))))))
    (round (* 100 (- 1 (exp (- (/ points (* +score-k+ norm)))))))))

(defun structural-label (id)
  "The (CATEGORY LABEL HINT SOURCE) the structural check ID carries."
  (cdr (assoc id '(("rhythm.same_ending" "rhythm" "同じ文末が続く" "文末の形を変えるか、2文を1文にまとめる。" "yomiyasu")
                    ("rhythm.taigen_run" "rhythm" "体言止めが続く" "動詞や「です」で終わる文を混ぜる。" "original")
                    ("rhythm.mixed_register" "rhythm" "敬体と常体が混ざる" "地の文を「です・ます」か「だ・である」のどちらかにそろえる。" "yomiyasu")
                    ("register.plain_in_polite" "register" "敬体が必要な場面での常体" "社外の文やメールは「です・ます」で書く。" "original")
                    ("register.casual_in_formal" "register" "改まった場面でのくだけた言葉" "「〜じゃない」「マジ」などを改まった言い方にする。" "original")
                    ("format.list_heavy" "format" "箇条書きの比率が高い" "考えのつながりは地の文で書く。" "yomiyasu"))
               :test #'string=)))

;;; --- the check itself ---------------------------------------------------------

(defvar *profile* nil "The surface profile CHECK-TEXT is running under.")
(defvar *text* nil "The text being checked, unmasked: excerpts and line numbers read it.")
(defvar *findings* nil "The findings collected so far, newest first.")

(defun note-finding (rule-id category label hint start end base counted source)
  "Record one finding on *FINDINGS*, its weight scaled by the surface."
  (let* ((w (surface-weight *profile* rule-id category base))
         (weight (round (* 100 w))))
    (multiple-value-bind (line col) (line-col *text* start)
      (push (make-finding :rule rule-id :category category :label label :hint hint
                          :start start :end end :line line :col col
                          :excerpt (excerpt-of *text* start end)
                          :weight weight
                          :counted (and counted (plusp weight))
                          :source source)
            *findings*))))

(defun note-structural (id start end base &optional (counted t))
  "Record one structural finding, its label and hint from the shared table."
  (destructuring-bind (category label hint source) (structural-label id)
    (note-finding id category label hint start end base counted source)))

(defun scan-rules (text scan quotes)
  "Every phrase rule over TEXT (or SCAN, for one that reads the raw text), the
hits inside 「」 skipped."
  (dolist (r +rules+)
    (let ((subject (if (rule-raw r) text scan))
          (hits '()))
      (ppcre:do-scans (ms me rs re (rule-scanner r) subject)
        (declare (ignore rs re))
        (when (> me ms) (push (cons ms me) hits)))
      (setf hits (nreverse hits))
      (when (rule-in-quotes r)
        (setf hits (remove-if (lambda (hit) (inside-quote-p (car hit) quotes)) hits)))
      (when (>= (length hits) (rule-min r))
        (loop for (start . end) in hits
              for i from 0
              do (note-finding (rule-id r) (rule-category r) (rule-label r) (rule-hint r)
                                start end (rule-weight r)
                                (and (rule-scored r) (< i (rule-max r)))
                                "original"))))))

(defun prose-sentences (scan quotes)
  "The terminated prose sentences of SCAN, those inside 「」 left out."
  (remove-if-not (lambda (s) (and (string= (sentence-kind s) "prose")
                                   (sentence-terminated s)
                                   (not (inside-quote-p (sentence-start s) quotes))))
                 (split-sentences scan)))

(defun scan-ending-runs (prose)
  "The same fine-grained ending four or more times in a row."
  (let ((i 0))
    (loop while (< i (length prose))
          do (let ((j i))
               (loop while (and (< (1+ j) (length prose))
                                (plusp (length (sentence-key (nth (1+ j) prose))))
                                (string= (sentence-key (nth (1+ j) prose))
                                         (sentence-key (nth i prose))))
                     do (incf j))
               (when (and (>= (1+ (- j i)) 4)
                          (not (member (sentence-key (nth i prose)) '("" "N") :test #'string=)))
                 (note-structural "rhythm.same_ending" (sentence-start (nth i prose))
                                  (sentence-end (nth j prose)) 1.0))
               (setf i (1+ j))))))

(defun scan-nominal-runs (prose)
  "Three or more nominal endings in a row."
  (let ((i 0))
    (loop while (< i (length prose))
          do (let ((j i))
               (loop while (and (< j (length prose))
                                (string= (sentence-ending (nth j prose)) "nominal"))
                     do (incf j))
               (when (>= (- j i) 3)
                 (note-structural "rhythm.taigen_run" (sentence-start (nth i prose))
                                  (sentence-end (nth (1- j) prose)) 1.5))
               (setf i (max j (1+ i)))))))

(defun scan-register (prose profile scan quotes)
  "The endings and casual words that do not fit the surface."
  (let ((polite (remove-if-not (lambda (s) (string= (sentence-ending s) "polite")) prose))
        (plain (remove-if-not (lambda (s) (string= (sentence-ending s) "plain")) prose)))
    (cond
      ((surface-value profile :polite)
       (loop for s in plain for k from 0
             do (note-structural "register.plain_in_polite" (sentence-start s)
                                 (sentence-end s) 2.5 (< k 3))))
      ((and (>= (length prose) 4) (>= (length polite) 2) (>= (length plain) 2))
       (when (>= (/ (float (min (length polite) (length plain)))
                    (+ (length polite) (length plain)))
                 0.2)
         (let ((first (if (<= (length plain) (length polite)) (car plain) (car polite))))
           (note-structural "rhythm.mixed_register" (sentence-start first)
                            (sentence-end first) 2.0)))))
    (when (surface-value profile :formal)
      (let ((hits '()))
        (ppcre:do-scans (ms me rs re +casual+ scan)
          (declare (ignore rs re))
          (when (and (> me ms) (not (inside-quote-p ms quotes))) (push (cons ms me) hits)))
        (loop for (start . end) in (nreverse hits) for k from 0
              do (note-structural "register.casual_in_formal" start end 2.0 (< k 2)))))))

(defun scan-lists (scan)
  "A reply that is mostly a markdown list."
  (let ((lines (remove-if (lambda (line) (zerop (length (string-trim '(#\Space #\Tab #\Return) line))))
                          (ppcre:split "\\n" scan))))
    (let* ((md-list (remove-if-not (lambda (line) (ppcre:scan +md-list-line+ line)) lines))
           (ratio (if lines (/ (float (length md-list)) (length lines)) 0.0)))
      (when (and (>= (length md-list) 4) (>= ratio 0.5))
        (let* ((first (car md-list))
               (start (or (search first scan) 0)))
          (note-structural "format.list_heavy" start (+ start (length first)) 2.0))))))

(defun check-text (text &key (surface "chat") threshold)
  "TEXT scored and annotated on SURFACE => a REPORT, as the original's check did."
  (let* ((profile (surface-profile surface))
         (thr (or threshold (surface-value profile :threshold)))
         (text (or text ""))
         (scan (mask-text text))
         (ja-chars (count-if #'ja-char-p scan))
         (japanese (japanese-p scan))
         (report (make-report :surface (car profile) :score 0 :threshold thr
                              :over-threshold nil :japanese japanese :chars ja-chars
                              :findings '() :register-issues 0 :stats '())))
    (when japanese
      (let* ((*profile* profile)
             (*text* text)
             (*findings* '())
             (quotes (quote-spans scan)))
        (scan-rules text scan quotes)
        (let ((prose (prose-sentences scan quotes)))
          (scan-ending-runs prose)
          (scan-nominal-runs prose)
          (scan-register prose profile scan quotes))
        (scan-lists scan)
        (let ((findings (stable-sort *findings*
                                      (lambda (a b)
                                        (or (< (finding-start a) (finding-start b))
                                            (and (= (finding-start a) (finding-start b))
                                                 (string< (finding-rule a) (finding-rule b))))))))
          (setf (report-score report)
                (score-of (/ (loop for f in findings
                                   when (and (finding-counted f)
                                             (not (string= (finding-category f) "register")))
                                     sum (finding-weight f))
                             100.0)
                           ja-chars)
                (report-over-threshold report) (>= (report-score report) thr)
                (report-register-issues report)
                (count-if (lambda (f) (and (finding-counted f)
                                           (string= (finding-category f) "register")))
                          findings)
                (report-findings report) findings))))
    report))

(defun report-sorted-categories (report)
  (let ((totals '()))
    (dolist (f (report-findings report))
      (when (finding-counted f)
        (let ((cell (assoc (finding-category f) totals :test #'string=)))
          (if cell
              (incf (cdr cell) (finding-weight f))
              (push (cons (finding-category f) (finding-weight f)) totals)))))
    (mapcar #'car (stable-sort totals (lambda (a b) (> (cdr a) (cdr b)))))))

(defun report-summary (report)
  "The Japanese one-line verdict the tool and the line under an answer carry."
  (if (not (report-japanese report))
      "日本語の文ではないため検査していません。"
      (let* ((head (format nil "AI的な言い回し ~d/100（~a の基準 ~d）"
                            (report-score report) (report-surface report) (report-threshold report)))
             (findings (report-findings report))
             (n (length findings)))
        (if (zerop n)
            (concatenate 'string head "。指摘なし。")
            (let ((cats (format nil "~{~a~^・~}"
                                (loop for c in (subseq (report-sorted-categories report)
                                                        0 (min 3 (length (report-sorted-categories report))))
                                      collect (or (cdr (assoc c +categories+ :test #'string=)) c))))
                  (over (report-over-threshold report))
                  (register (plusp (report-register-issues report))))
              (format nil "~a。指摘 ~d 件（多い順: ~a）。~a" head n cats
                      (cond ((and over register) "AI的な言い回しが基準を超え、場面の文体にも合いません。書き直しの対象です。")
                            (over "書き直しの対象です。")
                            (register "AI的な言い回しは基準内ですが、場面の文体（敬体など）に合わないため書き直しの対象です。")
                            (t "基準内です。"))))))))

(defun verdict (report)
  (cond ((not (report-japanese report)) "not_japanese")
        ((or (report-over-threshold report) (plusp (report-register-issues report))) "rewrite")
        (t "ok")))

(defun format-report (report &key (limit 10))
  "REPORT as the operator and the model read it: the summary, then each
finding with its line, column, label and fix hint."
  (with-output-to-string (out)
    (write-string (report-summary report) out)
    (loop for f in (subseq (report-findings report) 0 (min limit (length (report-findings report))))
          do (format out "~%- ~d行~d字目 ~a~:[（点数外）~;~]: ~a~%  直し方: ~a"
                     (finding-line f) (finding-col f) (finding-label f)
                     (finding-counted f) (finding-excerpt f) (finding-hint f)))
    (let ((more (- (length (report-findings report))
                   (min limit (length (report-findings report))))))
      (when (plusp more) (format out "~%（ほか ~d 件）" more)))))
