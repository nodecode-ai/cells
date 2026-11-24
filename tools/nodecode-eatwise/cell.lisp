;;;; cell.lisp --- meal nutrition, daily targets and a consented local history.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of harrylabsj/hermes-plugin-eatwise, whose one job was to
;;;; launch the eatwise-mcp Node server over stdio so a host could call its
;;;; nine MCP tools. The engine's arithmetic and its food table live here in
;;;; Common Lisp instead: no Node, no subprocess, no network. Four tools, one
;;;; command, one manual topic.
;;;;
;;;; Everything a person's body is enters through the profile, and it enters
;;;; only after they turn on profile memory: HISTORY_STATUS says where the
;;;; store is, CONFIGURE turns the two switches, SAVE_PROFILE writes. A meal
;;;; is written only after a separate consent (meal recording) and only when
;;;; the caller says the foods and the portions were confirmed.

(in-package #:nodecode-eatwise)

(nlk:define-peripheral eatwise :not-running t)
;;;; --- the food table -------------------------------------------------------------

(defstruct (food (:constructor food (id name aliases category amount unit grams
                                       kcal protein fat carbs fiber sodium tags note source)))
  "One row of the table, nutrition per 100 g. AMOUNT of UNIT weighs GRAMS:
`1 碗' of cooked rice is 160 g, so an amount of 2 is 320 g."
  (id "" :read-only t) (name "" :read-only t) (aliases '() :read-only t)
  (category "" :read-only t) (amount 100 :read-only t) (unit "克" :read-only t)
  (grams 100 :read-only t)
  (kcal 0 :read-only t) (protein 0 :read-only t) (fat 0 :read-only t)
  (carbs 0 :read-only t) (fiber 0 :read-only t) (sodium 0 :read-only t)
  (tags '() :read-only t) (note nil :read-only t) (source "" :read-only t))

(defvar *food-index* nil "Name and alias, downcased, to the food it names.")

(defun food-index ()
  (or *food-index*
      (setf *food-index*
            (let ((table (make-hash-table :test #'equal)))
              (dolist (food *foods*)
                (dolist (name (cons (food-name food) (food-aliases food)))
                  (let ((key (string-downcase name)))
                    (unless (gethash key table) (setf (gethash key table) food)))))
              table))))

(defun find-food (id)
  "The food whose id is ID, or NIL."
  (find id *foods* :key #'food-id :test #'string=))

(defun search-foods (query &key (limit 8))
  "The foods whose name or an alias holds QUERY, best first: an exact name,
then a name beginning with it, then any name holding it."
  (let* ((query (string-downcase (nlk:trimmed (or query ""))))
         (hits '()))
    (when (plusp (length query))
      (dolist (food *foods*)
        (let ((best nil))
          (dolist (name (cons (food-name food) (food-aliases food)))
            (let ((name (string-downcase name)))
              (when (search query name)
                (let ((rank (cond ((string= name query) 0)
                                  ((zerop (or (search query name) 1)) 1)
                                  (t 2))))
                  (when (or (null best) (< rank (car best)) (< (length name) (cdr best)))
                    (setf best (cons rank (length name))))))))
          (when best (push (cons best food) hits)))))
    (mapcar #'cdr (subseq (sort hits (lambda (a b)
                                       (or (< (car (car a)) (car (car b)))
                                           (and (= (car (car a)) (car (car b)))
                                                (< (length (food-name (cdr a)))
                                                   (length (food-name (cdr b))))))))
                        0 (min limit (length hits))))))

;;;; --- arithmetic ------------------------------------------------------------------

(defun round1 (value) (/ (round (* value 10)) 10))

(defun food-item (food amount)
  "FOOD eaten AMOUNT times its default serving, as a plist of the numbers the
original's buildMealItem answered."
  (let* ((grams (* (food-grams food) (/ amount (food-amount food))))
         (factor (/ grams 100)))
    (list :id (food-id food) :name (food-name food) :amount (round1 amount)
          :unit (food-unit food) :grams (round1 grams)
          :calories (round1 (* (food-kcal food) factor))
          :protein (round1 (* (food-protein food) factor))
          :fat (round1 (* (food-fat food) factor))
          :carbs (round1 (* (food-carbs food) factor))
          :fiber (round1 (* (food-fiber food) factor))
          :sodium (round1 (* (food-sodium food) factor))
          :confidence (if (member "估算误差大" (food-tags food) :test #'string=) "low" "medium")
          :source (food-source food)
          :note (food-note food))))

(defun sum-items (items)
  (let ((total '(:calories 0 :protein 0 :fat 0 :carbs 0 :fiber 0 :sodium 0)))
    (dolist (item items)
      (dolist (key '(:calories :protein :fat :carbs :fiber :sodium))
        (setf (getf total key) (+ (getf total key) (or (getf item key) 0)))))
    (dolist (key '(:calories :protein :fat :carbs :fiber :sodium))
      (setf (getf total key) (round1 (getf total key))))
    total))

;;;; --- the profile ------------------------------------------------------------------

(defparameter +genders+ '("male" "female"))
(defparameter +activity-levels+ '("sedentary" "light" "moderate" "high" "extreme"))
(defparameter +goals+ '("fatLoss" "maintain" "muscleGain" "bloodPressure" "bloodLipids"
                        "bloodSugar" "uricAcid"))
(defparameter +health-risks+ '("highBloodPressure" "bloodLipids" "bloodSugar" "highUricAcid"
                               "fattyLiver" "kidney" "cardiovascular"))
(defparameter +life-stages+ '("general" "pregnancy" "breastfeeding"))

(defun member-of (value allowed field)
  (let ((value (and value (princ-to-string value))))
    (unless (member value allowed :test #'string=)
      (fail "~a must be one of ~{~a~^ / ~}" field allowed))
    value))

(defun number-between (value low high field &key integer)
  (unless (numberp value)
    (fail "~a must be a number, not ~s" field value))
  (when (and integer (/= value (round value)))
    (fail "~a must be a whole number" field))
  (unless (<= low value high)
    (fail "~a must be between ~a and ~a" field low high))
  value)

(defun checked-profile (plist)
  "PLIST as the profile the arithmetic runs on, or a refusal naming the field.
The one rule the original called data minimisation: a field unrelated to a
nutrition estimate has no place in it."
  (let ((profile
          (list :gender (member-of (getf plist :gender) +genders+ "gender")
                :age (number-between (getf plist :age) 3 120 "age" :integer t)
                :height-cm (number-between (getf plist :height-cm) 80 230 "height_cm")
                :weight-kg (number-between (getf plist :weight-kg) 20 300 "weight_kg")
                :activity-level (member-of (getf plist :activity-level) +activity-levels+
                                           "activity_level")
                :goal (member-of (getf plist :goal) +goals+ "goal")
                :health-risks (let ((risks (getf plist :health-risks)))
                                (dolist (risk (if (listp risks) risks (and risks (list risks))))
                                  (member-of risk +health-risks+ "health_risks"))
                                (if (listp risks) risks (and risks (list risks))))
                :life-stage (member-of (or (getf plist :life-stage) "general")
                                       +life-stages+ "life_stage"))))
    (when (getf plist :target-weight-kg)
      (setf (getf profile :target-weight-kg)
            (number-between (getf plist :target-weight-kg) 20 300 "target_weight_kg")))
    profile))

(defun kidney-risk-p (profile) (member "kidney" (getf profile :health-risks) :test #'string=))

;;;; --- the safety gate ---------------------------------------------------------------

(defparameter +acute-symptoms+ '("胸痛" "胸闷" "呼吸困难" "晕倒" "昏厥" "剧烈腹痛" "呕血" "便血"
                                 "高烧" "严重头晕" "食物中毒"))
(defparameter +eating-disorder+ '("暴食" "催吐" "厌食" "绝食" "吃了就吐" "不敢吃饭"))
(defparameter +diagnosis-or-meds+ '("诊断" "确诊" "停药" "换药" "吃什么药" "药量" "胰岛素"
                                    "降糖药" "降压药"))
(defparameter +pregnancy-terms+ '("怀孕" "孕期" "孕妇" "哺乳" "月子"))

(defun safety-check (profile text)
  "The original's evaluateSafety: what takes a meal out of ordinary advice.
=> (LEVEL REASONS), LEVEL one of \"ok\" \"caution\" \"refer\"."
  (let ((reasons '()) (refer nil) (text (or text "")))
    (flet ((say (why &key (refers t))
             (push why reasons)
             (when refers (setf refer t))))
      (when (and (getf profile :age) (< (getf profile :age) 18))
        (say "未成年人：生长发育期的营养目标需要医生或注册营养师个体化制定，本结果不适用减脂目标。"))
      (when (member (getf profile :life-stage) '("pregnancy" "breastfeeding") :test #'string=)
        (say "孕期/哺乳期：营养需求变化大，请以产科医生或临床营养师建议为准。"))
      (loop for term in +pregnancy-terms+
            when (search term text)
              do (say (format nil "描述中提到“~a”：孕期/哺乳期相关饮食请以医生建议为准。" term))
                 (return))
      (loop for term in +acute-symptoms+
            when (search term text)
              do (say (format nil "描述中提到“~a”：这可能是急性症状，请及时就医，不要按饮食建议处理。" term))
                 (return))
      (loop for term in +eating-disorder+
            when (search term text)
              do (say (format nil "描述中提到“~a”：疑似进食障碍信号，建议尽快联系医生或心理/营养专业人员。" term))
                 (return))
      (loop for term in +diagnosis-or-meds+
            when (search term text)
              do (say (format nil "描述中提到“~a”：诊断、用药或停药问题请咨询医生，本服务不提供医疗建议。" term))
                 (return))
      (when (kidney-risk-p profile)
        (say "肾功能关注：蛋白质目标和补充方式请以医生或临床营养师建议为准。" :refers nil)))
    (values (cond (refer "refer") (reasons "caution") (t "ok"))
            (nreverse reasons))))

;;;; --- energy and targets -----------------------------------------------------------

(defparameter +activity-factors+ '(("sedentary" . 1.2) ("light" . 1.375) ("moderate" . 1.55)
                                   ("high" . 1.725) ("extreme" . 1.9)))

(defun bmr (profile)
  (let ((base (- (+ (* 10 (getf profile :weight-kg))
                    (* 6.25 (getf profile :height-cm)))
                 (* 5 (getf profile :age)))))
    (if (string= (getf profile :gender) "male") (+ base 5) (- base 161))))

(defun tdee (profile)
  (* (bmr profile)
     (or (cdr (assoc (getf profile :activity-level) +activity-factors+ :test #'string=)) 1.2)))

(defun target-calories (profile total)
  (let* ((height-m (/ (getf profile :height-cm) 100))
         (bmi (/ (getf profile :weight-kg) (* height-m height-m)))
         (goal (getf profile :goal))
         (raw (cond ((string= goal "fatLoss") (- total 400))
                    ((string= goal "muscleGain") (+ total 250))
                    ((string= goal "maintain") total)
                    (t (if (>= bmi 24) (- total 300) total))))
         (floor (if (string= (getf profile :gender) "female") 1200 1500)))
    (max raw floor)))

(defun nutrient-targets (profile calories)
  (let* ((factor (cond ((string= (getf profile :goal) "fatLoss") 1.4)
                        ((string= (getf profile :goal) "muscleGain") 1.6)
                        (t 1.0)))
         (professional (kidney-risk-p profile))
         (protein (* (getf profile :weight-kg) (if professional 1.0 factor)))
         (fat (/ (* calories 0.3) 9))
         (carbs (max 130 (/ (- calories (* protein 4) (* fat 9)) 4))))
    (list :calories calories :protein protein :fat fat :carbs carbs :fiber 25
          :sodium (if (member "highBloodPressure" (getf profile :health-risks) :test #'string=)
                      1500 2300)
          :water (* (getf profile :weight-kg) 30)
          :professional-protein professional)))

(defun daily-targets (profile)
  "The original's buildDailyTargets: (VALUES TARGETS ENERGY ASSUMPTIONS SAFETY-LEVEL),
TARGETS and ENERGY NIL when the safety gate refers."
  (multiple-value-bind (level reasons) (safety-check profile "")
    (if (string= level "refer")
        (values nil nil
                (list "未成年人、孕期或哺乳期的营养需求需要医生或注册营养师个体化制定，本服务不提供成人公式估算值。"
                      (format nil "~{~a~^ ~}" reasons))
                level)
        (let* ((basal (bmr profile)) (total (tdee profile))
               (calories (target-calories profile total))
               (targets (nutrient-targets profile calories))
               (assumptions
                 (list "BMR 按 Mifflin-St Jeor 公式估算，TDEE 按自述活动水平乘以活动系数。"
                       "热量目标按目标类型调整：减脂 -400 kcal，增肌 +250 kcal，慢病管理目标在 BMI≥24 时 -300 kcal；女性下限 1200 kcal，男性下限 1500 kcal。"
                       "蛋白质按体重系数估算（减脂 1.4 g/kg，增肌 1.6 g/kg，其他 1.0 g/kg），脂肪按热量 30% 估算，碳水取剩余热量且不低于 130 g。")))
          (when (getf targets :professional-protein)
            (push "肾功能关注：蛋白质目标按 1.0 g/kg 保守给出，请以医生或临床营养师建议为准。" assumptions))
          (when (member "highBloodPressure" (getf profile :health-risks) :test #'string=)
            (push "高血压关注：钠目标按 1500 mg 收紧（一般人群 2300 mg）。" assumptions))
          (values (list :calories (round calories)
                        :protein (round1 (getf targets :protein))
                        :fat (round1 (getf targets :fat))
                        :carbs (round1 (getf targets :carbs))
                        :fiber (round1 (getf targets :fiber))
                        :sodium (round (getf targets :sodium))
                        :water (round (getf targets :water)))
                  (list :bmr (round basal) :tdee (round total)
                        :target-calories (round calories))
                  (nreverse assumptions)
                  level)))))
;;;; --- one meal, read off the text ---------------------------------------------------

(defparameter +gram-units+ '(("克" . 1) ("g" . 1) ("公克" . 1) ("毫升" . 1) ("ml" . 1)
                             ("两" . 50) ("斤" . 500)))
(defparameter +serving-units+ '("碗" "个" "份" "杯" "盘" "盒" "根" "片" "碟" "串" "只" "条"
                                "块" "袋" "包" "张" "把"))
(defparameter +chinese-digits+
  '(("零" . 0) ("一" . 1) ("二" . 2) ("两" . 2) ("三" . 3) ("四" . 4) ("五" . 5)
    ("六" . 6) ("七" . 7) ("八" . 8) ("九" . 9) ("十" . 10) ("半" . 0.5)))

(defun matched-food (segment)
  "The longest food name or alias SEGMENT holds, as (NAME FOOD), or NIL."
  (let ((best nil))
    (dolist (food *foods*)
      (dolist (name (cons (food-name food) (food-aliases food)))
        (when (and (plusp (length name))
                   (search name segment)
                   (or (null best) (> (length name) (length (first best)))))
          (setf best (list name food)))))
    best))

(defun amount-of (before food)
  "How many default servings the text BEFORE the food's name writes: 1 when it
writes no number."
  (let* ((text (string-right-trim " " (or before "")))
         (unit nil))
    (dolist (word (append +serving-units+ (mapcar #'car +gram-units+)))
      (when (and (null unit) (<= (length word) (length text))
                 (string-equal (subseq text (- (length text) (length word))) word))
        (setf unit word)))
    (let ((head (string-right-trim " " (if unit
                                           (subseq text 0 (- (length text) (length unit)))
                                           text))))
      (let* ((start (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.)))
                                      head :from-end t))
             (digits (and (or (null start) (< start (1- (length head))))
                          (subseq head (if start (1+ start) 0))))
             (number (cond ((and digits (plusp (length digits))
                                  (every (lambda (c) (or (digit-char-p c) (char= c #\.))) digits))
                            (ignore-errors (read-from-string digits)))
                           ((and (plusp (length head))
                                 (cdr (assoc (string (char head (1- (length head))))
                                             +chinese-digits+ :test #'string=)))
                            (cdr (assoc (string (char head (1- (length head))))
                                        +chinese-digits+ :test #'string=)))
                           (t nil))))
        (cond ((and unit (cdr (assoc unit +gram-units+ :test #'string=)))
               (/ (* (or number 1) (cdr (assoc unit +gram-units+ :test #'string=)))
                  (food-grams food)))
              (number (coerce number 'single-float))
              (t 1))))))

(defun parse-meal-text (text)
  "TEXT as ((FOOD . AMOUNT) ...) and the assumptions the reading made:
(VALUES ITEMS UNRECOGNIZED ASSUMPTIONS)."
  (let ((items '()) (unknown '()) (notes '()))
    (dolist (segment (mapcar #'nlk:trimmed
                             (uiop:split-string (or text "")
                                                :separator '(#\， #\, #\、 #\。 #\； #\; #\Space
                                                             #\Tab #\Newline))))
      (when (plusp (length segment))
        (cond
          ((or (search "没" segment) (search "不" segment))
           (push (format nil "“~a”读作否定，未计入。" segment) notes))
          (t
           (let ((match (matched-food segment)))
             (if (null match)
                 (push segment unknown)
                 (let* ((name (first match)) (food (second match))
                        (at (search name segment))
                        (amount (amount-of (subseq segment 0 at) food)))
                   (push (cons food amount) items)
                   (when (= amount 1)
                     (push (format nil "“~a”未写份量，按默认一份（~a ~a，~a 克）估算。"
                                   name (food-amount food) (food-unit food)
                                   (round (food-grams food)))
                           notes)))))))))
    (values (nreverse items) (nreverse unknown) (nreverse notes))))

;;;; --- the local store ---------------------------------------------------------------

(defparameter +initial-store+
  (list :profile-memory nil :meal-recording nil :retention-days 180
        :profile nil :revision 0 :meals '())
  "What an untouched store reads as: nothing consented, nothing written.")

(defun store-path ()
  "Where the profile and the meals live: HISTORY_FILE under the home when it
is relative, as it is when it is absolute, and eatwise/store.json by default."
  (let ((name (nlk:trimmed (or (setting :history-file) ""))))
    (cond ((zerop (length name)) (nlk:home "eatwise/store.json"))
          ((char= (char name 0) #\/) name)
          (t (nlk:home name)))))

(defun meal-of (object)
  "One recorded meal, read off its JSON."
  (list :id (or (nlk:json-value object :string "id") "")
        :local-date (or (nlk:json-value object :string "local_date") "")
        :meal-type (nlk:json-value object :string "meal_type")
        :saved-at (or (nlk:json-value object :string "saved_at") "")
        :calories (or (nlk:json-value object :number "calories") 0)
        :protein (or (nlk:json-value object :number "protein_g") 0)
        :fat (or (nlk:json-value object :number "fat_g") 0)
        :carbs (or (nlk:json-value object :number "carbs_g") 0)
        :fiber (or (nlk:json-value object :number "fiber_g") 0)
        :sodium (or (nlk:json-value object :number "sodium_mg") 0)
        :text (or (nlk:json-value object :string "summary") "")))

(defun profile-of (object)
  (let ((risks (nlk:json-array object "health_risks")))
    (list :gender (nlk:json-value object :string "gender")
          :age (nlk:json-value object :integer "age")
          :height-cm (nlk:json-value object :number "height_cm")
          :weight-kg (nlk:json-value object :number "weight_kg")
          :activity-level (nlk:json-value object :string "activity_level")
          :goal (nlk:json-value object :string "goal")
          :life-stage (or (nlk:json-value object :string "life_stage") "general")
          :health-risks (if risks (loop for risk across risks collect risk) '()))))

(defun read-store ()
  "The store as a plist; an unreadable file is the untouched store, said once."
  (let ((path (store-path)))
    (if (probe-file path)
        (handler-case
            (let ((object (nlk:decode-json (nlk:read-text path))))
              (list :profile-memory (and (nlk:json-value object :integer "profile_memory")
                                         (plusp (nlk:json-value object :integer "profile_memory")))
                    :meal-recording (and (nlk:json-value object :integer "meal_recording")
                                         (plusp (nlk:json-value object :integer "meal_recording")))
                    :retention-days (or (nlk:json-value object :integer "retention_days") 180)
                    :profile (let ((profile (nlk:json-value object :object "profile")))
                               (and profile (profile-of profile)))
                    :revision (or (nlk:json-value object :integer "revision") 0)
                    :meals (loop for meal across (or (nlk:json-array object "meals") #())
                                 collect (meal-of meal))))
          (error () (copy-list +initial-store+)))
        (copy-list +initial-store+))))

(defun profile-object (profile)
  (nlk:make-json-object
   "gender" (getf profile :gender) "age" (getf profile :age)
   "height_cm" (getf profile :height-cm) "weight_kg" (getf profile :weight-kg)
   "activity_level" (getf profile :activity-level) "goal" (getf profile :goal)
   "life_stage" (getf profile :life-stage)
   "health_risks" (or (getf profile :health-risks) '())))

(defun meal-object (meal)
  (nlk:make-json-object
   "id" (getf meal :id) "local_date" (getf meal :local-date)
   "meal_type" (getf meal :meal-type) "saved_at" (getf meal :saved-at)
   "calories" (getf meal :calories) "protein_g" (getf meal :protein)
   "fat_g" (getf meal :fat) "carbs_g" (getf meal :carbs)
   "fiber_g" (getf meal :fiber) "sodium_mg" (getf meal :sodium)
   "summary" (getf meal :text)))

(defun write-store (store)
  "The store written whole, atomically, under its own directory."
  (let* ((path (store-path))
         (object (nlk:make-json-object
                  "profile_memory" (if (getf store :profile-memory) 1 0)
                  "meal_recording" (if (getf store :meal-recording) 1 0)
                  "retention_days" (getf store :retention-days)
                  "profile" (let ((profile (getf store :profile)))
                              (and profile (profile-object profile)))
                  "revision" (getf store :revision)
                  "meals" (mapcar #'meal-object (getf store :meals)))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:pretty-json object) out))
    path))

(defun prune (store)
  "STORE with the meals past its retention dropped: the original's 180 days."
  (let* ((days (or (getf store :retention-days) 180))
         (today (local-today))
         (cutoff (date-shift today (- days))))
    (setf (getf store :meals)
          (remove-if (lambda (meal) (string< (getf meal :local-date) cutoff))
                     (getf store :meals)))
    store))

;;;; --- dates --------------------------------------------------------------------------

(defun local-today ()
  "This machine's date as YYYY-MM-DD."
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

(defun date-shift (date days)
  "DATE, a YYYY-MM-DD string, DAYS days later (negative: earlier)."
  (let ((time (encode-universal-time 0 0 12
                                     (parse-integer date :start 8 :end 10)
                                     (parse-integer date :start 5 :end 7)
                                     (parse-integer date :start 0 :end 4))))
    (multiple-value-bind (sec min hour date month year)
        (decode-universal-time (+ time (* days 86400)))
      (declare (ignore sec min hour))
      (format nil "~4,'0d-~2,'0d-~2,'0d" year month date))))

(defun date-p (text)
  (and (stringp text) (= (length (nlk:trimmed text)) 10)
       (every (lambda (i) (or (/= i 4) (/= i 7))) '(0))
       (let ((text (nlk:trimmed text)))
         (and (every #'digit-char-p (subseq text 0 4))
              (char= (char text 4) #\-)
              (every #'digit-char-p (subseq text 5 7))
              (char= (char text 7) #\-)
              (every #'digit-char-p (subseq text 8 10))))))

;;;; --- one review --------------------------------------------------------------------

(defparameter +meal-shares+ '(("breakfast" . 0.25) ("lunch" . 0.35) ("dinner" . 0.35)
                               ("snack" . 0.1) ("lateNight" . 0.1)))
(defparameter +meal-labels+ '(("breakfast" . "breakfast") ("lunch" . "lunch")
                               ("dinner" . "dinner") ("snack" . "snack")
                               ("lateNight" . "late night")))

(defun classify-upper (value limit)
  (if (or (null limit) (<= limit 0)) "ok"
      (let ((ratio (/ value limit)))
        (cond ((<= ratio 0.9) "ok") ((<= ratio 1.2) "slightly high") (t "high")))))

(defun classify-target (value target)
  (if (or (null target) (<= target 0)) "ok"
      (let ((ratio (/ value target)))
        (cond ((< ratio 0.7) "low") ((< ratio 0.9) "slightly low") ((<= ratio 1.2) "ok")
              ((<= ratio 1.5) "slightly high") (t "high")))))

(defun scale-targets (targets share)
  (list :calories (round1 (* (getf targets :calories) share))
        :protein (round1 (* (getf targets :protein) share))
        :fat (round1 (* (getf targets :fat) share))
        :carbs (round1 (* (getf targets :carbs) share))
        :fiber (round1 (* (getf targets :fiber) share))
        :sodium (round1 (* (getf targets :sodium) share))))

(defun status-list (scope evaluated targets allow-deficit)
  (flet ((up (key target) (cons key (classify-upper (getf evaluated key) (getf target key)))))
    (if (and (string= scope "day") allow-deficit)
        (list (cons :calories (classify-target (getf evaluated :calories) (getf targets :calories)))
              (cons :protein (classify-target (getf evaluated :protein) (getf targets :protein)))
              (cons :fat (classify-target (getf evaluated :fat) (getf targets :fat)))
              (cons :carbs (classify-target (getf evaluated :carbs) (getf targets :carbs)))
              (cons :fiber (classify-target (getf evaluated :fiber) (getf targets :fiber)))
              (cons :sodium (classify-upper (getf evaluated :sodium) (getf targets :sodium))))
        (list (up :calories targets) (up :fat targets) (up :sodium targets)))))

(defun fmt (value) (format nil "~,1f" (coerce (or value 0) 'double-float)))

(defun review (text items meal-type profile today-before day-complete)
  "One meal against the profile in hand. ITEMS, when given, is already
confirmed ((FOOD . AMOUNT) ...); otherwise TEXT is read off the table.
=> a plist; :recognized items with their numbers, :unrecognized text, and
the numbers, unless the safety gate refers."
  (let ((parsed '()) (unknown '()) (notes '()))
    (if items
        (setf parsed items)
        (multiple-value-setq (parsed unknown notes) (parse-meal-text text)))
    (let* ((recognized (mapcar (lambda (pair) (food-item (car pair) (cdr pair))) parsed))
           (nutrition (sum-items recognized))
           (confidence (cond ((null recognized) "low")
                             ((some (lambda (item) (string= (getf item :confidence) "low"))
                                    recognized) "low")
                             (t "medium")))
           (assumptions (append notes
                                 (remove nil (mapcar (lambda (item) (getf item :note)) recognized))
                                 (list "未写份量的按常见默认份量估算；无法换算的数量、否定或修订描述不计入。"))))
      (when unknown
        (push "部分描述未识别或份量无法换算，未计入估算；信息不完整，暂停整体评价。" assumptions))
      (multiple-value-bind (level reasons) (safety-check profile (or text ""))
        (cond
          ((string= level "refer")
           (list :recognized recognized :unrecognized unknown :nutrition nutrition
                 :assumptions assumptions :safety-level level :safety-reasons reasons
                 :confidence "low" :scope "meal" :assessment "safety_referral"
                 :action "这类情况请先咨询医生或注册营养师，本服务不提供饮食行动建议。"))
          ((or unknown (null profile))
           (list :recognized recognized :unrecognized unknown :nutrition nutrition
                 :assumptions (append assumptions
                                      (unless profile
                                        (list "未建档：仅按食物库计算，不使用默认个人资料或成人目标公式。")))
                 :safety-level level :safety-reasons reasons
                 :confidence (if unknown "low" confidence)
                 :scope "meal" :assessment (if unknown "incomplete" "meal_information")
                 :action (if unknown
                             "请补充未计入食物或确认份量后再复盘。"
                             "下一餐可参考主食、蛋白质食物和蔬菜的组合；这是一般搭配信息，不是个人摄入目标。")))
          (t
           (multiple-value-bind (targets energy target-assumptions target-level)
               (daily-targets profile)
             (declare (ignore energy))
             (if (null targets)
                 (list :recognized recognized :unrecognized unknown :nutrition nutrition
                       :assumptions (append assumptions target-assumptions)
                       :safety-level level :safety-reasons reasons :confidence confidence
                       :scope "meal" :assessment "safety_referral"
                       :action "这类情况请先咨询医生或注册营养师，本服务不提供饮食行动建议。")
                 (let* ((share (or (cdr (assoc meal-type +meal-shares+ :test #'string=))
                                    (/ (setting :meal-share-percent) 100)))
                        (reference (scale-targets targets share))
                        (scope (if today-before "day" "meal"))
                        (day-totals (and today-before (sum-items (list today-before nutrition))))
                        (evaluated (or day-totals nutrition))
                        (comparison (if (string= scope "day") targets reference))
                        (status (status-list scope evaluated comparison day-complete))
                        (worst (find-if (lambda (pair) (member (cdr pair) '("high" "slightly high")
                                                                :test #'string=))
                                         status)))
                   (push (format nil "每日目标基于 Mifflin-St Jeor 公式和自述活动水平估算，存在个体差异。")
                         assumptions)
                   (push (if (string= scope "day")
                             (if day-complete
                                 "已提供 today_before 且 day_complete=true：按完整全天累计评估，可判断摄入不足。"
                                 "已提供 today_before：按截至目前累计评估；当天尚未记录完成，只判断明显过量，不判断摄入不足。")
                             (format nil "未提供 today_before：本次只评价当前这一餐，对照~a参考份额（约全天目标的 ~d%），不做全天判断。"
                                     (or (cdr (assoc meal-type +meal-labels+ :test #'string=)) "该餐")
                                     (round (* share 100))))
                         assumptions)
                   (list :recognized recognized :unrecognized unknown :nutrition nutrition
                         :day-totals day-totals :targets targets :reference reference
                         :status status :scope scope :energy energy
                         :assumptions (append (nreverse assumptions) target-assumptions)
                         :safety-level (if (string= level "ok") target-level level)
                         :safety-reasons reasons :confidence confidence
                         :assessment (cond (worst "over_limit") (t "meal_information"))
                         :action (cond ((null worst)
                                        "这一餐没有明显超出参考；下一餐继续把主食、蛋白质食物和蔬菜配齐。")
                                       ((string= (car worst) :sodium)
                                        "这一餐的钠偏高：下一餐少汤汁、少腌制品，主食照常。")
                                       ((string= (car worst) :fat)
                                        "这一餐的脂肪偏高：下一餐把油炸和浓酱换成清炒或蒸煮。")
                                       (t "这一餐的热量偏高：下一餐减一份主食，蛋白质照旧。"))))))))))))

;;;; --- rendering ----------------------------------------------------------------------

(defun item-line (item)
  (format nil "  ~a (~a) ~a × ~a (~a g): ~a kcal, protein ~a g, fat ~a g, carbs ~a g, fibre ~a g, sodium ~a mg [~a, ~a]"
          (getf item :name) (getf item :id) (fmt (getf item :amount))
          (getf item :unit) (fmt (getf item :grams)) (fmt (getf item :calories))
          (fmt (getf item :protein)) (fmt (getf item :fat)) (fmt (getf item :carbs))
          (fmt (getf item :fiber)) (fmt (getf item :sodium)) (getf item :confidence)
          (getf item :source)))

(defun nutrition-line (label nutrition)
  (format nil "~a: ~a kcal | protein ~a g | fat ~a g | carbs ~a g | fibre ~a g | sodium ~a mg"
          label (fmt (getf nutrition :calories)) (fmt (getf nutrition :protein))
          (fmt (getf nutrition :fat)) (fmt (getf nutrition :carbs))
          (fmt (getf nutrition :fiber)) (fmt (getf nutrition :sodium))))

(defun render-review (result)
  (with-output-to-string (out)
    (format out "RECOGNIZED~%")
    (dolist (item (getf result :recognized))
      (format out "~a~%" (item-line item)))
    (unless (getf result :recognized) (format out "  (none recognized)~%"))
    (format out "~a~%" (nutrition-line "MEAL" (getf result :nutrition)))
    (when (getf result :unrecognized)
      (format out "UNRECOGNIZED: ~{~a~^, ~}~%" (getf result :unrecognized)))
    (when (getf result :targets)
      (format out "DAILY TARGETS: ~a~%" (nutrition-line "targets" (getf result :targets)))
      (when (getf result :reference)
        (format out "REFERENCE FOR THIS MEAL: ~a~%"
                (nutrition-line "reference" (getf result :reference))))
      (when (getf result :day-totals)
        (format out "~a~%" (nutrition-line "DAY SO FAR (recorded meals + this one)"
                                            (getf result :day-totals)))))
    (when (getf result :status)
      (format out "STATUS: ~{~a~^, ~}~%"
              (loop for (key . class) in (getf result :status)
                    collect (format nil "~(~a~) ~a" key class))))
    (format out "ASSESSMENT: ~a (confidence ~a)~%"
            (getf result :assessment) (getf result :confidence))
    (format out "ACTION: ~a~%" (getf result :action))
    (format out "ASSUMPTIONS:~%~{  - ~a~%~}" (getf result :assumptions))
    (format out "SAFETY: ~a~@[ (~{~a~^ ~})~]~%"
            (getf result :safety-level) (getf result :safety-reasons))))

;;;; --- the boundaries ----------------------------------------------------------------

(nlk:define-contract meal-item
  (:doc "One food the person confirmed, by the id eatwise_foods answered and the number of that food's default servings.")
  ("food_id" :text :required t :doc "the food's id, as eatwise_foods answered it")
  ("amount" :number :required t :doc "how many of the food's default servings; 2 of a 1 碗 serving of rice is two bowls")
  ("name" :text :doc "the food's name, when it is passed back as a check"))

(nlk:define-contract nutrition-totals
  (:doc "What was already eaten today before this meal, as eatwise_review_meal answered it.")
  ("calories" :number :doc "kcal so far")
  ("protein_g" :number :doc "protein grams so far")
  ("fat_g" :number :doc "fat grams so far")
  ("carbs_g" :number :doc "carbohydrate grams so far")
  ("fiber_g" :number :doc "fibre grams so far")
  ("sodium_mg" :number :doc "sodium milligrams so far"))

(nlk:define-contract foods-request
  (:doc "Search the local food table (467 entries: Chinese and Taiwanese dishes, staples and their servings) by name or alias. Every number in a review comes from this table, so look a food up here instead of estimating it yourself.")
  ("query" :text :required t :doc "a Chinese food name or part of one, such as 米饭, 红烧肉 or 粥")
  ("limit" :integer :default 8 :doc "how many entries to answer, best match first"))

(nlk:define-contract targets-request
  (:doc "Daily energy and nutrient targets from the profile on this machine, by the Mifflin-St Jeor estimate the original engine used: BMR, TDEE, target calories, protein, fat, carbohydrate, fibre, sodium and water. Answers the profile's stored values when none are passed; refuses for a minor, pregnancy or breastfeeding, which need a professional.")
  ("gender" :text :doc "male or female")
  ("age" :integer :doc "years")
  ("height_cm" :number :doc "centimetres")
  ("weight_kg" :number :doc "kilograms")
  ("activity_level" :text :doc "sedentary, light, moderate, high or extreme")
  ("goal" :text :doc "fatLoss, maintain, muscleGain, bloodPressure, bloodLipids, bloodSugar or uricAcid")
  ("health_risks" :array :of :string :doc "any of highBloodPressure, bloodLipids, bloodSugar, highUricAcid, fattyLiver, kidney, cardiovascular")
  ("life_stage" :text :doc "general, pregnancy or breastfeeding"))

(nlk:define-contract review-request
  (:doc "Review one meal deterministically against the food table and, when a profile is on this machine, the day's targets: the numbers per food, the meal total, what it is against the targets, one action and the assumptions. Pass confirmed items (the foods and portions the person agreed to) or a meal description; only a confirmed meal can be saved.")
  ("meal_text" :text :doc "what was eaten, in Chinese, when the person has not picked foods from the table")
  ("items" :array :of meal-item :doc "the confirmed foods and portions; takes precedence over meal_text")
  ("meal_type" :text :doc "breakfast, lunch, dinner, snack or lateNight")
  ("confirmed" :boolean :doc "true only once the person confirmed the foods and the portions")
  ("today_before" :object :of nutrition-totals :doc "what was already eaten today, to judge the day rather than the meal")
  ("day_complete" :boolean :doc "true only when the person said the day's eating is fully recorded")
  ("local_date" :text :doc "YYYY-MM-DD, the day the meal happened; needed only to save")
  ("save" :boolean :doc "save the meal to the local history; needs meal recording to be on and confirmed=true")
  ("request_id" :text :doc "an idempotency id of 8 to 128 letters, digits, - or _, needed only to save"))

(nlk:define-contract history-request
  (:doc "The local store on this machine: whether the person agreed to keep a profile and their meals, what is stored, and the meals themselves. Nothing is written until configure turns the switches on, and a profile never holds a name, phone or id number.")
  ("action" :text :required t :doc "status, configure, profile, save_profile, list, day or delete")
  ("profile_memory" :boolean :doc "configure: keep the profile on this machine")
  ("meal_recording" :boolean :doc "configure: keep the meals on this machine")
  ("retention_days" :integer :doc "configure: how many days a meal is kept")
  ("gender" :text :doc "save_profile: male or female")
  ("age" :integer :doc "save_profile: years")
  ("height_cm" :number :doc "save_profile: centimetres")
  ("weight_kg" :number :doc "save_profile: kilograms")
  ("activity_level" :text :doc "save_profile: sedentary, light, moderate, high or extreme")
  ("goal" :text :doc "save_profile: fatLoss, maintain, muscleGain or a managed condition")
  ("health_risks" :array :of :string :doc "save_profile: the risks to keep in mind")
  ("life_stage" :text :doc "save_profile: general, pregnancy or breastfeeding")
  ("local_date" :text :doc "day: YYYY-MM-DD; list: the first day")
  ("limit" :integer :doc "list: how many meals, at most 50")
  ("scope" :text :doc "delete: profile, meals or all"))

;;;; --- the tools ---------------------------------------------------------------------

(defun guarded (body)
  (handler-case (funcall body)
    (eatwise-error (condition) (nle:failure (format nil "ERROR: EATWISE-ERROR: ~a" condition)))
    (error (condition) (nle:failure (format nil "ERROR: EATWISE-ERROR: ~a" condition)))))

(defun answer-foods (request)
  (guarded
   (lambda ()
     (let ((hits (search-foods (foods-request-query request)
                                :limit (or (foods-request-limit request) 8))))
       (if (null hits)
           (format nil "No food in the table matches ~s. Try a shorter name." (foods-request-query request))
           (with-output-to-string (out)
             (dolist (food hits)
               (format out "~a | ~a~@[ (~{~a~^, ~})~] | 1 ~a = ~a g | ~a kcal, protein ~a g, fat ~a g, carbs ~a g, fibre ~a g, sodium ~a mg per 100 g | ~a~@[ | ~a~]~%"
                       (food-id food) (food-name food) (food-aliases food)
                       (food-unit food) (fmt (food-grams food)) (fmt (food-kcal food))
                       (fmt (food-protein food)) (fmt (food-fat food)) (fmt (food-carbs food))
                       (fmt (food-fiber food)) (fmt (food-sodium food)) (food-source food)
                       (food-note food)))))))))

(defun profile-from (request)
  "The profile the request carries, or the one on this machine, or NIL."
  (let ((given (list :gender (targets-request-gender request)
                      :age (targets-request-age request)
                      :height-cm (targets-request-height-cm request)
                      :weight-kg (targets-request-weight-kg request)
                      :activity-level (targets-request-activity-level request)
                      :goal (targets-request-goal request)
                      :health-risks (let ((risks (targets-request-health-risks request)))
                                      (if risks (coerce risks 'list) '()))
                      :life-stage (targets-request-life-stage request))))
    (cond ((getf given :gender) (checked-profile given))
          (t (let ((store (read-store)))
               (and (getf store :profile-memory) (getf store :profile)))))))

(defun answer-targets (request)
  (guarded
   (lambda ()
     (let ((profile (profile-from request)))
       (unless profile
         (fail "no profile: pass gender, age, height_cm, weight_kg, activity_level and goal, or turn on profile memory and save_profile first"))
       (multiple-value-bind (targets energy assumptions level)
           (daily-targets profile)
         (with-output-to-string (out)
           (if (null targets)
               (format out "SAFETY: referral — no numeric target.~%~{  - ~a~%~}" assumptions)
               (format out "ENERGY: BMR ~a kcal, TDEE ~a kcal, target ~a kcal~%DAILY TARGETS: ~a~%WATER: ~a ml~%SAFETY: ~a~%~{  - ~a~%~}"
                       (getf energy :bmr) (getf energy :tdee) (getf energy :target-calories)
                       (nutrition-line "targets" targets) (getf targets :water) level assumptions))))))))

(defun confirmed-pairs (request)
  "The request's confirmed items as ((FOOD . AMOUNT) ...), or a refusal."
  (let ((pairs '()))
    (loop for item across (or (review-request-items request) #())
          do (let* ((id (meal-item-food-id item))
                    (food (find-food id))
                    (amount (meal-item-amount item)))
               (unless food
                 (fail "~s is not a food id in the table; search with eatwise_foods" id))
               (unless (and (numberp amount) (plusp amount) (<= amount 100))
                 (fail "the amount of ~a must be more than 0 and at most 100 servings" (food-name food)))
               (let ((name (meal-item-name item)))
                 (when (and name (plusp (length name)) (not (string= name (food-name food))))
                   (fail "~s is not the name of ~a" name id)))
               (when (assoc food pairs) (fail "~a is listed twice" (food-name food)))
               (push (cons food amount) pairs)))
    (nreverse pairs)))

(defun save-meal (result meal-type local-date request-id text)
  "The reviewed meal written to the store: (VALUES PATH MEAL)."
  (let* ((store (read-store)))
    (unless (getf store :meal-recording)
      (fail "meal recording is off: the person has to turn it on with the history tool's configure action, after saying yes"))
    (let* ((nutrition (getf result :nutrition))
           (meal (list :id (subseq (nlk:sha256-text (format nil "~a|~a|~a|~a|~a"
                                                             local-date (or meal-type "")
                                                             (getf nutrition :calories)
                                                             request-id text))
                                   0 16)
                       :local-date local-date :meal-type meal-type
                       :saved-at (nlk:iso-now) :calories (getf nutrition :calories)
                       :protein (getf nutrition :protein) :fat (getf nutrition :fat)
                       :carbs (getf nutrition :carbs) :fiber (getf nutrition :fiber)
                       :sodium (getf nutrition :sodium)
                       :text (format nil "~{~a~^，~}"
                                     (mapcar (lambda (item)
                                               (format nil "~a×~a~a" (fmt (getf item :amount))
                                                       (getf item :unit) (getf item :name)))
                                             (getf result :recognized))))))
      (setf (getf store :meals) (cons meal (getf store :meals)))
      (values (write-store (prune store)) meal))))

(defun answer-review (request)
  (guarded
   (lambda ()
     (unless (review-request-confirmed request)
       (fail "not confirmed: ask the person to confirm the foods and the portions (confirmed=true) before any number is computed or saved"))
     (let* ((store (read-store))
            (profile (and (getf store :profile-memory) (getf store :profile)))
            (items (confirmed-pairs request))
            (text (review-request-meal-text request))
            (today (review-request-today-before request))
            (before (and today (list :calories (or (nutrition-totals-calories today) 0)
                                     :protein (or (nutrition-totals-protein-g today) 0)
                                     :fat (or (nutrition-totals-fat-g today) 0)
                                     :carbs (or (nutrition-totals-carbs-g today) 0)
                                     :fiber (or (nutrition-totals-fiber-g today) 0)
                                     :sodium (or (nutrition-totals-sodium-mg today) 0))))
            (result (review text items (review-request-meal-type request) profile before
                            (review-request-day-complete request))))
       (when (and (null items) (or (null text) (zerop (length (nlk:trimmed text)))))
         (fail "nothing to review: pass items or meal_text"))
       (when (and (null items) (null (getf result :recognized)))
         (fail "no food in the description matches the table; look the foods up with eatwise_foods and pass items"))
       (if (review-request-save request)
           (let* ((date (review-request-local-date request))
                  (request-id (review-request-request-id request)))
             (unless (date-p date) (fail "local_date must be a YYYY-MM-DD date"))
             (unless (and request-id (<= 8 (length request-id) 128))
               (fail "request_id of 8 to 128 characters is needed to save"))
             (multiple-value-bind (path meal)
                 (save-meal result (review-request-meal-type request) date request-id text)
               (format nil "~aSAVED: ~a ~a (~a) in ~a~%"
                       (render-review result) date (getf meal :meal-type) (getf meal :id) path)))
           (render-review result))))))

(defun status-text (store)
  (format nil "STORE: ~a~%PROFILE MEMORY: ~a~%MEAL RECORDING: ~a~%RETENTION: ~a days~%MEALS: ~a~%PROFILE: ~a~%"
          (store-path) (if (getf store :profile-memory) "on" "off")
          (if (getf store :meal-recording) "on" "off")
          (getf store :retention-days) (length (getf store :meals))
          (if (getf store :profile) "on file" "none")))

(defun configure-text (store request)
  (setf (getf store :profile-memory) (and (history-request-profile-memory request) t)
        (getf store :meal-recording) (and (history-request-meal-recording request) t)
        (getf store :retention-days)
        (or (history-request-retention-days request) (getf store :retention-days)))
  (write-store (prune store))
  (format nil "PROFILE MEMORY: ~a~%MEAL RECORDING: ~a~%RETENTION: ~a days~%Turning a switch off keeps what is already stored; delete removes it.~%"
          (if (getf store :profile-memory) "on" "off")
          (if (getf store :meal-recording) "on" "off")
          (getf store :retention-days)))

(defun profile-text (store)
  (unless (getf store :profile-memory)
    (fail "profile memory is off: nothing is read until the person turns it on"))
  (let ((profile (getf store :profile)))
    (if (null profile)
        "PROFILE: none yet. Ask for gender, age, height, weight, activity level and goal, then save_profile."
        (format nil "PROFILE: ~a, ~a years, ~a cm, ~a kg, activity ~a, goal ~a, life stage ~a~@[, risks ~{~a~^, ~}~]~%"
                (getf profile :gender) (getf profile :age) (getf profile :height-cm)
                (getf profile :weight-kg) (getf profile :activity-level)
                (getf profile :goal) (getf profile :life-stage)
                (getf profile :health-risks)))))

(defun save-profile-text (store request)
  (unless (getf store :profile-memory)
    (fail "profile memory is off: turn it on with configure, after the person agrees"))
  (let* ((risks (history-request-health-risks request))
         (profile (checked-profile
                   (list :gender (history-request-gender request)
                         :age (history-request-age request)
                         :height-cm (history-request-height-cm request)
                         :weight-kg (history-request-weight-kg request)
                         :activity-level (history-request-activity-level request)
                         :goal (history-request-goal request)
                         :health-risks (if risks (coerce risks 'list) '())
                         :life-stage (history-request-life-stage request)))))
    (setf (getf store :profile) profile
          (getf store :revision) (1+ (getf store :revision)))
    (format nil "PROFILE SAVED (revision ~a) in ~a~%" (getf store :revision)
            (write-store store))))

(defun list-text (store request)
  (let* ((from (history-request-local-date request))
         (limit (min 50 (or (history-request-limit request) 50)))
         (meals (remove-if (lambda (meal)
                             (and from (string< (getf meal :local-date) from)))
                           (getf store :meals))))
    (if (null meals)
        "No meal recorded yet."
        (with-output-to-string (out)
          (loop for meal in (subseq meals 0 (min limit (length meals)))
                do (format out "~a ~a ~a: ~a kcal, protein ~a g, fat ~a g, carbs ~a g, sodium ~a mg — ~a~%"
                           (getf meal :local-date) (or (getf meal :meal-type) "")
                           (getf meal :id) (fmt (getf meal :calories))
                           (fmt (getf meal :protein)) (fmt (getf meal :fat))
                           (fmt (getf meal :carbs)) (fmt (getf meal :sodium))
                           (getf meal :text)))))))

(defun day-text (store request)
  (let ((date (history-request-local-date request)))
    (unless (date-p date) (fail "day needs local_date as YYYY-MM-DD"))
    (let* ((meals (remove-if-not (lambda (meal) (string= (getf meal :local-date) date))
                                  (getf store :meals)))
           (totals (sum-items meals))
           (profile (and (getf store :profile-memory) (getf store :profile))))
      (with-output-to-string (out)
        (format out "~a: ~d recorded meal~:p~%~a~%" date (length meals)
                (nutrition-line "RECORDED" totals))
        (when profile
          (multiple-value-bind (targets energy)
              (daily-targets profile)
            (declare (ignore energy))
            (when targets
              (format out "~a~%STATUS: ~{~a~^, ~}~%"
                      (nutrition-line "DAILY TARGETS" targets)
                      (loop for (key . class) in (status-list "day" totals targets nil)
                            collect (format nil "~(~a~) ~a" key class))))))))))

(defun delete-text (store request)
  (let ((scope (string-downcase (nlk:trimmed (or (history-request-scope request) "")))))
    (cond ((string= scope "profile") (setf (getf store :profile) nil))
          ((string= scope "meals") (setf (getf store :meals) '()))
          ((string= scope "all") (setf store (copy-list +initial-store+)))
          (t (fail "delete needs scope: profile, meals or all")))
    (write-store store)
    (format nil "DELETED (~a). PROFILE MEMORY: ~a, MEAL RECORDING: ~a keep what is left on the next visit.~%"
            scope (if (getf store :profile-memory) "on" "off")
            (if (getf store :meal-recording) "on" "off"))))

(defun answer-history (request)
  (guarded
   (lambda ()
     (let ((action (string-downcase (nlk:trimmed (history-request-action request))))
           (store (read-store)))
       (cond ((string= action "status") (status-text store))
             ((string= action "configure") (configure-text store request))
             ((string= action "profile") (profile-text store))
             ((string= action "save_profile") (save-profile-text store request))
             ((string= action "list") (list-text store request))
             ((string= action "day") (day-text store request))
             ((string= action "delete") (delete-text store request))
             (t (fail "unknown action ~s: status, configure, profile, save_profile, list, day or delete"
                      (history-request-action request))))))))

;;;; --- /eatwise ---------------------------------------------------------------------

(defparameter +manual+
  "Eatwise is a local-first Chinese meal coach. Every number comes from the
table compiled into this cell (467 foods: TFND and USDA FNDDS entries and
derived cooked-dish recipes) and from Mifflin-St Jeor arithmetic, never from
an estimate the model wrote itself.
  eatwise_foods QUERY        the entries a meal is likely to name, with their
                              id, one serving in grams and the numbers per 100 g
  eatwise_targets             the day's targets from the profile on this
                              machine, or from the fields passed
  eatwise_review_meal        one meal against the table and the day's targets;
                              items are the confirmed foods, and save=true
                              writes it once meal recording is on
  eatwise_history             status, configure, profile, save_profile, list,
                              day and delete, over the store on this machine
Consent is two switches: profile memory, then meal recording. Nothing is
written before the person agrees, a profile holds only gender, age, height,
weight, activity level, goal, health risks and life stage, and turning a
switch off keeps what is stored -- delete removes it.
Confirm first: a meal's foods and portions are the person's to confirm, and
every call that computes or saves needs confirmed=true. A minor, a pregnancy
or a breastfeeding stage goes to a professional instead of a numeric target.
/eatwise            the store's state
/eatwise 2026-11-08 the recorded meals of that day
/eatwise 2碗白饭 红烧肉   a review of what you type, from the table"
  "What (help :eatwise) answers while the cell runs.")

(defun run-eatwise (args session-id)
  "/eatwise: the store's state, one day's record, or a meal typed after it."
  (declare (ignore session-id))
  (let ((args (nlk:trimmed (or args ""))))
    (handler-case
        (cond
          ((zerop (length args))
           (answer-history (decode-history-request (nlk:make-json-object "action" "status"))))
          ((date-p args)
           (answer-history (decode-history-request (nlk:make-json-object "action" "day" "local_date" args))))
          (t
           (let* ((store (read-store))
                  (profile (and (getf store :profile-memory) (getf store :profile))))
             (render-review (review args nil nil profile nil nil)))))
      (error (condition) (format nil "~a" condition)))))

;;;; --- the cell -----------------------------------------------------------------------

(nle:define-cell eatwise
  (:section ("eatwise")
    (:guide "every member is optional; history_file is where the profile and the meals live; meal_share_percent is the share of the day one meal is compared with when its type is not named; retention_days bounds how long a meal is kept")
    ("history_file" :string :default ""
                    :doc "where the profile and the meal history live; empty is eatwise/store.json under the Nodecode home, a relative path is read under it")
    ("meal_share_percent" :integer :default 33 :min 10
                          :doc "the share of the day's targets one meal is compared with when the meal type is not named")
    ("retention_days" :integer :default 180 :min 1
                      :doc "how many days a recorded meal is kept"))
  (:tool "eatwise_foods" (nlk:contract 'foods-request) 'answer-foods)
  (:tool "eatwise_targets" (nlk:contract 'targets-request) 'answer-targets)
  (:tool "eatwise_review_meal" (nlk:contract 'review-request) 'answer-review)
  (:tool "eatwise_history" (nlk:contract 'history-request) 'answer-history)
  (:command "eatwise" 'run-eatwise
            :description "Today's recorded meals, or a meal typed after it"
            :argument-hint "MEAL TEXT or YYYY-MM-DD")
  (:help :eatwise
         "eatwise: a local-first Chinese meal coach — food table, daily targets, consented meal history"
         +manual+))
