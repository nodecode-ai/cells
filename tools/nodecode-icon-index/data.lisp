;;;; data.lisp --- the icon catalog, bundled with the cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original shipped as data/icons.json, as one list a layer can
;;;; rebind. Search, the metadata, the file names and the embed code all read
;;;; this and nothing else: only the icon files themselves come off the
;;;; network, from the pinned CDN path the section names in asset_base.

(in-package #:nodecode-icon-index)

(defparameter *catalog*
  '(
    (:id "rocket"
     :title "Rocket"
     :category "Tech"
     :description "A cartoon rocket for launches, new releases and growth."
     :tags ("launch" "startup" "growth" "boost" "ship" "release" "space" "new"))
    (:id "robot"
     :title "Robot"
     :category "Tech"
     :description "A friendly robot head for AI, agents and automation."
     :tags ("ai" "bot" "assistant" "agent" "automation" "machine" "chatbot"))
    (:id "chip"
     :title "Chip"
     :category "Tech"
     :description "A computer chip for hardware, compute and AI models."
     :tags ("processor" "cpu" "hardware" "ai" "compute" "semiconductor" "gpu"))
    (:id "cloud"
     :title "Cloud"
     :category "Tech"
     :description "A puffy cloud for hosting, storage and sync."
     :tags ("storage" "sync" "hosting" "upload" "backup" "server" "saas" "weather"))
    (:id "bolt"
     :title "Lightning Bolt"
     :category "Tech"
     :description "A lightning bolt for speed, power and instant results."
     :tags ("fast" "speed" "power" "energy" "instant" "electric" "performance"))
    (:id "key"
     :title "Key"
     :category "Tech"
     :description "A key for access, logins and API keys."
     :tags ("password" "access" "login" "api" "auth" "unlock" "credentials"))
    (:id "lock"
     :title "Padlock"
     :category "Tech"
     :description "A padlock for security, privacy and protection."
     :tags ("security" "privacy" "secure" "protect" "encryption" "safe" "locked"))
    (:id "gear"
     :title "Gear"
     :category "Tech"
     :description "A cog wheel for settings and how things work."
     :tags ("settings" "config" "cog" "options" "engineering" "preferences" "tools"))
    (:id "laptop"
     :title "Laptop"
     :category "Tech"
     :description "An open laptop for software, coding and remote work."
     :tags ("computer" "work" "coding" "dev" "desktop" "web" "app" "remote"))
    (:id "phone"
     :title "Smartphone"
     :category "Tech"
     :description "A smartphone for mobile apps and devices."
     :tags ("mobile" "app" "iphone" "android" "device" "sms"))
    (:id "magic"
     :title "Magic Wand"
     :category "Tech"
     :description "A magic wand with sparkles for AI and one-click magic."
     :tags ("ai" "magic" "generate" "sparkle" "automate" "create" "wizard"))
    (:id "globe"
     :title "Globe"
     :category "Tech"
     :description "A globe for going global, the web and languages."
     :tags ("world" "global" "internet" "web" "international" "earth" "language"))
    (:id "shield"
     :title "Shield"
     :category "Tech"
     :description "A shield with a check mark for trust and protection."
     :tags ("security" "protection" "verified" "safe" "trust" "guard" "insurance"))
    (:id "wallet"
     :title "Wallet"
     :category "Money"
     :description "A wallet with a coin for payments and money."
     :tags ("money" "payments" "cash" "budget" "finance" "savings" "crypto"))
    (:id "coins"
     :title "Coins"
     :category "Money"
     :description "A stack of coins for earnings, pricing and revenue."
     :tags ("money" "cash" "earnings" "revenue" "price" "cost" "crypto" "tokens"))
    (:id "card"
     :title "Credit Card"
     :category "Money"
     :description "A credit card for checkout, billing and subscriptions."
     :tags ("payment" "checkout" "billing" "subscription" "bank" "debit"))
    (:id "chart"
     :title "Growth Chart"
     :category "Money"
     :description "A rising bar chart for growth, analytics and stats."
     :tags ("analytics" "stats" "growth" "revenue" "graph" "metrics" "data" "increase"))
    (:id "piggy"
     :title "Piggy Bank"
     :category "Money"
     :description "A piggy bank for savings and budgeting."
     :tags ("savings" "save" "budget" "money" "finance" "deposit"))
    (:id "receipt"
     :title "Receipt"
     :category "Money"
     :description "A paper receipt for invoices, orders and expenses."
     :tags ("invoice" "bill" "purchase" "expense" "order" "accounting"))
    (:id "bag"
     :title "Shopping Bag"
     :category "Money"
     :description "A shopping bag for stores and ecommerce."
     :tags ("shop" "store" "ecommerce" "buy" "retail" "purchase"))
    (:id "cart"
     :title "Shopping Cart"
     :category "Money"
     :description "A shopping cart for checkout and online stores."
     :tags ("cart" "checkout" "ecommerce" "shop" "buy" "order"))
    (:id "gift"
     :title "Gift"
     :category "Money"
     :description "A gift box for rewards, offers and giveaways."
     :tags ("present" "reward" "bonus" "offer" "referral" "giveaway" "free"))
    (:id "heart"
     :title "Heart"
     :category "Social"
     :description "A puffy heart for likes, love and favourites."
     :tags ("like" "love" "favourite" "health" "care" "save"))
    (:id "chat"
     :title "Chat Bubble"
     :category "Social"
     :description "A speech bubble for chat, comments and support."
     :tags ("message" "comment" "talk" "support" "dm" "conversation" "reply"))
    (:id "bell"
     :title "Bell"
     :category "Social"
     :description "A bell for notifications, alerts and reminders."
     :tags ("notification" "alert" "reminder" "ping" "subscribe"))
    (:id "camera"
     :title "Camera"
     :category "Social"
     :description "A camera for photos and content creation."
     :tags ("photo" "photography" "picture" "content" "creator" "image"))
    (:id "mic"
     :title "Microphone"
     :category "Social"
     :description "A podcast microphone for audio, voice and podcasts."
     :tags ("podcast" "audio" "voice" "record" "speak" "sound"))
    (:id "play"
     :title "Play Button"
     :category "Social"
     :description "A play button for videos, streams and reels."
     :tags ("video" "play" "youtube" "stream" "watch" "media" "reel"))
    (:id "megaphone"
     :title "Megaphone"
     :category "Social"
     :description "A megaphone for announcements and marketing."
     :tags ("announce" "marketing" "promo" "news" "launch" "broadcast" "ads"))
    (:id "star"
     :title "Star"
     :category "Social"
     :description "A puffy star for ratings, reviews and premium."
     :tags ("rating" "review" "favourite" "premium" "featured" "best"))
    (:id "calendar"
     :title "Calendar"
     :category "Work"
     :description "A desk calendar for scheduling, events and bookings."
     :tags ("schedule" "date" "event" "booking" "plan" "deadline" "appointment"))
    (:id "folder"
     :title "Folder"
     :category "Work"
     :description "A file folder for documents and projects."
     :tags ("files" "documents" "organise" "project" "archive"))
    (:id "mail"
     :title "Envelope"
     :category "Work"
     :description "A closed envelope for email, newsletters and contact."
     :tags ("email" "newsletter" "inbox" "message" "contact" "send"))
    (:id "clock"
     :title "Alarm Clock"
     :category "Work"
     :description "An alarm clock for time, deadlines and reminders."
     :tags ("time" "deadline" "schedule" "reminder" "productivity" "wake"))
    (:id "check"
     :title "Check Mark"
     :category "Work"
     :description "A big check mark for done, success and approval."
     :tags ("done" "complete" "success" "approved" "task" "verified" "yes"))
    (:id "trophy"
     :title "Trophy"
     :category "Work"
     :description "A trophy for wins, awards and achievements."
     :tags ("win" "award" "achievement" "success" "champion" "goal" "leaderboard"))
    (:id "search"
     :title "Magnifier"
     :category "Work"
     :description "A magnifying glass for search and discovery."
     :tags ("search" "find" "explore" "seo" "zoom" "discover" "research"))
    (:id "target"
     :title "Target"
     :category "Work"
     :description "A dartboard with an arrow for goals and focus."
     :tags ("goal" "aim" "focus" "marketing" "objective" "accuracy" "kpi"))
    (:id "bulb"
     :title "Light Bulb"
     :category "Work"
     :description "A light bulb for ideas, tips and creativity."
     :tags ("idea" "creative" "insight" "tip" "innovation" "think"))
    (:id "hourglass"
     :title "Hourglass"
     :category "Work"
     :description "An hourglass for time, waiting and countdowns."
     :tags ("time" "waiting" "loading" "countdown" "limited" "patience"))
    )

  "The icon library: 40 icons, each with an id, a title, a category, a
description of what it means, and the words it is found by. CC0 1.0: the
icons are free for any use, commercial included, with no credit.")

(defparameter *icon-license*
  '(:name "CC0 1.0 (public domain)"
    :summary "Free for any use, including commercial. No credit required."
    :url "https://creativecommons.org/publicdomain/zero/1.0/")
  "The licence of the icon files themselves, which the MIT licence of this
cell covers only as code.")
