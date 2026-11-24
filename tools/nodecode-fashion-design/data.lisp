;;;; data.lisp --- the reference tables the ten verbs read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Data over code: sizes, grading increments, points of measure, the yardage
;;;; base per garment, the four seasonal palettes and the construction steps
;;;; are alists a layer can rebind, and every lookup is by a string key so a
;;;; garment the table does not name falls back to a stated default.

(in-package #:nodecode-fashion-design)

(defparameter +body-measurements+
  '(("women" ("XS" ("bust" 32) ("waist" 25) ("hip" 35) ("shoulder" 14) ("inseam" 30) ("height" 64))
     ("S" ("bust" 34) ("waist" 27) ("hip" 37) ("shoulder" 14.5) ("inseam" 30.5) ("height" 65))
     ("M" ("bust" 36) ("waist" 29) ("hip" 39) ("shoulder" 15) ("inseam" 31) ("height" 66))
     ("L" ("bust" 38) ("waist" 31) ("hip" 41) ("shoulder" 15.5) ("inseam" 31.5) ("height" 67))
     ("XL" ("bust" 40) ("waist" 33) ("hip" 43) ("shoulder" 16) ("inseam" 32) ("height" 68))
     ("XXL" ("bust" 42) ("waist" 35) ("hip" 45) ("shoulder" 16.5) ("inseam" 32.5) ("height" 69)))
    ("men" ("XS" ("chest" 34) ("waist" 28) ("hip" 34) ("shoulder" 16) ("inseam" 31) ("height" 67))
     ("S" ("chest" 36) ("waist" 30) ("hip" 36) ("shoulder" 17) ("inseam" 31.5) ("height" 68))
     ("M" ("chest" 38) ("waist" 32) ("hip" 38) ("shoulder" 17.5) ("inseam" 32) ("height" 69))
     ("L" ("chest" 40) ("waist" 34) ("hip" 40) ("shoulder" 18) ("inseam" 32.5) ("height" 70))
     ("XL" ("chest" 42) ("waist" 36) ("hip" 42) ("shoulder" 18.5) ("inseam" 33) ("height" 71))
     ("XXL" ("chest" 44) ("waist" 38) ("hip" 44) ("shoulder" 19) ("inseam" 33.5) ("height" 72))))
  "Body measurements in inches, per gender and size, per point of measure.")

(defparameter +grade-increments+
  '(("women" ("bust" . 2) ("waist" . 2) ("hip" . 2) ("shoulder" . 0.5) ("inseam" . 0.5))
    ("men" ("chest" . 2) ("waist" . 2) ("hip" . 2) ("shoulder" . 0.5) ("inseam" . 0.5)))
  "The step each circumference and length takes per size, in inches.")

(defparameter +points-by-garment+
  '(("t-shirt" "chest" "waist" "hip" "shoulder" "sleeve_length" "body_length")
    ("shirt" "chest" "waist" "hip" "shoulder" "sleeve_length" "body_length" "collar")
    ("dress" "bust" "waist" "hip" "shoulder" "dress_length" "sleeve_length")
    ("pants" "waist" "hip" "inseam" "outseam" "thigh" "knee" "hem")
    ("skirt" "waist" "hip" "skirt_length")
    ("jacket" "chest" "waist" "hip" "shoulder" "sleeve_length" "body_length")
    ("coat" "chest" "waist" "hip" "shoulder" "sleeve_length" "body_length"))
  "The points of measure a tech pack or a size sheet carries, per garment.")

(defparameter +yardage-base+
  '(("t-shirt" . 1.25) ("shirt" . 1.75) ("dress" . 2.5) ("pants" . 1.5)
    ("skirt" . 1.25) ("jacket" . 2.0) ("coat" . 3.0) ("blouse" . 1.5))
  "Yards of 45-inch fabric one garment of each type takes, before size.")

(defparameter +size-factor+
  '(("XS" . 0.9) ("S" . 0.95) ("M" . 1.0) ("L" . 1.05) ("XL" . 1.1) ("XXL" . 1.15))
  "How much more or less fabric a size takes than the base.")

(defparameter +color-palettes+
  '(("spring" ("Soft Petal" "#F7C5CC" "Pantone 13-1520 TPX")
     ("Sage Mist" "#A8C3A0" "Pantone 15-6316 TPX")
     ("Butter Cream" "#F5E6C8" "Pantone 12-0719 TPX")
     ("Sky Blue" "#A7D8F0" "Pantone 14-4318 TPX")
     ("Warm Coral" "#F08A7B" "Pantone 16-1546 TPX"))
    ("summer" ("Ocean Teal" "#2A9D8F" "Pantone 17-5126 TPX")
     ("Sunflower" "#F4A261" "Pantone 15-1157 TPX")
     ("Crisp White" "#FFFFFF" "Pantone 11-0601 TPX")
     ("Deep Navy" "#1D3557" "Pantone 19-4025 TPX")
     ("Bright Lime" "#B5E48C" "Pantone 13-0550 TPX"))
    ("fall" ("Rust" "#B7410E" "Pantone 18-1448 TPX")
     ("Olive" "#556B2F" "Pantone 18-0430 TPX")
     ("Camel" "#C19A6B" "Pantone 16-1334 TPX")
     ("Burgundy" "#800020" "Pantone 19-1629 TPX")
     ("Charcoal" "#36454F" "Pantone 19-4006 TPX"))
    ("winter" ("Icy Blue" "#DCE6F1" "Pantone 12-4005 TPX")
     ("Emerald" "#046307" "Pantone 18-5634 TPX")
     ("Plum" "#673147" "Pantone 19-2617 TPX")
     ("Silver" "#C0C0C0" "Pantone 14-5002 TPX")
     ("Black" "#000000" "Pantone 19-4008 TPX")))
  "One five-colour story per season: name, hex, Pantone reference.")

(defparameter +fabrics-by-season+
  '(("spring" "cotton poplin" "linen" "lightweight jersey")
    ("summer" "linen" "rayon" "cotton voile")
    ("fall" "wool blend" "twill" "corduroy")
    ("winter" "wool" "cashmere blend" "quilted nylon"))
  "Fabric suggestions per season; a season the table does not name gets cotton.")

(defparameter +silhouettes-by-price+
  '(("budget" . "relaxed, commercial silhouettes with broad appeal")
    ("mid" . "balanced silhouettes mixing classic and trend")
    ("premium" . "refined, tailored silhouettes with elevated details")
    ("luxury" . "sculptural, statement silhouettes with couture finishing"))
  "What each price tier asks of a silhouette.")

(defparameter +construction-steps+
  '(("t-shirt" "Cut fabric pieces: front, back, sleeves, neckband."
     "Sew shoulder seams (right sides together)."
     "Attach neckband: fold, quarter-mark, stretch to fit neckline, stitch."
     "Sew sleeves to armholes."
     "Sew side seams and underarm in one continuous seam."
     "Hem sleeves and bottom with a twin needle or coverstitch."
     "Press all seams.")
    ("dress" "Cut all pieces and mark notches and darts."
     "Sew darts on front and back bodice."
     "Join bodice front and back at the shoulders."
     "Insert zipper (center back or side)."
     "Attach skirt to bodice, matching notches."
     "Sew side seams."
     "Finish neckline with facing or bias binding."
     "Hem skirt."
     "Press.")
    ("pants" "Cut front and back leg pieces."
     "Sew front pockets."
     "Join front and back at side seams."
     "Sew inseam."
     "Sew crotch seam."
     "Attach waistband, insert elastic or button."
     "Hem legs."
     "Press.")
    ("jacket" "Cut all pieces including interfacing."
     "Fuse interfacing to collar, lapel and cuffs."
     "Sew darts and princess seams."
     "Join shoulders and side seams."
     "Set sleeves."
     "Attach collar and lapel."
     "Install lining."
     "Finish hems and buttonholes."
     "Press."))
  "Step-by-step construction per garment; anything else gets the generic order.")

(defparameter +generic-steps+
  '("Cut all pattern pieces."
    "Prepare and finish raw edges."
    "Assemble main seams."
    "Attach closures and trims."
    "Finish hems."
    "Press garment.")
  "The order a garment the table does not name is built in.")

(defparameter +bom-lines+
  '(;; item, description, quantity, unit, supplier, cost per unit in the section's currency
    ("Main fabric" "main fabric" 2.5 "yards" "Fabric supplier" 8.0)
    ("Lining" "poly lining" 1.0 "yards" "Fabric supplier" 3.0)
    ("Thread" "polyester thread" 1 "spool" "Trim supplier" 1.5)
    ("Trims" "standard trims" 1 "set" "Trim supplier" 2.0)
    ("Hardware" "standard hardware" 1 "set" "Hardware supplier" 1.5)
    ("Labels" "brand, size and care labels" 3 "pcs" "Label supplier" 0.5)
    ("Packaging" "polybag and hangtag" 1 "set" "Packaging supplier" 0.75))
  "The components a bill of materials lists: item, description, quantity, unit,
supplier and cost per unit. A caller may replace the main fabric, trims and
hardware descriptions; the rest are the standard quantities.")

(defparameter +collection-types+
  '("t-shirt" "shirt" "pants" "dress" "jacket" "skirt" "coat" "blouse")
  "The garments a planned collection is built from, in the order it fills.")

(defparameter +skill-tips+
  '(("beginner" . "Use a longer stitch length for basting, and press as you go.")
    ("intermediate" . "Use the needle and presser foot the fabric asks for.")
    ("advanced" . "Consider couture finishes and hand-stitched details."))
  "One tip per skill level; anything else gets the intermediate one.")
