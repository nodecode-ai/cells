;;;; presets.lisp --- the six built-in style contracts, as data.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A carrier of the original plugin's procgen_studio/presets/*.md: one entry a
;;;; preset, its prose rules in the order the original wrote them and its
;;;; palette, role by role, so a drawing written against one role keeps its
;;;; meaning when the palette is swapped.
(in-package #:nodecode-procgen-studio)

(defparameter *presets*
  '(
    ("brand-minimal"
     :description "minimal geometric brand marks with a single gradient"
     :renderer "python"
     :rules (
      ("outline" . "none, or one hairline highlight stroke (white at 20-25% opacity) on the outer mark")
      ("lighting" . "a soft radial halo behind the mark instead of blur")
      ("shading" . "one linear gradient accent -> liquid across the mark at 45 degrees, everything else flat")
      ("background" . "dark radial gradient bg -> bg2, centred slightly above the middle")
      ("shape_language" . "one strong geometric container (hexagon, circle, rounded square) with a simple glyph inside built from thick round-capped strokes; generous negative space; optical centring")
      ("typography" . "wordmark in a geometric sans (font stack \"Sora, Inter, DejaVu Sans, sans-serif\"), bold, tight tracking; tagline in a monospace (font stack \"JetBrains Mono, DejaVu Sans Mono, monospace\"), small, wide tracking, uppercase, muted colour")
      ("composition" . "mark in the upper half, wordmark below, tagline under it, everything centred"))
     :palette (
      ("bg" . "#1b1640")
      ("bg2" . "#07060f")
      ("ink" . "#0d0b22")
      ("base" . "#7c5cff")
      ("base2" . "#3a2a9a")
      ("metal" . "#f4f3ff")
      ("metal2" . "#8f8ac0")
      ("accent" . "#7c5cff")
      ("accent2" . "#4b3bc0")
      ("wood" . "#2a2650")
      ("wood2" . "#16132e")
      ("liquid" . "#22d3ee")
      ("liquid2" . "#0e7490")
      ("gem" . "#22d3ee")
      ("glow" . "#5b6cff")
      ("highlight" . "#ffffff")
      ("shadow" . "#03020a")
      ("sky" . "#1b1640")
      ("sky2" . "#07060f")
      ("ground" . "#0d0b22")
      ("foliage" . "#2a2650")
      ("light" . "#f4f3ff")))
    ("dark-fantasy-lowpoly"
     :description "low-poly faceted dark-fantasy characters with ember lighting"
     :renderer "python"
     :rules (
      ("outline" . "none on facets (facet stroke = facet fill to hide seams); thin ink edges only on claws and horns")
      ("lighting" . "strong top-left key light computed per facet (dot product of facet direction and light vector), hot under-glow from emissive cracks and eyes")
      ("shading" . "every body part is a polygon subdivided into jittered triangles, each shaded from base by its light term; bone and horn use a lighter material")
      ("background" . "deep radial vignette bg -> bg2 -> black, a large soft glow behind the head, drifting ember particles, a fog gradient over the lower quarter")
      ("shape_language" . "hulking asymmetric silhouette, small head, long arms, sharp horns and spines, readable at thumbnail size")
      ("composition" . "full-body hero shot, centred, feet on an elliptical ground shadow, optional title in a serif font along the bottom"))
     :palette (
      ("bg" . "#4a1420")
      ("bg2" . "#1a0810")
      ("ink" . "#1a1218")
      ("base" . "#463e4e")
      ("base2" . "#282030")
      ("metal" . "#8a8296")
      ("metal2" . "#4a4456")
      ("accent" . "#c9a27a")
      ("accent2" . "#7a5a40")
      ("wood" . "#c8baa0")
      ("wood2" . "#8a7a60")
      ("liquid" . "#ff6a2e")
      ("liquid2" . "#a01e10")
      ("gem" . "#ffb02e")
      ("glow" . "#ff3b2f")
      ("highlight" . "#ffe26a")
      ("shadow" . "#050206")
      ("sky" . "#2a0e18")
      ("sky2" . "#4a1420")
      ("ground" . "#1a0a10")
      ("foliage" . "#2a1a22")
      ("light" . "#ff7a3a")))
    ("ember"
     :description "warm cel-shaded game icons on dark rounded tiles"
     :renderer "python"
     :rules (
      ("outline" . "10px ink, round joins and caps, thinner 6-7px on inner details")
      ("lighting" . "top-left key light, soft warm rim on the lower-right edge")
      ("shading" . "two-tone cel plus one linear gradient per material, exactly one specular highlight per object")
      ("background" . "rounded square tile (rx about 12% of the tile), radial gradient bg -> bg2, faint inner bevel line, radial glow behind the subject, soft elliptical floor shadow")
      ("shape_language" . "chunky readable silhouettes, slightly exaggerated proportions, 45 degree diagonals for weapons")
      ("composition" . "one subject per tile, centred, about 70% of the tile, generous padding"))
     :palette (
      ("bg" . "#3a1c2e")
      ("bg2" . "#14091a")
      ("ink" . "#1a1020")
      ("base" . "#e0874a")
      ("base2" . "#7a3420")
      ("metal" . "#f4e6d0")
      ("metal2" . "#9a8a7a")
      ("accent" . "#ffd36b")
      ("accent2" . "#c0702a")
      ("wood" . "#a8643a")
      ("wood2" . "#5a2c18")
      ("liquid" . "#ff5a6e")
      ("liquid2" . "#9b1240")
      ("gem" . "#ff3d5a")
      ("glow" . "#ff9a4a")
      ("highlight" . "#fff4e0")
      ("shadow" . "#0b0710")
      ("sky" . "#2a0f1e")
      ("sky2" . "#6a2a32")
      ("ground" . "#1c0c14")
      ("foliage" . "#4a2a2a")
      ("light" . "#ffcf6e")))
    ("flat-illustration"
     :description "layered flat vector illustration with atmospheric depth"
     :renderer "python"
     :rules (
      ("outline" . "none; shapes are separated by value, not lines")
      ("lighting" . "one dominant light source (moon, sun, lamp) with a large radial-gradient glow; warm light against a cool scene")
      ("shading" . "flat fills per layer, depth by atmospheric perspective (far layers lighter and bluer, near layers darker), gradient fog bands between layers")
      ("background" . "vertical sky gradient sky -> sky2, sparse stars or particles scattered with the seeded RNG")
      ("shape_language" . "simple geometric silhouettes (triangular pines, rounded hills, boxy buildings), 3-5 parallax layers, one clear focal point placed on a rule-of-thirds intersection")
      ("composition" . "wide establishing shot, foreground silhouettes framing the edges, a path or line leading to the focal point"))
     :palette (
      ("bg" . "#2b3d7a")
      ("bg2" . "#0b1030")
      ("ink" . "#0a0f22")
      ("base" . "#2b3a6b")
      ("base2" . "#101731")
      ("metal" . "#c8d0f0")
      ("metal2" . "#6a74a8")
      ("accent" . "#ffb347")
      ("accent2" . "#c0702a")
      ("wood" . "#2a1b24")
      ("wood2" . "#1c1220")
      ("liquid" . "#6ac8ff")
      ("liquid2" . "#2a5a9a")
      ("gem" . "#ffcf6e")
      ("glow" . "#ffb347")
      ("highlight" . "#fffbe8")
      ("shadow" . "#050816")
      ("sky" . "#0b1030")
      ("sky2" . "#6a5a9a")
      ("ground" . "#172042")
      ("foliage" . "#1f2b55")
      ("light" . "#ffcf6e")))
    ("frost"
     :description "cool cel-shaded game icons on dark rounded tiles"
     :extends "ember"
     :rules (
      ("lighting" . "top-left key light, soft icy rim on the lower-right edge"))
     :palette (
      ("bg" . "#16304a")
      ("bg2" . "#070f1e")
      ("ink" . "#0b1424")
      ("base" . "#6fb6e8")
      ("base2" . "#24507a")
      ("metal" . "#e8f6ff")
      ("metal2" . "#7c98b4")
      ("accent" . "#9ff0ff")
      ("accent2" . "#2f86b8")
      ("wood" . "#5a6a9a")
      ("wood2" . "#2a2f58")
      ("liquid" . "#7affc8")
      ("liquid2" . "#0f7a74")
      ("gem" . "#56e0ff")
      ("glow" . "#8ad8ff")
      ("highlight" . "#f4fdff")
      ("shadow" . "#030812")
      ("sky" . "#0b1a33")
      ("sky2" . "#2c5a86")
      ("ground" . "#0a1424")
      ("foliage" . "#1d3a52")
      ("light" . "#d8f6ff")))
    ("pixel-arcade"
     :description "crisp 16-32px pixel sprites drawn as palette grids, scaled with nearest-neighbour"
     :renderer "pixel"
     :rules (
      ("outline" . "1px ink outline around every sprite silhouette, no anti-aliasing")
      ("lighting" . "light from the top-left; one highlight row on top surfaces, one shadow column on the right side")
      ("shading" . "at most 3 shades per material, no dithering unless asked, no gradients inside sprites")
      ("background" . "dark checkerboard of bg and bg2 in 32px cells")
      ("shape_language" . "chunky readable silhouettes, big head proportions for characters, symmetrical where it reads better")
      ("composition" . "sprite sheets show numbered frames in a row with a larger hero copy below"))
     :palette (
      ("bg" . "#1c1730")
      ("bg2" . "#16122a")
      ("ink" . "#1a1020")
      ("base" . "#c4cede")
      ("base2" . "#7884a0")
      ("metal" . "#eef2fa")
      ("metal2" . "#828caf")
      ("accent" . "#ffd25a")
      ("accent2" . "#be8228")
      ("wood" . "#6e4628")
      ("wood2" . "#46301c")
      ("liquid" . "#28c8f0")
      ("liquid2" . "#1a6a9a")
      ("gem" . "#ff4a6a")
      ("glow" . "#28c8f0")
      ("highlight" . "#ffffff")
      ("shadow" . "#0c0a16")
      ("sky" . "#2a3a6a")
      ("sky2" . "#4a6aa0")
      ("ground" . "#3a2a20")
      ("foliage" . "#3a8a4a")
      ("light" . "#f0c8a0"))))
  "The six built-in style presets, in the order the original listed them.")
