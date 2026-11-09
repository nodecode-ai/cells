# Color: assign meaning, then verify the painted result

## Choose a strategy tied to the subject

Decide where color does its main work: restrained accent for a busy workspace, one committed field for a campaign, several named categories for a collection, or a material/image-led palette. None is universally better. Purple, green, monochrome, gradients and saturated surfaces are legitimate choices when they belong to the subject; none should be selected automatically. The atlas palettes are opening swatches, not certified accessible token sets.

Build a compact role ledger: canvas, surface, foreground, secondary foreground, border, primary action, action text, focus, selection, link, error, warning, success and information. Add data-series colors only when the content requires them. Define each role in hover, focus, selected, disabled and dark/high-contrast contexts that the product supports. Do not derive secondary text by reducing opacity without checking the actual composite.

## Make roles distinguishable

One hue may serve brand and action, but a decorative colored region should not masquerade as a button. A selected state needs shape, marker or explicit text as well as color. Errors need an intelligible message and field association; charts need direct labels, patterns or an accessible data equivalent. Test neighboring series and status colors in context, including common color-vision differences. A palette viewer alone cannot establish distinguishability.

For a colored canvas, compare foreground relationships on that exact canvas rather than importing neutral grays from another system. Use perceptual color spaces to explore relationships if helpful; final accessibility checks still concern the rendered foreground/background pair, not an abstract lightness coordinate. Themes are separate systems to test, not numerical inversions of every token.

## Contrast checks and their limits

For WCAG 2.2, ordinary text generally needs 4.5:1 and large text 3:1 under 1.4.3, with its defined exceptions. “Large” has a specific size/weight meaning: at least 18pt regular or 14pt bold, not simply a class named `large`. Relevant non-text controls and visual state indicators have 3:1 requirements under 1.4.11, with exceptions; not every decorative border needs that ratio. Focus visibility, focus not obscured and contrast are related but distinct checks. Do not claim the enhanced AAA focus appearance criterion is the AA minimum.

Call `hardlight_contrast` with explicit `{foreground,background,role,label}` pairs. Supported roles are `text`, `large_text` and `ui`; supported colors are opaque `#RGB` or `#RRGGBB`. The tool compares unrounded ratios to thresholds. A rounded display of 4.50 cannot justify a pass when the underlying result is lower. The role you select is an assumption that must match actual use.

Transparency, backdrop blur, gradients, photographs, blend modes and canvas/WebGL need analysis of the actual composite and the worst relevant painted region. An unsupported syntax or unknown background is **unknown**, not passing. Move required text to an opaque plane, add a reliable scrim or inspect representative composites; do not approximate an effect into a passing hex pair and call the original checked.

## Practical pass

Modern authoring can use OKLCH for perceptual relationships and `color-mix()` for controlled variations. Keep explicit semantic roles and useful sRGB fallbacks. Wrap custom-property overrides in a feature query: custom properties accept raw values even when the eventual color syntax is unsupported.

```css
:root { --canvas: #f4f6f8; --ink: #172b3e; }
@supports (color: oklch(97% 0.005 240)) {
  :root { --canvas: oklch(97% 0.005 240); }
}
```

These are illustrative values, not certified equivalent or accessible pairs. Check each actual supported light/dark role and its hover, focus and selected state; do not invert tokens to manufacture a dark theme. Perceptual lightness, `color-mix()` and automatic contrast color selection do not establish WCAG compliance. The current pure contrast tool accepts opaque hexadecimal sRGB, and the browser helper conservatively recognizes simple sRGB text paint only. OKLCH or unresolved mixes must remain unknown unless a separately validated conversion and actual paint analysis are available.

Sources checked 2026-10-22: [MDN OKLCH](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/color_value/oklch), [color-mix](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/Values/color_value/color-mix).

1. Inventory rendered text/action/status roles, including disabled and selected states.
2. Check known opaque pairs through the deterministic tool; retain the exact role and threshold with each result.
3. Inspect image/effect backgrounds, focus indicators, charts and overlays in the browser. Mark uninspected backgrounds explicitly.
4. Test forced-colors/high-contrast mode when relevant; ensure borders, icons and selection remain understandable when fills change.
5. Recheck narrow layouts, where a label can move onto a different background, and hover/focus combinations, where colors can change.

**Deliverable:** a role table and check results with known limitations, not a claim that “the palette passes WCAG.” Accessibility applies to use, content and behavior, not isolated swatches.
