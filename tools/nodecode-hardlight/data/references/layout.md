# Layout: adapt the composition to the space and the task

## Choose what changes, not just a breakpoint

Write the wide-to-narrow transformation before styling. A workbench becomes a list followed by an addressable inspector; a reading rail moves before the article; an illustration changes crop while its caption stays with it. Preserve destinations, meaningful sequence and recovery. Do not shrink a desktop canvas or hide essential actions to fit a phone. Viewport media queries describe page topology; container queries describe a component's available space.

Start with intrinsic constraints: `minmax(0, 1fr)`, `min-inline-size: 0`, flexible gaps and readable maximum measures. Long names, translated labels and large default text are ordinary inputs. Apply `overflow-wrap: anywhere` to unbroken user text when breaking it is preferable to overflow; do not truncate essential errors, amounts or destinations. Use a semantic table and a labeled scrolling region when genuine two-dimensional data needs both axes.

## A component can be narrower than its viewport

The baseline flow below works without container queries. The enhancement separates list and detail only when their containing region has room. The container is an ancestor of the element being changed.

```css
.desk-region { container-type: inline-size; }
.desk { display: grid; gap: 1.5rem; }
.desk > * { min-inline-size: 0; }
@container (min-width: 52rem) {
  .desk { grid-template-columns: minmax(16rem, .85fr) minmax(0, 1.15fr); }
}
```

This is a starting recipe, not a universal 52rem breakpoint. Derive the threshold from actual controls and reading measure. Containment changes intrinsic sizing, so do not add it indiscriminately. For aligned metadata, subgrid can share row tracks between unequal label/value pairs; keep independent grids as the fallback. Layout does not replace semantic relationships: use `dl` for labeled facts and a table for comparable rows and columns.

## Keep the same interface across input and writing modes

Prefer logical properties (`padding-inline`, `border-inline-start`, `max-inline-size`) for content relationships. Test `dir="rtl"` with real labels rather than simply mirroring an illustration. Source order should remain the reading and keyboard order; `order` and grid placement must not make focus jump unexpectedly.

Use `:focus-visible` for an unmistakable keyboard indicator. Ensure fixed bars do not cover focused controls or anchor destinations. Touch must not depend on hover, precise dragging or a custom cursor. Generous controls are a design choice; WCAG 2.2's 24 CSS pixel target criterion has spacing, inline and other exceptions. Check the specific use before declaring failure. Do not disable user zoom.

## Verify the transformation

Check 320 CSS pixel reflow, a representative phone, an intermediate width and a wide view, then resize the component inside a wide page. Exercise the actual selection or next step at each topology. Enlarge text, use difficult content and switch relevant writing directions. Record what was observed, the baseline fallback and any genuinely two-dimensional exceptions. A screenshot at 390 pixels alone does not establish reflow or usable text resizing.

Sources checked 2026-10-22: [MDN container queries](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_containment/Container_queries), [subgrid](https://developer.mozilla.org/en-US/docs/Web/CSS/Guides/Grid_layout/Subgrid), [W3C reflow](https://www.w3.org/WAI/WCAG22/Understanding/reflow.html), [target size](https://www.w3.org/WAI/WCAG22/Understanding/target-size-minimum.html). W3C Understanding documents explain criteria; partial checks do not establish conformance.
