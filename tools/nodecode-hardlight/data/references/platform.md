# Platform craft: use current capabilities for a concrete job

## Choose a support target

Start with the audience's browsers, embedded webviews, device constraints and existing stack. Baseline describes availability across a browser set; it is not a compatibility guarantee for every device, subfeature or assistive technology. State the target and keep the core task usable when an enhancement is absent. A feature detection only proves that an API or syntax is recognized, not that the entire interaction works.

This topic also returns `features` from the shipped, versioned capability ledger. Each entry names one capability, design job, source/check date, detection, fallback, constraints and verification. `established`, `enhancement` and `experimental` are adoption recommendations, not an automatic browser policy. Recheck the linked primary source for the actual target before consequential use. There is no live fetch, installed browser probe or background updater in the reference tool.

## Design the baseline first

Write ordinary HTML and a legible layout before adding capability-specific CSS. Use `@supports` for an optional styling layer and a guarded API call for optional behavior. Important content is present and visible immediately. The baseline should look intentional: a stacked workbench, an inline disclosure, a still illustration or an ordinary view change.

```js
function changeView(update) {
  const reduced = matchMedia('(prefers-reduced-motion: reduce)').matches;
  if (reduced || typeof document.startViewTransition !== 'function') {
    update();
    return;
  }
  const transition = document.startViewTransition(update);
  // A skipped animation is not a failed application update.
  transition.ready.catch(() => {});
}
```

Keep state, focus, URL/history and error handling in `update`; do not make task completion depend on the animation. The callback must still be robust if activation repeats. Catch or report actual update failures in the application's normal error path; suppressing `ready` rejection only handles a skipped visual transition. Exercise no-support and reduced-motion paths.

## Native controls before custom machinery

Use native buttons, links, forms, `details` and `dialog` where their behavior fits. An auto popover is non-modal: its existence does not trap focus or make the rest of the page inert. For a modal task use `dialog.showModal()` and explicitly plan its accessible name, initial focus, cancel behavior and return focus. Native primitives still need a tested interaction and appropriate semantics; a popover is not automatically an ARIA menu.

Anchor positioning can place supplementary UI beside a trigger, but detect the exact properties used and inspect edges, scrolling, zoom and RTL. Keep an inline or bounded-panel fallback. Newer text-box trimming is useful for chosen display specimens; test fallback fonts and scripts. Limited-support intrinsic-size interpolation and CSS `if()` belong in optional experiments, never in the essential path.

## A capability is not a visual identity

Do not add every ledger entry to one page. Choose the smallest subset that expresses the premise and improves the task. Record the exact detected capabilities alongside observed behavior, and include a run with enhancements disabled. Modern color authoring does not override the contrast tool's limitations: unsupported syntax and composite paint remain unknown.

Sources checked 2026-10-22: [choosing a Baseline target](https://web.dev/articles/how-to-choose-your-baseline-target), [same-document view transitions](https://developer.mozilla.org/en-US/docs/Web/API/Document/startViewTransition), [Popover API](https://developer.mozilla.org/en-US/docs/Web/API/Popover_API), [dialog](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dialog). The feature ledger contains capability-specific sources and fallback requirements.
