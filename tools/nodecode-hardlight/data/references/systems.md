# Systems: extract the interface that actually shipped

## Inspect before inventing

Read representative screens, existing CSS variables, components, assets and routes. Use `hardlight_inspect` for bounded local inventory and source-line advisories, then read the relevant files yourself. The inspector does not execute the application, understand all JSX semantics or certify a complete design system. Skipped paths, unsupported files and exhausted budgets are coverage limits, not clean passes.

An extension inherits the established system unless an explicit conflict requires a documented exception. A redesign preserves business and interaction truth while replacing the chosen visual layer; do not quietly combine incompatible old and new grammars. Do not create a token factory before the first concrete screen teaches you which decisions recur.

## A small token architecture

Separate levels when they are useful:
- **Primitive values:** available color or size values, with no promise of semantic use.
- **Semantic roles:** canvas, text, action, focus, selection, error, spacing within a group and spacing between chapters.
- **Component decisions:** toolbar gap, field radius or dialog elevation only when a repeated component actually needs them.

For each shipped semantic token record value, purpose, states/themes and a usage example from the implementation. Avoid names such as `gray-500` as the only clue to a semantic job, or `premium-shadow` without a behavioral purpose. Radius and elevation should express region/interaction distinctions rather than creating a separate floating panel around every element.

Type roles need family/fallback, weight, size, line height, measure and wrapping guidance. Spacing needs a few intentional intervals for local grouping, peer separation and chapter changes; multiplying one spacing step everywhere does not create rhythm. Layout rules need maximum readable width, density choices and actual topology changes, not just breakpoint numbers.

## Components include behavior

Document a component's semantic element, accessible name, required content, variants, state transitions, keyboard policy, focus return, disabled/loading behavior and error recovery. A screenshot of three buttons is not a button system. Keep route navigation distinct from command actions even when visual styles overlap.

Write patterns where combinations recur: collection/filter/detail return; multi-field validation; permission-limited deep link; save/undo/conflict; mobile navigation sheet. Reference the real implementation and test path. Do not abstract a one-off illustration into a configurable framework merely because it might be reused someday.

## Assets are part of the system

Record original/licensed source, usage rights, crop rules, alternative text intent, dimensions, loading priority and fallback. Define an illustration or photography grammar with subject, lighting/stroke, palette and acceptable variation. Preserve identity across empty, error and mobile states without requiring a new illustration for every message.

Keep third-party research screenshots and branded reference assets out of distributable examples unless rights explicitly permit inclusion. Link to sources and describe the transferable principle instead. Do not mistake studying a public website for permission to reuse its copy, code, images or logo.

## Complete the handoff

Extract only after inspecting the working wide, narrow and engaged states. Record the chosen thesis, navigation rationale, semantic tokens, typography roles, material/motion rules, asset plan and exceptions. Include evidence status: implemented, exercised, visually inspected or still unknown. Add the exact reproduction steps for important states.

A system is useful when a future contributor can add a real screen that belongs to it, retains task behavior and passes the same checks. It is not useful merely because all possible future options have been parameterized. Remove unused tokens and inaccurate pre-build promises before release.
