# States: design the operation, not just its ideal frame

## Build a state ledger for the actual task

For each operation record: trigger, preconditions, pending behavior, result, failure, recovery, focus destination, announcement and URL effect. Mark inapplicable states with a reason; do not invent fake functionality to populate a checklist. Cover at least the main form, list/detail path, navigation overlay and any asynchronous mutation.

| Condition | Show and enable | Verify |
| --- | --- | --- |
| Initial/no script | Meaningful content and real links; honest enhancement boundary | Disable JavaScript, read the page and reach core content |
| First-use empty | Explain what this area contains and a feasible first action | A new account/collection has no fictional history |
| Filtered empty | Result count, active constraints, clear/reset or broader search | Reset changes the actual results without erasing unrelated state |
| Loading | Keep context and stable dimensions; label the pending operation | Slow the request; avoid layout collapse and repeated announcements |
| Background refresh | Retain usable prior data, identify updating or stale values | A failed refresh does not masquerade as fresh data |
| Validation failure | Field-level message, retained input and a linked summary for multi-field forms | Keyboard reaches each invalid field from the summary |
| Transport/server error | Name the failed operation and a safe retry or alternate route | Repeated retry cannot duplicate an irreversible mutation |
| Success | Confirm the real result and its destination; show undo when meaningful | Read back the persisted result, not merely a successful animation |
| Disabled/unavailable | Explain missing prerequisite or unavailability near the action | A nonfocusable disabled button is not the only place containing the explanation |
| Permission-limited | State what is unavailable and a legitimate request/access route | Direct URL and expired permission do not leak private content |
| Offline | Say what remains available and whether changes are queued | Reconnect handling makes conflicts and unsaved changes visible |
| Deleted/moved/stale link | Explain the missing object and provide a relevant return/search | Do not replace every failure with an unrelated homepage |

## Forms and mutation rules

Place a persistent label and any needed format/help before the input. Use placeholders only as examples, never as the label. Validate at a useful point without scolding incomplete typing; client-side validation supplements server validation. An error says what is wrong and how to fix it. Keep entered values unless retaining a sensitive value would be unsafe. GOV.UK's error summary is a useful pattern: a clear heading and links that lead to the relevant fields, consistent with the field messages.

A pending submit may prevent accidental repeated submission while retaining its label and visible context. Do not claim “Saved” before persistence succeeds. For optimistic updates, define rollback, failure notification and conflict handling before choosing the animation. Destructive actions need an appropriate confirmation, reversible undo or another safeguard based on consequences; a confirmation dialog is not automatically required for every reversible deletion.

## Focus and announcement policy

- Keep focus on the controlling element for routine sort, filter and disclosure changes unless the task calls for movement.
- On failed full-form submission, focus a useful summary or first error according to the form's structure. Link summary entries to the actual fields.
- Announce asynchronous completion or result counts through a restrained status region when they would otherwise be missed. Do not mark the entire changing workspace as a live region.
- On dialog close, return focus to its invoking control when it still exists. After deleting that control's object, move to the next sensible item or list heading.
- Loading, selected and disabled visuals cannot rely on reduced opacity alone; test contrast and explicit status.

## Stress content before polishing

Exercise long names, a one-word title, absent images, zero and many results, large/negative numbers where valid, multiline validation, translated expansion, mixed scripts and RTL when supported. Check 200% text zoom and 400% page zoom/reflow contexts. Render one error and one permission/offline case at narrow width so recovery does not become an inaccessible afterthought.

**Handoff:** a state ledger with implemented, exercised and unavailable statuses, plus the exact steps that reproduce each material state. A static mockup showing an error is evidence of a design decision, not proof of real recovery behavior.
