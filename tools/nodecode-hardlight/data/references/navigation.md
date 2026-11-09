# Navigation: make the next decision understandable

## Define the map in the visitor's vocabulary

List the main jobs, objects and destinations. For each destination record its URL, parent, entry links, page heading, local actions, completion/return destination and permission requirements. Separate **global scope** (workspace, collection, service), **local view** (list, calendar, chapter) and **object action** (edit, export, book). Two different scopes labeled “Settings” require visible context. Read labels without the surrounding artwork; replace “Discover”, “Manage” or “Learn more” when the destination remains ambiguous.

Do not encode the company's org chart unless it matches how visitors seek information. Test a label with a realistic question: “Where would I change the delivery address on this order?” not “Do you like this menu?” For important services, use observed task behavior and label/comprehension testing when available; explain when that evidence is absent.

## Choose a topology, not fashionable chrome

| Work shape | Candidate structure | Failure to prevent |
| --- | --- | --- |
| A few peer destinations | Labeled horizontal navigation; compact disclosure on narrow screens | Hiding the current place inside a closed menu |
| Many stable work areas | Grouped side navigation with current scope and local page header | Mixing workspaces, views and actions in one flat list |
| Long argument or documentation | Contents anchors plus parent/breadcrumb context when hierarchical | Scrollspy that changes the URL on every scroll or steals focus |
| Collection to object | Search/filter list with shareable detail routes | Back clearing filters, selection and scroll position |
| Required sequence | Steps with saved progress and clear previous/next | Letting a decorative stepper imply unavailable jumps |
| Flexible multi-session service | Task list with status and dependency explanations | Using a task list for work that must occur in a fixed order |
| Large heterogeneous catalog | Grouped disclosure/mega-navigation plus search | Bare icon grids, unlabeled panels or search as the only route |

Bottom navigation can suit a small stable set of high-frequency destinations, but it is not a universal mobile replacement for a website's hierarchy. Material rail guidance is platform/version-specific; choose based on destination count, width and task, not a copied breakpoint.

## Semantics and keyboard contract

Use `<nav>` with an accessible name when multiple navigation regions exist; use real anchors for destinations and buttons for actions. Mark current pages with `aria-current="page"` and a visible marker that is not color alone. Preserve normal link behaviors: copy URL, open new tab and browser history. An SPA route change updates the document title and moves focus to a sensible page heading/main region when appropriate; a local filter update should not reset focus to the top.

Ordinary site navigation is not an ARIA `menu` or `menubar`. The WAI APG disclosure example uses buttons and lists of links, not the application-menu interaction model. For a disclosure: name the button, expose `aria-expanded`, connect its controlled region, toggle on Enter/Space, allow Tab through visible links, and close on Escape with focus returned to the trigger. Hidden links cannot remain tabbable. Hover may supplement click/tap, not replace it. If a top-level item is both a destination and a parent, use a link plus a separate named disclosure button. Optional arrow keys must supplement, not replace, Tab behavior. Test your implementation; APG examples are illustrative, not a production-support guarantee.

A modal navigation sheet needs intentional focus entry, containment while modal, an obvious close control, Escape and focus return. A non-modal inline disclosure should not trap focus. Closing a sheet after navigation must not return focus to a now-irrelevant trigger and undo the new page's focus placement.

## State, deep links and recovery

Choose deliberately:
- Put meaningful shareable search, filter, sort, page and selected-object state in routes/query parameters when appropriate; do not serialize secrets or personal data into URLs.
- Use history entries for meaningful navigable changes; avoid a Back press for every keystroke. Debounce fetching independently from the visible input.
- Preserve filters and scroll on list/detail return. Test pasted detail URLs, refresh, Back, Forward and multiple tabs.
- Give zero-result searches a count, active constraints and a useful reset; distinguish them from a truly empty collection.
- Handle moved/deleted objects, forbidden routes, expired sessions and offline entry with explanation and a viable destination. Do not silently redirect a forbidden deep link to an unrelated home screen.
- At completion, say what happened and offer the next useful route; a dead end is not justified merely because it looks like a success page.

## Graph proof and its boundary

Call `hardlight_navigation` with pages `{id,label,links:[{to,label}]}`, tasks `{name,start,goal,max_steps?}` and an explicit home. Include the routes people actually see, including recovery routes. Resolve unknown targets and unreachable intended destinations. Review ambiguous labels and dead ends. Choose each optional path budget from that task's cost; there is no universal three-click rule. A shortest path is a count of graph edges, not evidence that a person can recognize or use the path. Check overlays, focus, permission and dynamic state in a real browser after the graph passes.

## Narrow-screen check

At a narrow width, identify current location without opening navigation; open it by keyboard, reach a child, close it and resume. Ensure the same core destinations exist as on desktop. Check long labels, zoom, landscape, software keyboard and fixed bars. WCAG 2.2 AA target-size minimum is 24 by 24 CSS pixels with specified exceptions; an original project may choose a more generous touch target, but do not misstate 44 pixels as the AA rule. Focus must not be entirely obscured under 2.4.11; prefer the stronger practical goal of keeping the whole indicator visible.
