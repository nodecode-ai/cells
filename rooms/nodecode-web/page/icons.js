// icons.js -- the page's marks, drawn like the ones index.html carries: a 15px
// box, a 1.5 stroke, round caps, in the text's own color. A mark stands where a
// word stood and the word stays its name and its tooltip, so `iconButton' takes
// the word as well as the mark.

const NS = "http://www.w3.org/2000/svg";

const SHAPES = {
  check: '<path d="M3 7.8l3 3 6-6.3"/>',
  chevron: '<path d="M5.5 3.5l4 4-4 4"/>',
  close: '<path d="M3.5 3.5l8 8M11.5 3.5l-8 8"/>',
  collapse: '<path d="M5 2.5L7.5 5 10 2.5M5 12.5L7.5 10 10 12.5"/>',
  copy: '<rect x="5" y="5" width="8" height="8" rx="1.5"/><path d="M10 5V3.5A1.5 1.5 0 0 0 8.5 2h-5A1.5 1.5 0 0 0 2 3.5v5A1.5 1.5 0 0 0 3.5 10H5"/>',
  download: '<path d="M7.5 2v7.5M4.5 6.5l3 3 3-3M2.5 12.5h10"/>',
  expand: '<path d="M5 5.5l2.5-2.5L10 5.5M5 9.5L7.5 12 10 9.5"/>',
  fork: '<circle cx="4.5" cy="3" r="1.5"/><circle cx="4.5" cy="12" r="1.5"/><circle cx="10.5" cy="4.5" r="1.5"/><path d="M4.5 4.5v6M10.5 6c0 3.5-6 2-6 4.5"/>',
  info: '<circle cx="7.5" cy="7.5" r="5.5"/><path d="M7.5 7v3.5"/><circle cx="7.5" cy="4.8" r=".8" fill="currentColor" stroke="none"/>',
  list: '<path d="M5.5 4h7M5.5 7.5h7M5.5 11h7"/><circle cx="2.8" cy="4" r=".8" fill="currentColor" stroke="none"/><circle cx="2.8" cy="7.5" r=".8" fill="currentColor" stroke="none"/><circle cx="2.8" cy="11" r=".8" fill="currentColor" stroke="none"/>',
  next: '<path d="M3.5 3.2v8.6l6.5-4.3z"/><path d="M12 3v9"/>',
  pause: '<path d="M5 3v9M10 3v9"/>',
  pencil: '<path d="M2.5 12.5l.6-2.6 7-7a1.4 1.4 0 0 1 2 2l-7 7z"/>',
  play: '<path d="M4.5 2.8v9.4l7.5-4.7z"/>',
  plus: '<path d="M7.5 2.5v10M2.5 7.5h10"/>',
  refresh: '<path d="M12.5 7.5a5 5 0 1 1-1.6-3.7"/><path d="M12.5 2.5v2.8H9.7"/>',
  rewind: '<path d="M4 5.5h5.5a3 3 0 0 1 0 6H6"/><path d="M6.5 3L4 5.5 6.5 8"/>',
  trash: '<path d="M2.5 4h10M6 4V2.5h3V4M3.8 4l.7 8.5h6l.7-8.5"/>',
  upload: '<path d="M7.5 9.5V2M4.5 5l3-3 3 3M2.5 12.5h10"/>',
};

// The mark NAME as an <svg>, SIZE px square, hidden from a screen reader: the
// control it stands in carries the word.
export function icon(name, size = 14) {
  const svg = document.createElementNS(NS, "svg");
  for (const [key, value] of Object.entries({
    viewBox: "0 0 15 15", width: size, height: size, fill: "none", stroke: "currentColor",
    "stroke-width": 1.5, "stroke-linecap": "round", "stroke-linejoin": "round", "aria-hidden": "true",
  })) svg.setAttribute(key, String(value));
  svg.innerHTML = SHAPES[name];
  return svg;
}

// A button that shows the mark NAME and says WORD: its accessible name and its
// tooltip. CLS is its class, "icon" (app.css) unless the caller's row needs another.
export function iconButton(name, word, cls = "icon") {
  const button = document.createElement("button");
  button.type = "button";
  button.className = cls;
  button.setAttribute("aria-label", word);
  button.title = word;
  button.append(icon(name));
  return button;
}
