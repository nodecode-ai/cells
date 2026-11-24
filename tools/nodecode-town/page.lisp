;;;; page.lisp --- the page /town/ serves, and the routes around it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One self-contained page: no build step, no external script, no font, no
;;;; image, nothing fetched but the snapshot route on the port it came from.
;;;; It draws the town -- a building per place, a resident per session the
;;;; snapshot names by pseudonym -- and polls /api/town/snapshot every two
;;;; seconds, so a walk across the square follows a real tool call.
;;;;
;;;; The page reads only what SNAPSHOT publishes: a resident key, a role, a
;;;; place, a tool name. No session id, no prompt, no tool argument, no tool
;;;; result is representable in the route's answer, and the page carries no
;;;; token because it needs none.

(in-package #:nodecode-town)

(defparameter +snapshot-route+ "/api/town/snapshot"
  "The one thing the page fetches, on the gateway's own port.")

(defparameter +page+
  (concatenate
   'string
   "<!doctype html>
<html lang='en'>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width, initial-scale=1'>
<title>Nodecode town</title>
<style>
  html,body{margin:0;height:100%;background:#101820;color:#cfe3e8;font:13px/1.45 ui-monospace,Menlo,Consolas,monospace}
  #town{display:block;width:100%;height:calc(100% - 132px)}
  #hud{position:fixed;left:0;right:0;bottom:0;height:132px;overflow:auto;border-top:1px solid #23414b;padding:8px 12px;box-sizing:border-box}
  #status{color:#7fd1c0}
  ul{margin:6px 0 0;padding:0}
  li{list-style:none;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
  .k{color:#efc47b}
  .q{color:#7f9aa3}
</style>
</head>
<body>
<canvas id='town'></canvas>
<div id='hud'><div id='status'>starting</div><ul id='events'></ul></div>
<script>
var RING = [
  ['library','Library',-152,'#3f6b8f'],
  ['workshop','Workshop',-104,'#8a6b4a'],
  ['forge','Forge',-52,'#8f4a4a'],
  ['observatory','Observatory',8,'#3f8f7f'],
  ['post','Post office',58,'#6a5a8f'],
  ['hall','Town hall',118,'#5f7a3f'],
  ['market','Market',166,'#8f8f4a']
];
var COAT = {coordinator:'#7fd1c0', child:'#efc47b'};
var canvas = document.getElementById('town');
var ctx = canvas.getContext('2d');
var statusLine = document.getElementById('status');
var eventList = document.getElementById('events');
var snapshotUrl = 'SNAPSHOT';
var places = {};
var residents = [];
var events = [];
var at = {};
var failures = 0;

function place(name) {
  return places[name] || places.plaza;
}

function layOut() {
  var w = canvas.width, h = canvas.height;
  var cx = w / 2, cy = h / 2;
  var rx = Math.max(120, w * 0.36), ry = Math.max(80, h * 0.34);
  places = { plaza: { x: cx, y: cy, label: 'Plaza', colour: '#1c2f39' } };
  for (var i = 0; i < RING.length; i++) {
    var row = RING[i];
    var rad = row[2] * Math.PI / 180;
    places[row[0]] = {
      x: cx + Math.cos(rad) * rx,
      y: cy + Math.sin(rad) * ry,
      label: row[1],
      colour: row[3]
    };
  }
  places.tavern = {
    x: (places.hall.x + places.forge.x) / 2,
    y: (places.hall.y + places.forge.y) / 2 + 10,
    label: 'Tavern',
    colour: '#7a5a3a'
  };
}

function home(resident, index, count) {
  var p = place(resident.place);
  if (count < 2) return { x: p.x, y: p.y + 18 };
  var spread = Math.min(34, 8 * count);
  var offset = (index / (count - 1) - 0.5) * spread * 2;
  return { x: p.x + offset, y: p.y + 18 };
}

function drawBuilding(p) {
  ctx.fillStyle = p.colour;
  ctx.fillRect(p.x - 26, p.y - 30, 52, 34);
  ctx.fillStyle = '#0b141a';
  ctx.fillRect(p.x - 6, p.y - 14, 12, 18);
  ctx.fillStyle = '#cfe3e8';
  ctx.font = '11px ui-monospace,Menlo,Consolas,monospace';
  ctx.textAlign = 'center';
  ctx.fillText(p.label, p.x, p.y + 22);
}

function drawResident(r, spot) {
  var here = at[r.key];
  if (!here) { here = { x: place('plaza').x, y: place('plaza').y }; at[r.key] = here; }
  here.x += (spot.x - here.x) * 0.08;
  here.y += (spot.y - here.y) * 0.08;
  ctx.fillStyle = COAT[r.role] || '#9ab';
  ctx.fillRect(here.x - 4, here.y - 6, 8, 12);
  ctx.fillStyle = '#f0e2c8';
  ctx.fillRect(here.x - 3, here.y - 12, 6, 6);
  ctx.fillStyle = '#7f9aa3';
  ctx.font = '10px ui-monospace,Menlo,Consolas,monospace';
  ctx.textAlign = 'center';
  ctx.fillText(String(r.key).slice(-4), here.x, here.y + 20);
}

function draw() {
  ctx.fillStyle = '#101820';
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  var names = Object.keys(places);
  for (var i = 0; i < names.length; i++) drawBuilding(places[names[i]]);
  var byPlace = {};
  for (var j = 0; j < residents.length; j++) {
    var r = residents[j];
    if (!byPlace[r.place]) byPlace[r.place] = [];
    byPlace[r.place].push(r);
  }
  var keys = Object.keys(byPlace);
  for (var k = 0; k < keys.length; k++) {
    var group = byPlace[keys[k]];
    for (var m = 0; m < group.length; m++) drawResident(group[m], home(group[m], m, group.length));
  }
  requestAnimationFrame(draw);
}

function render(snap) {
  residents = snap.residents || [];
  events = snap.events || [];
  var live = 0;
  for (var i = 0; i < residents.length; i++) if (residents[i].state !== 'idle') live++;
  statusLine.textContent = residents.length + ' residents, ' + live + ' at work, '
    + (snap.received || 0) + ' events filed, ' + (snap.dropped || 0) + ' dropped';
  var lines = [];
  for (var j = 0; j < Math.min(8, events.length); j++) {
    var e = events[j];
    lines.push('<li><span class=\"q\">' + (e.kind || '') + '</span> <span class=\"k\">'
      + String(e.key || '').slice(-4) + '</span> ' + (e.place || '') + ' '
      + (e.tool || '') + ' ' + (e.outcome || '') + '</li>');
  }
  eventList.innerHTML = lines.join('');
}

function poll() {
  fetch(snapshotUrl, { cache: 'no-store' })
    .then(function (r) { return r.json(); })
    .then(function (snap) { failures = 0; render(snap); })
    .catch(function () {
      failures++;
      statusLine.textContent = 'the town did not answer (' + failures + '); is the cell still on?';
    });
}

function resize() {
  var box = canvas.getBoundingClientRect();
  canvas.width = Math.max(320, Math.floor(box.width));
  canvas.height = Math.max(240, Math.floor(box.height));
  layOut();
}

window.addEventListener('resize', resize);
resize();
requestAnimationFrame(draw);
poll();
setInterval(poll, 2000);
</script>
</body>
</html>
")
  "The town page: drawn on a canvas from the snapshot, with no asset of its own.")

(defun substitute-page-vars (page)
  "PAGE with its one placeholder replaced by the snapshot route."
  (let ((at (search "SNAPSHOT" page)))
    (if at
        (concatenate 'string (subseq page 0 at) +snapshot-route+
                     (subseq page (+ at (length "SNAPSHOT"))))
        page)))

(defun serve-page (env)
  "GET /town/: the page. Anything else is refused, and no cache holds it."
  (if (member (getf env :request-method) '(:get :head) :test #'equal)
      (list 200
            (list :content-type "text/html; charset=utf-8"
                  :cache-control "no-store"
                  :x-content-type-options "nosniff"
                  :referrer-policy "no-referrer"
                  :content-security-policy
                  "default-src 'none'; script-src 'unsafe-inline'; style-src 'unsafe-inline'; connect-src 'self'; base-uri 'none'")
            (list (substitute-page-vars +page+)))
      (list 405 (list :content-type "text/plain; charset=utf-8")
                (list "method not allowed"))))

(defun redirect-to-page (env)
  "GET /town: the page is a directory; send the browser to it."
  (declare (ignore env))
  (list 302 (list :location "/town/") '()))
