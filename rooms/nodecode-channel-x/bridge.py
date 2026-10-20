"""bridge.py -- the X channel's one door to X: a logged-in web session (tweetkit-x).

SPDX-License-Identifier: MIT

Run by the nodecode-channel-x cell as a long-lived child:

    uvx --from tweetkit-x==0.3.0 python bridge.py

One JSON object per line on stdin, one answer per line on stdout, in order.
The first request must be {"op": "login", "cookie": "..."}: the cookie never
rides argv or the environment, and no answer ever carries it. A session is
kept for the child's life, so X's per-request transaction id is derived from
one page load, as a browser tab would.

Ops:
  login    {cookie}                       -> {user_id}
  me       {}                             -> {user_id}
  user_id  {handle}                       -> {user_id}
  mentions {since, limit}                 -> {tweets: [...] oldest first, newer than since}
  tweet    {id}                           -> {tweet}
  reply    {text, reply_to, image?}       -> {id, url}
  delete   {id}                           -> {}

Every answer is {"ok": true, ...} or {"ok": false, "error": TEXT, "status": N}.
There is deliberately no op that posts a tweet which is not a reply, and none
that sends a direct message.
"""

import json
import sys
import traceback

import requests
from bs4 import BeautifulSoup
from tweetkit_x import constants as C
from tweetkit_x.client import TweetKit
from x_client_transaction import ClientTransaction
from x_client_transaction.utils import get_ondemand_file_url

TK = None


class BridgeTweetKit(TweetKit):
    """TweetKit with the transaction id fetched from x.com/home, as a browser tab does."""

    def _transaction(self):
        # Upstream 0.3.0 derives the id from handle_x_migration's answer, which
        # no longer carries the on-demand script (get_ondemand_file_url finds
        # nothing and raises). Fetching the logged-in home page directly does.
        if self._ct is None:
            err = None
            for extra in ({"cookie": self.cookie}, {}):
                try:
                    home = self._session.get("https://x.com/home", headers=extra)
                    page = BeautifulSoup(home.text, "html.parser")
                    ondemand = requests.get(get_ondemand_file_url(page),
                                            headers=dict(self._session.headers), timeout=self.timeout)
                    self._ct = ClientTransaction(page, BeautifulSoup(ondemand.text, "html.parser"))
                    break
                except Exception as e:  # the next variant, then the error
                    err = e
            if self._ct is None:
                raise err
        return self._ct


def _users_and_tweets(node, users, tweets):
    """Collect every User (id -> handle, name) and every tweet's legacy object."""
    if isinstance(node, dict):
        if node.get("__typename") == "User" and node.get("rest_id"):
            core = node.get("core") if isinstance(node.get("core"), dict) else {}
            legacy = node.get("legacy") if isinstance(node.get("legacy"), dict) else {}
            handle = core.get("screen_name") or legacy.get("screen_name")
            name = core.get("name") or legacy.get("name")
            if handle:
                users[node["rest_id"]] = {"handle": handle, "name": name or handle}
        lg = node.get("legacy")
        if isinstance(lg, dict) and "full_text" in lg:
            tid = lg.get("id_str") or node.get("rest_id")
            if tid:
                tweets[tid] = {
                    "id": tid,
                    "text": lg.get("full_text", ""),
                    "created_at": lg.get("created_at", ""),
                    "author_id": lg.get("user_id_str"),
                    "in_reply_to": lg.get("in_reply_to_status_id_str"),
                    "conversation_id": lg.get("conversation_id_str") or tid,
                    "retweet": "retweeted_status_result" in lg,
                }
        for value in node.values():
            _users_and_tweets(value, users, tweets)
    elif isinstance(node, list):
        for value in node:
            _users_and_tweets(value, users, tweets)


def _with_authors(tweets, users):
    out = []
    for t in tweets.values():
        who = users.get(t["author_id"]) or {}
        out.append({**t, "author": who.get("handle"), "author_name": who.get("name")})
    return out


def _notifications(timeline_type, limit):
    """One page of the notifications timeline, raw tweets and their authors."""
    op = "NotificationsTimeline"
    qid = C.QUERY_IDS[op]
    path = f"/i/api/graphql/{qid}/{op}"
    variables = {"count": max(1, min(int(limit), 40)), "timeline_type": timeline_type}
    params = {"variables": json.dumps(variables),
              "features": json.dumps(C.USER_TWEETS_FEATURES)}
    r = TK._session.get(f"{C.GQL_BASE}/{qid}/{op}", params=params,
                        headers=TK._headers("GET", path), timeout=TK.timeout)
    if r.status_code != 200:
        raise BridgeError(f"{op} answered HTTP {r.status_code}: {r.text[:200]}", r.status_code)
    users, tweets = {}, {}
    _users_and_tweets(r.json(), users, tweets)
    return _with_authors(tweets, users)


class BridgeError(Exception):
    def __init__(self, text, status=0):
        super().__init__(text)
        self.status = status


def op_login(req):
    global TK
    TK = BridgeTweetKit(cookie=req["cookie"])
    who = TK.whoami()
    if not who.get("ready") or not who.get("user_id"):
        raise BridgeError("X did not take the session cookie (signed out, or missing ct0/auth_token)", 401)
    return {"user_id": str(who["user_id"])}


def op_me(req):
    return {"user_id": str(TK.whoami().get("user_id") or "")}


def op_user_id(req):
    return {"user_id": str(TK.user_id_by_name(req["handle"]))}


def op_mentions(req):
    since = int(req.get("since") or 0)
    tweets = [t for t in _notifications("Mentions", req.get("limit", 40))
              if int(t["id"]) > since and not t["retweet"]]
    tweets.sort(key=lambda t: int(t["id"]))
    return {"tweets": tweets}


def op_tweet(req):
    t = TK.get_tweet(str(req["id"]))
    if not t.get("ok"):
        raise BridgeError(t.get("error") or "tweet not found", t.get("status") or 404)
    return {"tweet": t}


def op_reply(req):
    reply_to = str(req.get("reply_to") or "")
    if not reply_to:
        raise BridgeError("the X channel posts replies only: reply_to is required", 400)
    r = TK.post(req["text"], image_path=req.get("image") or None, reply_to=reply_to)
    if not r.get("ok"):
        raise BridgeError(str(r.get("error") or "the post was refused"), r.get("status") or 0)
    return {"id": r["id"], "url": r.get("url")}


def op_delete(req):
    r = TK.delete(str(req["id"]))
    if isinstance(r, dict) and r.get("ok") is False:
        raise BridgeError(str(r.get("error") or "the delete was refused"), r.get("status") or 0)
    return {}


OPS = {"login": op_login, "me": op_me, "user_id": op_user_id, "mentions": op_mentions,
       "tweet": op_tweet, "reply": op_reply, "delete": op_delete}


READ_OPS = {"me", "user_id", "mentions", "tweet"}


def answer(req):
    op = req.get("op")
    if op not in OPS:
        return {"ok": False, "status": 400, "error": f"no op {op!r}; one of {sorted(OPS)}"}
    if op != "login" and TK is None:
        return {"ok": False, "status": 401, "error": "not logged in: the first request is login"}
    try:
        try:
            return {"ok": True, **OPS[op](req)}
        except BridgeError:
            raise
        except Exception:
            # A read that failed on the web client's transaction id (x.com's
            # page shell sometimes arrives without the script it is derived
            # from) is retried once with the id fetched again. A post is
            # never retried: it may have landed before the error.
            if op not in READ_OPS:
                raise
            TK._ct = None
            return {"ok": True, **OPS[op](req)}
    except BridgeError as e:
        return {"ok": False, "status": e.status, "error": str(e)}
    except Exception as e:  # the child stays up; the cell reads why
        text = f"{type(e).__name__}: {e}"
        if "cookie" in req:
            text = text.replace(req["cookie"], "<cookie>")
        traceback.print_exc(file=sys.stderr)
        return {"ok": False, "status": 0, "error": text[:400]}


def main():
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            req = json.loads(line)
        except ValueError as e:
            out = {"ok": False, "status": 400, "error": f"not JSON: {e}"}
        else:
            out = answer(req)
        sys.stdout.write(json.dumps(out) + "\n")
        sys.stdout.flush()


if __name__ == "__main__":
    main()
