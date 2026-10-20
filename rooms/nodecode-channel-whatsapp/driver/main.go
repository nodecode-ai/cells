// Command whatsapp-driver is the Nodecode WhatsApp channel's transport: one
// linked device (go.mau.fi/whatsmeow), spoken to over stdin and stdout, one
// JSON object a line.
//
// SPDX-License-Identifier: MIT
//
// The Lisp cell (../adapter.lisp) owns every platform decision: who may talk,
// what a line means, where an answer goes. This program owns the WhatsApp Web
// session and nothing else: the linked-device keys (a SQLite file the cell
// names), the socket, the end-to-end encryption, media upload and download,
// and a small archive of the messages it has seen so a quote, a reaction or a
// read of history can name a message by its id alone.
//
// In:  {"id":"7","op":"send","args":{...}}
// Out: {"id":"7","ok":true,"result":{...}}  or  {"id":"7","ok":false,"error":"..."}
//
//	{"event":"message", ...}   whenever WhatsApp says something
//
// Logs go to stderr. Closing stdin disconnects and exits.
package main

import (
	"bufio"
	"context"
	"crypto/sha256"
	"database/sql"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"time"

	"go.mau.fi/whatsmeow"
	"go.mau.fi/whatsmeow/proto/waCompanionReg"
	"go.mau.fi/whatsmeow/proto/waE2E"
	"go.mau.fi/whatsmeow/store"
	"go.mau.fi/whatsmeow/store/sqlstore"
	"go.mau.fi/whatsmeow/types"
	"go.mau.fi/whatsmeow/types/events"
	waLog "go.mau.fi/whatsmeow/util/log"
	"google.golang.org/protobuf/proto"
	_ "modernc.org/sqlite"
)

// --- the wire -------------------------------------------------------------

var (
	outMu sync.Mutex
	out   = bufio.NewWriterSize(os.Stdout, 1<<16)
)

func emit(v any) {
	b, err := json.Marshal(v)
	if err != nil {
		logf("ERROR", "marshal: %v", err)
		return
	}
	outMu.Lock()
	defer outMu.Unlock()
	out.Write(b)
	out.WriteByte('\n')
	out.Flush()
}

type obj = map[string]any

func event(kind string, fields obj) {
	if fields == nil {
		fields = obj{}
	}
	fields["event"] = kind
	emit(fields)
}

// --- logging (stderr only: stdout is the wire) ------------------------------

var logLevel = 1 // 0 debug, 1 info, 2 warn, 3 error

func logf(level, msg string, args ...any) {
	fmt.Fprintf(os.Stderr, "%s [%s] %s\n", time.Now().Format("15:04:05.000"), level, fmt.Sprintf(msg, args...))
}

type stderrLogger struct{ module string }

func (l stderrLogger) out(rank int, level, msg string, args ...any) {
	if rank >= logLevel {
		logf(level, l.module+": "+msg, args...)
	}
}
func (l stderrLogger) Debugf(msg string, args ...any) { l.out(0, "DEBUG", msg, args...) }
func (l stderrLogger) Infof(msg string, args ...any)  { l.out(1, "INFO", msg, args...) }
func (l stderrLogger) Warnf(msg string, args ...any)  { l.out(2, "WARN", msg, args...) }
func (l stderrLogger) Errorf(msg string, args ...any) { l.out(3, "ERROR", msg, args...) }
func (l stderrLogger) Sub(module string) waLog.Logger {
	return stderrLogger{module: l.module + "/" + module}
}

// --- state ------------------------------------------------------------------

var (
	cli      *whatsmeow.Client
	archive  *sql.DB
	mediaDir string
	online   bool

	// chat -> the disappearing-message timer its last message carried, in
	// seconds: a reply in a disappearing chat disappears with it.
	expiryMu sync.Mutex
	expiry   = map[string]uint32{}

	// group -> its addressing mode and when it was read: a mention in a group
	// that addresses people by LID must name the LID.
	groupMu    sync.Mutex
	groupModes = map[string]groupMode{}

	connectMu sync.Mutex
)

type groupMode struct {
	lid  bool
	read time.Time
}

func ctx() context.Context { return context.Background() }

func ownPN() types.JID {
	if cli == nil || cli.Store.ID == nil {
		return types.EmptyJID
	}
	return cli.Store.ID.ToNonAD()
}

func ownLID() types.JID {
	if cli == nil {
		return types.EmptyJID
	}
	return cli.Store.LID.ToNonAD()
}

func isMe(j types.JID) bool {
	if j.IsEmpty() {
		return false
	}
	pn, lid := ownPN(), ownLID()
	return (!pn.IsEmpty() && j.User == pn.User && j.Server == pn.Server) ||
		(!lid.IsEmpty() && j.User == lid.User && j.Server == lid.Server)
}

func identity() obj {
	pn, lid := ownPN(), ownLID()
	return obj{
		"connected": cli.IsConnected(),
		"logged_in": cli.IsLoggedIn(),
		"linked":    cli.Store.ID != nil,
		"jid":       jidString(pn),
		"phone":     pn.User,
		"lid":       jidString(lid),
		"push_name": cli.Store.PushName,
	}
}

func jidString(j types.JID) string {
	if j.IsEmpty() {
		return ""
	}
	return j.String()
}

// pnOf answers the phone-number JID behind J: J itself when it is one, ALT
// when that is, else what the LID store knows; empty when nothing does.
func pnOf(j, alt types.JID) types.JID {
	j, alt = j.ToNonAD(), alt.ToNonAD()
	if j.Server == types.DefaultUserServer {
		return j
	}
	if alt.Server == types.DefaultUserServer {
		return alt
	}
	if j.Server == types.HiddenUserServer && cli != nil {
		if pn, err := cli.Store.LIDs.GetPNForLID(ctx(), j); err == nil && !pn.IsEmpty() {
			return pn.ToNonAD()
		}
	}
	return types.EmptyJID
}

func lidOf(j, alt types.JID) types.JID {
	j, alt = j.ToNonAD(), alt.ToNonAD()
	if j.Server == types.HiddenUserServer {
		return j
	}
	if alt.Server == types.HiddenUserServer {
		return alt
	}
	if j.Server == types.DefaultUserServer && cli != nil {
		if lid, err := cli.Store.LIDs.GetLIDForPN(ctx(), j); err == nil && !lid.IsEmpty() {
			return lid.ToNonAD()
		}
	}
	return types.EmptyJID
}

// person is how one participant is named on the wire: the phone number's
// digits when known (what an allowlist names), the LID otherwise.
func person(j, alt types.JID) obj {
	pn, lid := pnOf(j, alt), lidOf(j, alt)
	id := pn.User
	if id == "" {
		id = jidString(j.ToNonAD())
	}
	return obj{"id": id, "jid": jidString(j.ToNonAD()), "phone": pn.User, "lid": jidString(lid)}
}

// canonicalChat names a chat one way whichever address the message came by:
// a direct chat by the person's phone number when known, so one person is
// one conversation; a group by its own JID.
func canonicalChat(chat, alt types.JID) types.JID {
	chat = chat.ToNonAD()
	if chat.Server == types.HiddenUserServer {
		if pn := pnOf(chat, alt); !pn.IsEmpty() {
			return pn
		}
	}
	return chat
}

func parseJID(s string) (types.JID, error) {
	s = strings.TrimSpace(s)
	if s == "" {
		return types.EmptyJID, errors.New("no chat named")
	}
	if !strings.Contains(s, "@") {
		digits := strings.Map(func(r rune) rune {
			if r >= '0' && r <= '9' {
				return r
			}
			return -1
		}, s)
		if digits == "" {
			return types.EmptyJID, fmt.Errorf("%q is no WhatsApp address", s)
		}
		return types.NewJID(digits, types.DefaultUserServer), nil
	}
	return types.ParseJID(s)
}

// --- the archive --------------------------------------------------------------
// Every message the device sees or sends, by chat and id: a quote names the
// message it quotes by sender and text, a reaction names its target's sender,
// and `history' reads a chat back. The phone's history sync at linking fills
// it with what was said before.

const archiveSchema = `
CREATE TABLE IF NOT EXISTS nc_messages (
  chat TEXT NOT NULL, id TEXT NOT NULL, sender TEXT NOT NULL DEFAULT '',
  sender_id TEXT NOT NULL DEFAULT '', push_name TEXT NOT NULL DEFAULT '',
  from_me INTEGER NOT NULL DEFAULT 0, ts INTEGER NOT NULL DEFAULT 0,
  kind TEXT NOT NULL DEFAULT 'text', text TEXT NOT NULL DEFAULT '',
  deleted INTEGER NOT NULL DEFAULT 0, PRIMARY KEY (chat, id));
CREATE INDEX IF NOT EXISTS nc_messages_chat_ts ON nc_messages (chat, ts);
CREATE TABLE IF NOT EXISTS nc_polls (
  id TEXT PRIMARY KEY, chat TEXT NOT NULL, options TEXT NOT NULL);
`

type archived struct {
	Chat, ID, Sender, SenderID, PushName string
	FromMe                               bool
	TS                                   int64
	Kind, Text                           string
}

func keep(m archived) {
	if archive == nil || m.ID == "" || m.Chat == "" {
		return
	}
	_, err := archive.Exec(`INSERT INTO nc_messages (chat,id,sender,sender_id,push_name,from_me,ts,kind,text)
 VALUES (?,?,?,?,?,?,?,?,?) ON CONFLICT(chat,id) DO UPDATE SET text=excluded.text, kind=excluded.kind,
 push_name=CASE WHEN excluded.push_name<>'' THEN excluded.push_name ELSE nc_messages.push_name END`,
		m.Chat, m.ID, m.Sender, m.SenderID, m.PushName, boolInt(m.FromMe), m.TS, m.Kind, m.Text)
	if err != nil {
		logf("WARN", "archive: %v", err)
	}
}

func boolInt(b bool) int {
	if b {
		return 1
	}
	return 0
}

func recall(chat, id string) (archived, bool) {
	var m archived
	if archive == nil {
		return m, false
	}
	var fromMe int
	err := archive.QueryRow(`SELECT chat,id,sender,sender_id,push_name,from_me,ts,kind,text FROM nc_messages WHERE chat=? AND id=?`,
		chat, id).Scan(&m.Chat, &m.ID, &m.Sender, &m.SenderID, &m.PushName, &fromMe, &m.TS, &m.Kind, &m.Text)
	m.FromMe = fromMe == 1
	return m, err == nil
}

// --- what a message says --------------------------------------------------------

type mediaRef struct {
	Type          string `json:"type"`
	DirectPath    string `json:"direct_path"`
	MediaKey      []byte `json:"media_key"`
	FileEncSHA256 []byte `json:"file_enc_sha256"`
	FileSHA256    []byte `json:"file_sha256"`
	FileLength    uint64 `json:"file_length"`
	Mime          string `json:"mime"`
	Filename      string `json:"filename,omitempty"`
	Seconds       uint32 `json:"seconds,omitempty"`
	PTT           bool   `json:"ptt,omitempty"`
}

type said struct {
	Kind  string
	Text  string
	Media *mediaRef
	Ctx   *waE2E.ContextInfo
}

func describe(m *waE2E.Message) said {
	if m == nil {
		return said{}
	}
	switch {
	case m.Conversation != nil:
		return said{Kind: "text", Text: m.GetConversation()}
	case m.ExtendedTextMessage != nil:
		x := m.GetExtendedTextMessage()
		return said{Kind: "text", Text: x.GetText(), Ctx: x.GetContextInfo()}
	case m.ImageMessage != nil:
		x := m.GetImageMessage()
		return said{Kind: "image", Text: x.GetCaption(), Ctx: x.GetContextInfo(), Media: &mediaRef{
			Type: "image", DirectPath: x.GetDirectPath(), MediaKey: x.GetMediaKey(), FileEncSHA256: x.GetFileEncSHA256(),
			FileSHA256: x.GetFileSHA256(), FileLength: x.GetFileLength(), Mime: x.GetMimetype(), Filename: "photo.jpg"}}
	case m.VideoMessage != nil:
		x := m.GetVideoMessage()
		return said{Kind: "video", Text: x.GetCaption(), Ctx: x.GetContextInfo(), Media: &mediaRef{
			Type: "video", DirectPath: x.GetDirectPath(), MediaKey: x.GetMediaKey(), FileEncSHA256: x.GetFileEncSHA256(),
			FileSHA256: x.GetFileSHA256(), FileLength: x.GetFileLength(), Mime: x.GetMimetype(), Filename: "video.mp4",
			Seconds: x.GetSeconds()}}
	case m.PtvMessage != nil:
		x := m.GetPtvMessage()
		return said{Kind: "video", Text: x.GetCaption(), Ctx: x.GetContextInfo(), Media: &mediaRef{
			Type: "video", DirectPath: x.GetDirectPath(), MediaKey: x.GetMediaKey(), FileEncSHA256: x.GetFileEncSHA256(),
			FileSHA256: x.GetFileSHA256(), FileLength: x.GetFileLength(), Mime: x.GetMimetype(), Filename: "video-note.mp4",
			Seconds: x.GetSeconds()}}
	case m.AudioMessage != nil:
		x := m.GetAudioMessage()
		kind, name := "audio", "audio"+extOf(x.GetMimetype(), ".ogg")
		if x.GetPTT() {
			kind, name = "voice", "voice.ogg"
		}
		return said{Kind: kind, Ctx: x.GetContextInfo(), Media: &mediaRef{
			Type: "audio", DirectPath: x.GetDirectPath(), MediaKey: x.GetMediaKey(), FileEncSHA256: x.GetFileEncSHA256(),
			FileSHA256: x.GetFileSHA256(), FileLength: x.GetFileLength(), Mime: x.GetMimetype(), Filename: name,
			Seconds: x.GetSeconds(), PTT: x.GetPTT()}}
	case m.DocumentMessage != nil:
		x := m.GetDocumentMessage()
		name := x.GetFileName()
		if name == "" {
			name = x.GetTitle()
		}
		return said{Kind: "document", Text: x.GetCaption(), Ctx: x.GetContextInfo(), Media: &mediaRef{
			Type: "document", DirectPath: x.GetDirectPath(), MediaKey: x.GetMediaKey(), FileEncSHA256: x.GetFileEncSHA256(),
			FileSHA256: x.GetFileSHA256(), FileLength: x.GetFileLength(), Mime: x.GetMimetype(), Filename: name}}
	case m.StickerMessage != nil:
		x := m.GetStickerMessage()
		return said{Kind: "sticker", Ctx: x.GetContextInfo(), Media: &mediaRef{
			Type: "sticker", DirectPath: x.GetDirectPath(), MediaKey: x.GetMediaKey(), FileEncSHA256: x.GetFileEncSHA256(),
			FileSHA256: x.GetFileSHA256(), FileLength: x.GetFileLength(), Mime: x.GetMimetype(), Filename: "sticker.webp"}}
	case m.LocationMessage != nil:
		x := m.GetLocationMessage()
		return said{Kind: "location", Ctx: x.GetContextInfo(), Text: strings.TrimSpace(fmt.Sprintf("[location %.6f,%.6f] %s %s",
			x.GetDegreesLatitude(), x.GetDegreesLongitude(), x.GetName(), x.GetAddress()))}
	case m.LiveLocationMessage != nil:
		x := m.GetLiveLocationMessage()
		return said{Kind: "location", Ctx: x.GetContextInfo(), Text: strings.TrimSpace(fmt.Sprintf("[live location %.6f,%.6f] %s",
			x.GetDegreesLatitude(), x.GetDegreesLongitude(), x.GetCaption()))}
	case m.ContactMessage != nil:
		x := m.GetContactMessage()
		return said{Kind: "contact", Ctx: x.GetContextInfo(), Text: fmt.Sprintf("[contact %s]\n%s", x.GetDisplayName(), x.GetVcard())}
	case m.ContactsArrayMessage != nil:
		x := m.GetContactsArrayMessage()
		names := []string{}
		for _, c := range x.GetContacts() {
			names = append(names, c.GetDisplayName())
		}
		return said{Kind: "contact", Ctx: x.GetContextInfo(), Text: "[contacts " + strings.Join(names, ", ") + "]"}
	case m.PollCreationMessage != nil || m.PollCreationMessageV2 != nil || m.PollCreationMessageV3 != nil:
		x := m.GetPollCreationMessage()
		if x == nil {
			x = m.GetPollCreationMessageV2()
		}
		if x == nil {
			x = m.GetPollCreationMessageV3()
		}
		opts := []string{}
		for _, o := range x.GetOptions() {
			opts = append(opts, o.GetOptionName())
		}
		return said{Kind: "poll", Ctx: x.GetContextInfo(), Text: fmt.Sprintf("[poll] %s: %s", x.GetName(), strings.Join(opts, " / "))}
	case m.ButtonsResponseMessage != nil:
		x := m.GetButtonsResponseMessage()
		return said{Kind: "text", Ctx: x.GetContextInfo(), Text: x.GetSelectedDisplayText()}
	case m.ListResponseMessage != nil:
		x := m.GetListResponseMessage()
		return said{Kind: "text", Ctx: x.GetContextInfo(), Text: x.GetTitle()}
	case m.TemplateButtonReplyMessage != nil:
		x := m.GetTemplateButtonReplyMessage()
		return said{Kind: "text", Ctx: x.GetContextInfo(), Text: x.GetSelectedDisplayText()}
	case m.GroupInviteMessage != nil:
		x := m.GetGroupInviteMessage()
		return said{Kind: "text", Ctx: x.GetContextInfo(), Text: fmt.Sprintf("[group invite %s] %s", x.GetGroupName(), x.GetCaption())}
	}
	return said{}
}

func extOf(mime, fallback string) string {
	switch {
	case strings.Contains(mime, "mpeg"):
		return ".mp3"
	case strings.Contains(mime, "mp4"), strings.Contains(mime, "aac"):
		return ".m4a"
	case strings.Contains(mime, "ogg"):
		return ".ogg"
	}
	return fallback
}

var mentionToken = regexp.MustCompile(`@(\d{5,20})`)

// readableText spells every @LID mention in TEXT as the phone number behind
// it, when known: one person, one name, the one a reply can mention back.
func readableText(text string, mentioned []string) string {
	for _, s := range mentioned {
		j, err := types.ParseJID(s)
		if err != nil || j.Server != types.HiddenUserServer {
			continue
		}
		if pn := pnOf(j, types.EmptyJID); !pn.IsEmpty() {
			text = strings.ReplaceAll(text, "@"+j.User, "@"+pn.User)
		}
	}
	return text
}

func mentionList(mentioned []string) (list []obj, me bool) {
	for _, s := range mentioned {
		j, err := types.ParseJID(s)
		if err != nil {
			continue
		}
		if isMe(j) {
			me = true
		}
		list = append(list, person(j, types.EmptyJID))
	}
	return list, me
}

func noteExpiry(chat string, ci *waE2E.ContextInfo) {
	if ci == nil || ci.Expiration == nil {
		return
	}
	expiryMu.Lock()
	expiry[chat] = ci.GetExpiration()
	expiryMu.Unlock()
}

func expiryOf(chat string) uint32 {
	expiryMu.Lock()
	defer expiryMu.Unlock()
	return expiry[chat]
}

// --- events -----------------------------------------------------------------

func onEvent(raw any) {
	switch evt := raw.(type) {
	case *events.Connected:
		if online {
			if err := cli.SendPresence(ctx(), types.PresenceAvailable); err != nil {
				logf("WARN", "presence: %v", err)
			}
		}
		event("ready", identity())
	case *events.Disconnected:
		event("disconnected", nil)
	case *events.KeepAliveTimeout:
		event("keepalive_timeout", obj{"errors": evt.ErrorCount})
	case *events.KeepAliveRestored:
		event("keepalive_restored", nil)
	case *events.LoggedOut:
		event("logged_out", obj{"reason": evt.Reason.String(), "on_connect": evt.OnConnect})
	case *events.PairSuccess:
		event("paired", obj{"jid": jidString(evt.ID.ToNonAD()), "phone": evt.ID.User, "lid": jidString(evt.LID.ToNonAD()),
			"business_name": evt.BusinessName, "platform": evt.Platform})
	case *events.PairError:
		event("pair_error", obj{"error": evt.Error.Error()})
	case *events.StreamReplaced:
		event("stream_replaced", nil)
	case *events.TemporaryBan:
		event("temporary_ban", obj{"reason": evt.String(), "expire_s": int64(evt.Expire.Seconds())})
	case *events.ClientOutdated:
		event("client_outdated", nil)
	case *events.ConnectFailure:
		event("connect_failure", obj{"reason": evt.Reason.String(), "message": evt.Message})
	case *events.OfflineSyncCompleted:
		event("offline_sync", obj{"count": evt.Count})
	case *events.JoinedGroup:
		event("joined_group", obj{"chat": jidString(evt.JID), "name": evt.Name})
	case *events.CallOffer:
		event("call", obj{"from": person(evt.From, evt.CallCreatorAlt), "call_id": evt.CallID,
			"group": jidString(evt.GroupJID), "timestamp_ms": evt.Timestamp.UnixMilli()})
	case *events.UndecryptableMessage:
		event("undecryptable", obj{"chat": jidString(canonicalChat(evt.Info.Chat, evt.Info.SenderAlt)), "id": evt.Info.ID,
			"from": person(evt.Info.Sender, evt.Info.SenderAlt)})
	case *events.HistorySync:
		onHistory(evt)
	case *events.Message:
		onMessage(evt)
	}
}

func onHistory(evt *events.HistorySync) {
	n := 0
	for _, conv := range evt.Data.GetConversations() {
		chatJID, err := types.ParseJID(conv.GetID())
		if err != nil {
			continue
		}
		for _, hm := range conv.GetMessages() {
			parsed, err := cli.ParseWebMessage(chatJID, hm.GetMessage())
			if err != nil {
				continue
			}
			s := describe(parsed.Message)
			if s.Kind == "" {
				continue
			}
			info := parsed.Info
			sender := person(info.Sender, info.SenderAlt)
			keep(archived{Chat: jidString(canonicalChat(info.Chat, info.SenderAlt)), ID: info.ID,
				Sender: jidString(info.Sender.ToNonAD()), SenderID: sender["id"].(string), PushName: info.PushName,
				FromMe: info.IsFromMe, TS: info.Timestamp.UnixMilli(), Kind: s.Kind, Text: s.Text})
			n++
		}
	}
	event("history_sync", obj{"type": evt.Data.GetSyncType().String(), "messages": n})
}

func onMessage(evt *events.Message) {
	info := evt.Info
	if info.Chat.Server == types.BroadcastServer || info.Chat.Server == types.NewsletterServer {
		return
	}
	chat := jidString(canonicalChat(info.Chat, info.SenderAlt))
	if !info.IsGroup && info.IsFromMe && !info.RecipientAlt.IsEmpty() {
		chat = jidString(canonicalChat(info.Chat, info.RecipientAlt))
	}
	from := person(info.Sender, info.SenderAlt)
	base := obj{
		"chat":         chat,
		"group":        info.IsGroup,
		"from":         from,
		"from_me":      info.IsFromMe,
		"push_name":    info.PushName,
		"timestamp_ms": info.Timestamp.UnixMilli(),
	}
	m := evt.Message

	if r := m.GetReactionMessage(); r != nil {
		key := r.GetKey()
		base["target_id"] = key.GetID()
		base["target_from_me"] = key.GetFromMe()
		base["emoji"] = r.GetText()
		event("reaction", base)
		return
	}
	if m.GetPollUpdateMessage() != nil {
		onVote(evt, base)
		return
	}
	if p := m.GetProtocolMessage(); p != nil {
		switch p.GetType() {
		case waE2E.ProtocolMessage_REVOKE:
			base["target_id"] = p.GetKey().GetID()
			if archive != nil {
				archive.Exec(`UPDATE nc_messages SET deleted=1 WHERE chat=? AND id=?`, chat, p.GetKey().GetID())
			}
			event("revoke", base)
		case waE2E.ProtocolMessage_MESSAGE_EDIT:
			s := describe(p.GetEditedMessage())
			target := p.GetKey().GetID()
			mentioned := s.Ctx.GetMentionedJID()
			mentions, me := mentionList(mentioned)
			base["id"] = target
			base["text"] = readableText(s.Text, mentioned)
			base["mentions"] = mentions
			base["mentions_me"] = me
			base["kind"] = s.Kind
			if prior, ok := recall(chat, target); ok {
				prior.Text = s.Text
				keep(prior)
			}
			event("edit", base)
		case waE2E.ProtocolMessage_EPHEMERAL_SETTING:
			expiryMu.Lock()
			expiry[chat] = p.GetEphemeralExpiration()
			expiryMu.Unlock()
		}
		return
	}

	s := describe(m)
	if s.Kind == "" {
		return
	}
	noteExpiry(chat, s.Ctx)
	mentioned := s.Ctx.GetMentionedJID()
	mentions, me := mentionList(mentioned)
	text := readableText(s.Text, mentioned)
	keep(archived{Chat: chat, ID: info.ID, Sender: jidString(info.Sender.ToNonAD()), SenderID: from["id"].(string),
		PushName: info.PushName, FromMe: info.IsFromMe, TS: info.Timestamp.UnixMilli(), Kind: s.Kind, Text: text})

	base["id"] = info.ID
	base["kind"] = s.Kind
	base["text"] = text
	base["mentions"] = mentions
	base["mentions_me"] = me
	base["forwarded"] = s.Ctx.GetIsForwarded()
	base["view_once"] = evt.IsViewOnce
	base["expiration"] = s.Ctx.GetExpiration()
	if s.Media != nil {
		base["media"] = s.Media
	}
	if qid := s.Ctx.GetStanzaID(); qid != "" {
		q := obj{"id": qid}
		var qsender types.JID
		if p := s.Ctx.GetParticipant(); p != "" {
			qsender, _ = types.ParseJID(p)
		}
		qs := describe(s.Ctx.GetQuotedMessage())
		if prior, ok := recall(chat, qid); ok {
			if qs.Kind == "" {
				qs.Text, qs.Kind = prior.Text, prior.Kind
			}
			q["from_me"] = prior.FromMe
			if prior.PushName != "" {
				q["push_name"] = prior.PushName
			}
		}
		if !qsender.IsEmpty() {
			q["from"] = person(qsender, types.EmptyJID)
			q["from_me"] = isMe(qsender)
		}
		q["text"] = readableText(qs.Text, qs.Ctx.GetMentionedJID())
		q["kind"] = qs.Kind
		if qs.Media != nil {
			q["media"] = qs.Media
		}
		base["quote"] = q
	}
	event("message", base)
}

func onVote(evt *events.Message, base obj) {
	vote, err := cli.DecryptPollVote(ctx(), evt)
	pollID := evt.Message.GetPollUpdateMessage().GetPollCreationMessageKey().GetID()
	base["poll_id"] = pollID
	if err != nil {
		base["error"] = err.Error()
		event("poll_vote", base)
		return
	}
	var names []string
	if archive != nil {
		var raw string
		if archive.QueryRow(`SELECT options FROM nc_polls WHERE id=?`, pollID).Scan(&raw) == nil {
			var options []string
			json.Unmarshal([]byte(raw), &options)
			for _, hash := range vote.GetSelectedOptions() {
				for _, o := range options {
					sum := sha256.Sum256([]byte(o))
					if string(sum[:]) == string(hash) {
						names = append(names, o)
					}
				}
			}
		}
	}
	if names == nil {
		names = []string{}
	}
	base["options"] = names
	event("poll_vote", base)
}

// --- commands -----------------------------------------------------------------

type request struct {
	ID   string          `json:"id"`
	Op   string          `json:"op"`
	Args json.RawMessage `json:"args"`
}

type failure struct {
	status int
	err    error
}

func (f failure) Error() string { return f.err.Error() }

// refused is a request the driver will not carry out as asked: the cell
// reads it as a 4xx, never retried.
func refused(format string, args ...any) error {
	return failure{status: 400, err: fmt.Errorf(format, args...)}
}

func serve(req request) {
	defer func() {
		if r := recover(); r != nil {
			emit(obj{"id": req.ID, "ok": false, "status": 500, "error": fmt.Sprintf("driver panic: %v", r)})
		}
	}()
	var args map[string]json.RawMessage
	if len(req.Args) > 0 {
		if err := json.Unmarshal(req.Args, &args); err != nil {
			emit(obj{"id": req.ID, "ok": false, "status": 400, "error": "args: " + err.Error()})
			return
		}
	}
	a := argReader(args)
	result, err := dispatch(req.Op, a)
	if err != nil {
		status := 502
		var f failure
		if errors.As(err, &f) {
			status = f.status
		} else if errors.Is(err, whatsmeow.ErrNotConnected) || errors.Is(err, whatsmeow.ErrNotLoggedIn) {
			status = 503
		}
		emit(obj{"id": req.ID, "ok": false, "status": status, "error": err.Error()})
		return
	}
	emit(obj{"id": req.ID, "ok": true, "status": 200, "result": result})
}

type argReader map[string]json.RawMessage

func (a argReader) str(key string) string {
	var s string
	if raw, ok := a[key]; ok {
		json.Unmarshal(raw, &s)
	}
	return s
}
func (a argReader) num(key string) int64 {
	var n float64
	if raw, ok := a[key]; ok {
		json.Unmarshal(raw, &n)
	}
	return int64(n)
}
func (a argReader) flag(key string) bool {
	var b bool
	if raw, ok := a[key]; ok {
		json.Unmarshal(raw, &b)
	}
	return b
}
func (a argReader) strs(key string) []string {
	var s []string
	if raw, ok := a[key]; ok {
		json.Unmarshal(raw, &s)
	}
	return s
}

// octets reads an array of numbers 0-255 as bytes: what a voice message's
// waveform is, sent as plain JSON numbers.
func (a argReader) octets(key string) []byte {
	var ns []int
	if raw, ok := a[key]; ok {
		json.Unmarshal(raw, &ns)
	}
	out := make([]byte, 0, len(ns))
	for _, n := range ns {
		if n < 0 {
			n = 0
		} else if n > 255 {
			n = 255
		}
		out = append(out, byte(n))
	}
	return out
}
func (a argReader) media(key string) *mediaRef {
	raw, ok := a[key]
	if !ok {
		return nil
	}
	var m mediaRef
	if json.Unmarshal(raw, &m) != nil || m.DirectPath == "" {
		return nil
	}
	return &m
}

func needLogin() error {
	if cli.Store.ID == nil {
		return failure{status: 401, err: errors.New("this WhatsApp device is not linked yet: link it first (pair_phone or the QR code)")}
	}
	if !cli.IsConnected() {
		return failure{status: 503, err: errors.New("not connected to WhatsApp right now")}
	}
	return nil
}

var knownOps = map[string]bool{
	"send": true, "edit": true, "revoke": true, "react": true, "typing": true, "read": true,
	"send_file": true, "poll": true, "download": true, "group_info": true, "groups": true,
	"resolve": true, "history": true, "contact": true, "presence": true, "reject_call": true,
}

func dispatch(op string, a argReader) (any, error) {
	switch op {
	case "status":
		return identity(), nil
	case "connect":
		return identity(), connect()
	case "pair_phone":
		return pairPhone(a.str("phone"))
	case "logout":
		if cli.Store.ID == nil {
			return obj{"linked": false}, nil
		}
		return obj{"linked": false}, cli.Logout(ctx())
	}
	if !knownOps[op] {
		return nil, refused("unknown op %q", op)
	}
	if err := needLogin(); err != nil {
		return nil, err
	}
	switch op {
	case "send":
		return sendText(a)
	case "edit":
		return editText(a)
	case "revoke":
		return revoke(a)
	case "react":
		return react(a)
	case "typing":
		return typing(a)
	case "read":
		return markRead(a)
	case "send_file":
		return sendFile(a)
	case "poll":
		return sendPoll(a)
	case "download":
		return download(a)
	case "group_info":
		return groupInfo(a.str("chat"))
	case "groups":
		return groups()
	case "resolve":
		return resolve(a.strs("phones"))
	case "history":
		return history(a)
	case "contact":
		return contact(a.str("user"))
	case "presence":
		state := types.PresenceAvailable
		if a.str("state") == "unavailable" {
			state = types.PresenceUnavailable
		}
		return obj{"state": string(state)}, cli.SendPresence(ctx(), state)
	case "reject_call":
		from, err := parseJID(a.str("from"))
		if err != nil {
			return nil, refused("%v", err)
		}
		return obj{}, cli.RejectCall(ctx(), from, a.str("call_id"))
	}
	return nil, refused("unknown op %q", op)
}

func connect() error {
	connectMu.Lock()
	defer connectMu.Unlock()
	if cli.IsConnected() {
		return nil
	}
	if cli.Store.ID == nil {
		qr, err := cli.GetQRChannel(ctx())
		if err != nil {
			return err
		}
		go func() {
			for item := range qr {
				switch item.Event {
				case "code":
					event("qr", obj{"code": item.Code, "timeout_s": int64(item.Timeout.Seconds())})
				case "error":
					msg := "pairing failed"
					if item.Error != nil {
						msg = item.Error.Error()
					}
					event("pair_state", obj{"state": "error", "error": msg})
				default:
					event("pair_state", obj{"state": item.Event})
				}
			}
		}()
	}
	return cli.Connect()
}

func pairPhone(phone string) (any, error) {
	if cli.Store.ID != nil {
		return nil, refused("this device is already linked to %s", ownPN().User)
	}
	if strings.TrimSpace(phone) == "" {
		return nil, refused("pair_phone needs the phone number to link, with its country code")
	}
	if !cli.IsConnected() {
		if err := connect(); err != nil {
			return nil, err
		}
		time.Sleep(2 * time.Second)
	}
	code, err := cli.PairPhone(ctx(), phone, true, whatsmeow.PairClientChrome, "Chrome (Linux)")
	if err != nil {
		return nil, err
	}
	return obj{"code": code}, nil
}

// mentionJIDs resolves every person TEXT mentions -- MENTIONS named outright
// (digits or JIDs), and every @digits in the words that is a phone number on
// WhatsApp -- into the JIDs the message lists, spelled in the chat's own
// addressing: a group that names people by LID is told the LID, and the words
// say @<lid> where they said @<number>.
func mentionJIDs(chat types.JID, text string, mentions []string) (string, []string) {
	seen := map[string]bool{}
	var out []string
	lidGroup := chat.Server == types.GroupServer && groupIsLID(chat)
	add := func(j types.JID) {
		j = j.ToNonAD()
		if lidGroup && j.Server == types.DefaultUserServer {
			if lid := lidOf(j, types.EmptyJID); !lid.IsEmpty() {
				text = strings.ReplaceAll(text, "@"+j.User, "@"+lid.User)
				j = lid
			}
		}
		if !seen[j.String()] {
			seen[j.String()] = true
			out = append(out, j.String())
		}
	}
	for _, m := range mentions {
		if j, err := parseJID(m); err == nil {
			add(j)
		}
	}
	if chat.Server == types.GroupServer {
		for _, match := range mentionToken.FindAllStringSubmatch(text, -1) {
			digits := match[1]
			if inGroup(chat, digits) {
				add(types.NewJID(digits, types.DefaultUserServer))
			}
		}
	}
	return text, out
}

func groupIsLID(chat types.JID) bool {
	groupMu.Lock()
	mode, ok := groupModes[chat.String()]
	groupMu.Unlock()
	if ok && time.Since(mode.read) < 10*time.Minute {
		return mode.lid
	}
	info, err := cli.GetGroupInfo(ctx(), chat)
	if err != nil {
		return ok && mode.lid
	}
	noteGroup(info)
	return info.AddressingMode == types.AddressingModeLID
}

var memberMu sync.Mutex
var members = map[string]map[string]bool{}

func noteGroup(info *types.GroupInfo) {
	set := map[string]bool{}
	for _, p := range info.Participants {
		for _, j := range []types.JID{p.JID, p.PhoneNumber, p.LID} {
			if !j.IsEmpty() {
				set[j.User] = true
			}
		}
	}
	memberMu.Lock()
	members[info.JID.String()] = set
	memberMu.Unlock()
	groupMu.Lock()
	groupModes[info.JID.String()] = groupMode{lid: info.AddressingMode == types.AddressingModeLID, read: time.Now()}
	groupMu.Unlock()
}

func inGroup(chat types.JID, user string) bool {
	memberMu.Lock()
	defer memberMu.Unlock()
	return members[chat.String()][user]
}

// contextFor is the ContextInfo a post carries: the message it quotes,
// whom it mentions, and the chat's disappearing timer.
func contextFor(chat types.JID, quote string, mentioned []string) *waE2E.ContextInfo {
	ci := &waE2E.ContextInfo{}
	used := false
	if quote != "" {
		ci.StanzaID = proto.String(quote)
		quoted := &waE2E.Message{Conversation: proto.String("")}
		if prior, ok := recall(chat.String(), quote); ok {
			sender := prior.Sender
			if prior.FromMe || sender == "" {
				sender = ownPN().String()
			}
			ci.Participant = proto.String(sender)
			quoted.Conversation = proto.String(prior.Text)
		} else if chat.Server != types.GroupServer {
			ci.Participant = proto.String(chat.String())
		}
		ci.QuotedMessage = quoted
		used = true
	}
	if len(mentioned) > 0 {
		ci.MentionedJID = mentioned
		used = true
	}
	if e := expiryOf(chat.String()); e > 0 {
		ci.Expiration = proto.Uint32(e)
		used = true
	}
	if !used {
		return nil
	}
	return ci
}

func textMessage(chat types.JID, text, quote string, mentions []string) (*waE2E.Message, string) {
	text, mentioned := mentionJIDs(chat, text, mentions)
	ci := contextFor(chat, quote, mentioned)
	if ci == nil {
		return &waE2E.Message{Conversation: proto.String(text)}, text
	}
	return &waE2E.Message{ExtendedTextMessage: &waE2E.ExtendedTextMessage{Text: proto.String(text), ContextInfo: ci}}, text
}

func sendAndKeep(chat types.JID, msg *waE2E.Message, kind, text string) (obj, error) {
	resp, err := cli.SendMessage(ctx(), chat, msg)
	if err != nil {
		return nil, err
	}
	keep(archived{Chat: chat.String(), ID: resp.ID, Sender: ownPN().String(), SenderID: ownPN().User,
		PushName: cli.Store.PushName, FromMe: true, TS: resp.Timestamp.UnixMilli(), Kind: kind, Text: text})
	return obj{"id": resp.ID, "chat": chat.String(), "timestamp_ms": resp.Timestamp.UnixMilli()}, nil
}

func chatArg(a argReader) (types.JID, error) {
	chat, err := parseJID(a.str("chat"))
	if err != nil {
		return chat, refused("%v", err)
	}
	return chat, nil
}

func sendText(a argReader) (any, error) {
	chat, err := chatArg(a)
	if err != nil {
		return nil, err
	}
	if strings.TrimSpace(a.str("text")) == "" {
		return nil, refused("send needs text")
	}
	msg, text := textMessage(chat, a.str("text"), a.str("quote"), a.strs("mentions"))
	return sendAndKeep(chat, msg, "text", text)
}

func editText(a argReader) (any, error) {
	chat, err := chatArg(a)
	if err != nil {
		return nil, err
	}
	id := a.str("id")
	if prior, ok := recall(chat.String(), id); ok {
		if !prior.FromMe {
			return nil, refused("message %s is not ours to edit", id)
		}
		if age := time.Since(time.UnixMilli(prior.TS)); age > whatsmeow.EditWindow-30*time.Second {
			return nil, refused("WhatsApp allows an edit for %v after sending; message %s is %v old",
				whatsmeow.EditWindow, id, age.Round(time.Second))
		}
	}
	text, mentioned := mentionJIDs(chat, a.str("text"), a.strs("mentions"))
	var content *waE2E.Message
	if len(mentioned) > 0 {
		content = &waE2E.Message{ExtendedTextMessage: &waE2E.ExtendedTextMessage{Text: proto.String(text),
			ContextInfo: &waE2E.ContextInfo{MentionedJID: mentioned}}}
	} else {
		content = &waE2E.Message{Conversation: proto.String(text)}
	}
	resp, err := cli.SendMessage(ctx(), chat, cli.BuildEdit(chat, id, content))
	if err != nil {
		return nil, err
	}
	if archive != nil {
		archive.Exec(`UPDATE nc_messages SET text=? WHERE chat=? AND id=?`, text, chat.String(), id)
	}
	return obj{"id": id, "chat": chat.String(), "timestamp_ms": resp.Timestamp.UnixMilli()}, nil
}

func revoke(a argReader) (any, error) {
	chat, err := chatArg(a)
	if err != nil {
		return nil, err
	}
	id := a.str("id")
	sender := types.EmptyJID
	if prior, ok := recall(chat.String(), id); ok && !prior.FromMe && prior.Sender != "" {
		sender, _ = types.ParseJID(prior.Sender)
	}
	resp, err := cli.SendMessage(ctx(), chat, cli.BuildRevoke(chat, sender, id))
	if err != nil {
		return nil, err
	}
	if archive != nil {
		archive.Exec(`UPDATE nc_messages SET deleted=1 WHERE chat=? AND id=?`, chat.String(), id)
	}
	return obj{"id": id, "timestamp_ms": resp.Timestamp.UnixMilli()}, nil
}

func react(a argReader) (any, error) {
	chat, err := chatArg(a)
	if err != nil {
		return nil, err
	}
	id := a.str("id")
	sender := ownPN()
	if prior, ok := recall(chat.String(), id); ok && !prior.FromMe && prior.Sender != "" {
		sender, _ = types.ParseJID(prior.Sender)
	} else if !ok && chat.Server != types.GroupServer {
		sender = chat
	} else if !ok {
		return nil, refused("message %s in %s is not one this device has seen", id, chat)
	}
	resp, err := cli.SendMessage(ctx(), chat, cli.BuildReaction(chat, sender, id, a.str("emoji")))
	if err != nil {
		return nil, err
	}
	return obj{"id": resp.ID, "target_id": id, "emoji": a.str("emoji")}, nil
}

func typing(a argReader) (any, error) {
	chat, err := chatArg(a)
	if err != nil {
		return nil, err
	}
	state := types.ChatPresenceComposing
	if a.str("state") == "paused" {
		state = types.ChatPresencePaused
	}
	media := types.ChatPresenceMediaText
	if a.str("media") == "audio" {
		media = types.ChatPresenceMediaAudio
	}
	return obj{"state": string(state)}, cli.SendChatPresence(ctx(), chat, state, media)
}

func markRead(a argReader) (any, error) {
	chat, err := chatArg(a)
	if err != nil {
		return nil, err
	}
	ids := a.strs("ids")
	if len(ids) == 0 {
		return nil, refused("read needs message ids")
	}
	sender := types.EmptyJID
	if chat.Server == types.GroupServer {
		if prior, ok := recall(chat.String(), ids[0]); ok && prior.Sender != "" {
			sender, _ = types.ParseJID(prior.Sender)
		}
	}
	return obj{"ids": ids}, cli.MarkRead(ctx(), ids, time.Now(), chat, sender)
}

var mediaKinds = map[string]whatsmeow.MediaType{
	"image": whatsmeow.MediaImage, "video": whatsmeow.MediaVideo, "audio": whatsmeow.MediaAudio,
	"voice": whatsmeow.MediaAudio, "document": whatsmeow.MediaDocument, "sticker": whatsmeow.MediaImage,
}

func sendFile(a argReader) (any, error) {
	chat, err := chatArg(a)
	if err != nil {
		return nil, err
	}
	path := a.str("path")
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, refused("cannot read %s: %v", filepath.Base(path), err)
	}
	kind := a.str("kind")
	mediaType, ok := mediaKinds[kind]
	if !ok {
		return nil, refused("unknown file kind %q", kind)
	}
	up, err := cli.Upload(ctx(), data, mediaType)
	if err != nil {
		return nil, err
	}
	caption, mentioned := mentionJIDs(chat, a.str("caption"), a.strs("mentions"))
	ci := contextFor(chat, a.str("quote"), mentioned)
	mime := a.str("mime")
	name := a.str("filename")
	if name == "" {
		name = filepath.Base(path)
	}
	msg := &waE2E.Message{}
	switch kind {
	case "image":
		msg.ImageMessage = &waE2E.ImageMessage{Caption: optString(caption), Mimetype: proto.String(mime),
			URL: &up.URL, DirectPath: &up.DirectPath, MediaKey: up.MediaKey, FileEncSHA256: up.FileEncSHA256,
			FileSHA256: up.FileSHA256, FileLength: &up.FileLength, ContextInfo: ci}
	case "video":
		msg.VideoMessage = &waE2E.VideoMessage{Caption: optString(caption), Mimetype: proto.String(mime),
			URL: &up.URL, DirectPath: &up.DirectPath, MediaKey: up.MediaKey, FileEncSHA256: up.FileEncSHA256,
			FileSHA256: up.FileSHA256, FileLength: &up.FileLength, ContextInfo: ci}
	case "audio", "voice":
		msg.AudioMessage = &waE2E.AudioMessage{Mimetype: proto.String(mime), PTT: proto.Bool(kind == "voice"),
			URL: &up.URL, DirectPath: &up.DirectPath, MediaKey: up.MediaKey, FileEncSHA256: up.FileEncSHA256,
			FileSHA256: up.FileSHA256, FileLength: &up.FileLength, ContextInfo: ci}
		if s := a.num("seconds"); s > 0 {
			msg.AudioMessage.Seconds = proto.Uint32(uint32(s))
		}
		if w := a.octets("waveform"); len(w) > 0 {
			msg.AudioMessage.Waveform = w
		}
	case "sticker":
		msg.StickerMessage = &waE2E.StickerMessage{Mimetype: proto.String(mime),
			URL: &up.URL, DirectPath: &up.DirectPath, MediaKey: up.MediaKey, FileEncSHA256: up.FileEncSHA256,
			FileSHA256: up.FileSHA256, FileLength: &up.FileLength, ContextInfo: ci}
	default:
		msg.DocumentMessage = &waE2E.DocumentMessage{Caption: optString(caption), Mimetype: proto.String(mime),
			FileName: proto.String(name), Title: proto.String(name),
			URL: &up.URL, DirectPath: &up.DirectPath, MediaKey: up.MediaKey, FileEncSHA256: up.FileEncSHA256,
			FileSHA256: up.FileSHA256, FileLength: &up.FileLength, ContextInfo: ci}
	}
	return sendAndKeep(chat, msg, kind, caption)
}

func optString(s string) *string {
	if s == "" {
		return nil
	}
	return proto.String(s)
}

func sendPoll(a argReader) (any, error) {
	chat, err := chatArg(a)
	if err != nil {
		return nil, err
	}
	options := a.strs("options")
	if len(options) < 2 || len(options) > 12 {
		return nil, refused("a poll takes 2 to 12 options, got %d", len(options))
	}
	selectable := int(a.num("selectable"))
	if selectable <= 0 {
		selectable = 1
	}
	msg := cli.BuildPollCreation(a.str("name"), options, selectable)
	if quote := a.str("quote"); quote != "" {
		msg.PollCreationMessage.ContextInfo = contextFor(chat, quote, nil)
	}
	res, err := sendAndKeep(chat, msg, "poll", a.str("name"))
	if err != nil {
		return nil, err
	}
	if archive != nil {
		raw, _ := json.Marshal(options)
		archive.Exec(`INSERT OR REPLACE INTO nc_polls (id, chat, options) VALUES (?,?,?)`, res["id"], chat.String(), string(raw))
	}
	return res, nil
}

func download(a argReader) (any, error) {
	ref := a.media("media")
	if ref == nil {
		return nil, refused("download needs the media reference a message carried")
	}
	mediaType, ok := mediaKinds[ref.Type]
	if !ok {
		return nil, refused("unknown media type %q", ref.Type)
	}
	data, err := cli.DownloadMediaWithPath(ctx(), ref.DirectPath, ref.FileEncSHA256, ref.FileSHA256, ref.MediaKey,
		mediaType, "", false)
	if err != nil {
		return nil, err
	}
	dest := a.str("dest")
	if dest == "" {
		f, err := os.CreateTemp(mediaDir, "wa-*")
		if err != nil {
			return nil, err
		}
		dest = f.Name()
		f.Close()
	}
	if err := os.WriteFile(dest, data, 0o600); err != nil {
		return nil, err
	}
	return obj{"path": dest, "size": len(data), "mime": ref.Mime}, nil
}

func groupInfo(s string) (any, error) {
	chat, err := parseJID(s)
	if err != nil {
		return nil, refused("%v", err)
	}
	if chat.Server != types.GroupServer {
		return nil, refused("%s is not a group", chat)
	}
	info, err := cli.GetGroupInfo(ctx(), chat)
	if err != nil {
		return nil, err
	}
	noteGroup(info)
	return describeGroup(info), nil
}

func describeGroup(info *types.GroupInfo) obj {
	people := []obj{}
	for _, p := range info.Participants {
		one := person(p.JID, p.PhoneNumber)
		if one["phone"] == "" && !p.PhoneNumber.IsEmpty() {
			one["phone"] = p.PhoneNumber.User
			one["id"] = p.PhoneNumber.User
		}
		one["admin"] = p.IsAdmin || p.IsSuperAdmin
		one["superadmin"] = p.IsSuperAdmin
		if c, err := cli.Store.Contacts.GetContact(ctx(), p.JID); err == nil && c.Found {
			one["name"] = firstNonEmpty(c.FullName, c.PushName, c.BusinessName)
		}
		people = append(people, one)
	}
	return obj{"chat": info.JID.String(), "name": info.Name, "topic": info.Topic,
		"lid_addressing": info.AddressingMode == types.AddressingModeLID,
		"announce":       info.IsAnnounce, "participants": people}
}

func firstNonEmpty(xs ...string) string {
	for _, x := range xs {
		if x != "" {
			return x
		}
	}
	return ""
}

func groups() (any, error) {
	list, err := cli.GetJoinedGroups(ctx())
	if err != nil {
		return nil, err
	}
	out := []obj{}
	for _, g := range list {
		noteGroup(g)
		out = append(out, obj{"chat": g.JID.String(), "name": g.Name, "participants": len(g.Participants)})
	}
	return out, nil
}

func resolve(phones []string) (any, error) {
	if len(phones) == 0 {
		return []obj{}, nil
	}
	list, err := cli.IsOnWhatsApp(ctx(), phones)
	if err != nil {
		return nil, err
	}
	out := []obj{}
	for _, r := range list {
		out = append(out, obj{"query": r.Query, "jid": jidString(r.JID), "on_whatsapp": r.IsIn})
	}
	return out, nil
}

func contact(user string) (any, error) {
	j, err := parseJID(user)
	if err != nil {
		return nil, refused("%v", err)
	}
	c, err := cli.Store.Contacts.GetContact(ctx(), j)
	if err != nil {
		return nil, err
	}
	return obj{"jid": j.String(), "found": c.Found, "name": firstNonEmpty(c.FullName, c.FirstName),
		"push_name": c.PushName, "business_name": c.BusinessName}, nil
}

func history(a argReader) (any, error) {
	if archive == nil {
		return []obj{}, nil
	}
	limit := a.num("limit")
	if limit <= 0 || limit > 500 {
		limit = 50
	}
	before := a.num("before_ms")
	if before <= 0 {
		before = 1 << 62
	}
	chat := a.str("chat")
	if j, err := parseJID(chat); err == nil {
		chat = j.String()
	}
	rows, err := archive.Query(`SELECT id,sender_id,push_name,from_me,ts,kind,text,deleted FROM nc_messages
 WHERE chat=? AND ts<? ORDER BY ts DESC LIMIT ?`, chat, before, limit)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	out := []obj{}
	for rows.Next() {
		var id, sender, name, kind, text string
		var fromMe, deleted int
		var ts int64
		if rows.Scan(&id, &sender, &name, &fromMe, &ts, &kind, &text, &deleted) == nil {
			out = append(out, obj{"id": id, "from": sender, "push_name": name, "from_me": fromMe == 1,
				"timestamp_ms": ts, "kind": kind, "text": text, "deleted": deleted == 1})
		}
	}
	for i, j := 0, len(out)-1; i < j; i, j = i+1, j-1 {
		out[i], out[j] = out[j], out[i]
	}
	return out, nil
}

// --- main -------------------------------------------------------------------

func main() {
	db := flag.String("db", "", "the SQLite file the linked device's keys and the archive live in")
	media := flag.String("media", os.TempDir(), "where downloads land")
	level := flag.String("log-level", "info", "debug, info, warn or error")
	name := flag.String("device-name", "Nodecode", "the name the phone's Linked devices list shows")
	flag.BoolVar(&online, "online", true, "mark the account online while connected (typing shows; the phone stops notifying)")
	version := flag.Bool("version", false, "print the protocol version and exit")
	flag.Parse()
	if *version {
		fmt.Println("nodecode-whatsapp-driver 1")
		return
	}
	logLevel = map[string]int{"debug": 0, "info": 1, "warn": 2, "error": 3}[*level]
	if *db == "" {
		fmt.Fprintln(os.Stderr, "--db is required")
		os.Exit(2)
	}
	mediaDir = *media
	os.MkdirAll(filepath.Dir(*db), 0o700)
	os.MkdirAll(mediaDir, 0o700)

	store.SetOSInfo(*name, [3]uint32{0, 1, 0})
	store.DeviceProps.PlatformType = waCompanionReg.DeviceProps_DESKTOP.Enum()

	dsn := "file:" + *db + "?_pragma=foreign_keys(1)&_pragma=busy_timeout(10000)&_pragma=journal_mode(WAL)"
	logger := stderrLogger{module: "wa"}
	container, err := sqlstore.New(ctx(), "sqlite", dsn, logger.Sub("db"))
	if err != nil {
		logf("ERROR", "store: %v", err)
		event("fatal", obj{"error": "store: " + err.Error()})
		os.Exit(1)
	}
	os.Chmod(*db, 0o600)
	device, err := container.GetFirstDevice(ctx())
	if err != nil {
		event("fatal", obj{"error": "device: " + err.Error()})
		os.Exit(1)
	}
	archive, err = sql.Open("sqlite", dsn)
	if err == nil {
		_, err = archive.Exec(archiveSchema)
	}
	if err != nil {
		logf("WARN", "archive off: %v", err)
		archive = nil
	}

	cli = whatsmeow.NewClient(device, logger.Sub("client"))
	cli.AddEventHandler(onEvent)
	event("started", identity())
	if err := connect(); err != nil {
		event("connect_failure", obj{"reason": "connect", "message": err.Error()})
	}

	in := bufio.NewScanner(os.Stdin)
	in.Buffer(make([]byte, 1<<20), 64<<20)
	for in.Scan() {
		line := strings.TrimSpace(in.Text())
		if line == "" {
			continue
		}
		var req request
		if err := json.Unmarshal([]byte(line), &req); err != nil {
			emit(obj{"ok": false, "status": 400, "error": "bad request line: " + err.Error()})
			continue
		}
		go serve(req)
	}
	cli.Disconnect()
	if archive != nil {
		archive.Close()
	}
	container.Close()
}
