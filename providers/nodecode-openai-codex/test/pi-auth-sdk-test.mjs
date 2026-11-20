// Opt-in installed-Pi compatibility proof. Only synthetic stores are opened;
// global fetch is replaced before any refresh and cannot reach the network.
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, writeFile, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createRequire } from 'node:module';
import { setTimeout as sleep } from 'node:timers/promises';
import { resolvePiCredential, refreshWithPi, withPiReadLock } from '../pi-auth.mjs';

const root = process.env.NODECODE_PI_PACKAGE_DIR;
const jwt = `header.${Buffer.from(JSON.stringify({ 'https://api.openai.com/auth':
  { chatgpt_account_id: 'synthetic-account' } })).toString('base64url')}.signature`;

for (const provider of ['openai-codex', 'openai', 'openai-codex-2', 'openai-codex-3']) {
  test(`installed Pi refreshes and persists ${provider} under its lock`, { skip: !root }, async () => {
    const dir = await mkdtemp(join(tmpdir(), 'nodecode-pi-sdk-'));
    const path = join(dir, 'auth.json');
    const fetch = globalThis.fetch;
    let requests = 0;
    try {
      const other = provider === 'openai-codex' ? 'openai-codex-2' : 'openai-codex';
      const untouched = { type: 'oauth', access: 'synthetic-untouched',
        refresh: 'synthetic-untouched-refresh', expires: 1, accountId: 'other-account' };
      await writeFile(path, JSON.stringify({ preserved: { type: 'api_key', key: 'synthetic-other' },
        [other]: untouched,
        [provider]: { type: 'oauth', access: jwt, refresh: 'synthetic-old-refresh', expires: 1,
          accountId: 'synthetic-account', clientId: 'synthetic-issued-client',
          scopes: ['chatgpt.tokens.use.direct'] } }), { mode: 0o600 });
      globalThis.fetch = async (url, init) => {
        requests++;
        assert.equal(String(url), provider === 'openai' ?
          'https://auth.openai.com/api/accounts/oauth/token' : 'https://auth.openai.com/oauth/token');
        assert.equal(init.method, 'POST');
        assert.equal(init.body.get('refresh_token'), 'synthetic-old-refresh');
        return new Response(JSON.stringify({ access_token: jwt,
          refresh_token: 'synthetic-rotated-refresh', expires_in: 3600,
          scope: 'chatgpt.tokens.use.direct' }), { status: 200 });
      };
      const results = await Promise.all(Array.from({ length: 3 },
        () => resolvePiCredential({ provider, path })));
      const result = results[0];
      assert.ok(results.every((entry) => entry.key === jwt));
      assert.equal(requests, 1, 'concurrent callers must share the refreshed grant');
      assert.equal(result.key, jwt);
      assert.ok(!JSON.stringify(result).includes('refresh'));
      const saved = JSON.parse(await readFile(path, 'utf8'));
      assert.equal(saved[provider].refresh, 'synthetic-rotated-refresh');
      assert.equal(saved.preserved.key, 'synthetic-other');
      assert.deepEqual(saved[other], untouched, 'another account must remain equivalent');
      assert.ok(saved[provider].expires > Date.now());
      await resolvePiCredential({ provider, path });
      assert.equal(requests, 1, 'fresh credentials must not refresh again');
    } finally {
      globalThis.fetch = fetch;
      await rm(dir, { recursive: true });
    }
  });
}

test('logout after a numbered read is not resurrected by refresh', { skip: !root }, async () => {
  const dir = await mkdtemp(join(tmpdir(), 'nodecode-pi-logout-'));
  const path = join(dir, 'auth.json');
  const fetch = globalThis.fetch;
  try {
    const base = { type: 'oauth', access: jwt, refresh: 'synthetic-base',
      expires: Date.now() + 3600000 };
    await writeFile(path, JSON.stringify({ 'openai-codex': base,
      'openai-codex-2': { type: 'oauth', access: jwt,
        refresh: 'synthetic-second', expires: 1 } }), { mode: 0o600 });
    globalThis.fetch = async () => { assert.fail('logged-out account must not refresh'); };
    await assert.rejects(resolvePiCredential({ provider: 'openai-codex-2', path }, {
      refresh: async (provider, file) => {
        await withPiReadLock(file, async () => {
          const data = JSON.parse(await readFile(file, 'utf8'));
          delete data[provider];
          await writeFile(file, JSON.stringify(data));
        });
        return refreshWithPi(provider, file);
      },
    }));
    assert.deepEqual(JSON.parse(await readFile(path, 'utf8')), { 'openai-codex': base });
  } finally {
    globalThis.fetch = fetch;
    await rm(dir, { recursive: true });
  }
});

test('initial read waits for a Pi writer instead of parsing its partial file', { skip: !root }, async () => {
  const dir = await mkdtemp(join(tmpdir(), 'nodecode-pi-lock-'));
  const path = join(dir, 'auth.json');
  const lockfile = createRequire(join(root, 'package.json'))('proper-lockfile');
  let release;
  try {
    await writeFile(path, '{}', { mode: 0o600 });
    release = await lockfile.lock(path, { realpath: false });
    await writeFile(path, '{partial synthetic data');
    let settled = false;
    const pending = resolvePiCredential({ provider: 'p', path }).finally(() => { settled = true; });
    await sleep(80);
    assert.equal(settled, false);
    await writeFile(path, JSON.stringify({ p: { type: 'api_key', key: 'synthetic-key' } }));
    await release();
    release = undefined;
    assert.equal((await pending).key, 'synthetic-key');
  } finally {
    if (release) await release();
    await rm(dir, { recursive: true });
  }
});

test('a staggered reader waits through a slow Pi refresh', { skip: !root }, async () => {
  const dir = await mkdtemp(join(tmpdir(), 'nodecode-pi-slow-'));
  const path = join(dir, 'auth.json');
  const fetch = globalThis.fetch;
  let started;
  const refreshing = new Promise((resolve) => { started = resolve; });
  let requests = 0;
  let first;
  try {
    await writeFile(path, JSON.stringify({ 'openai-codex': { type: 'oauth',
      access: jwt, refresh: 'synthetic-refresh', expires: 1 } }), { mode: 0o600 });
    globalThis.fetch = async (url) => {
      assert.equal(String(url), 'https://auth.openai.com/oauth/token');
      requests++;
      started();
      await sleep(6000); // Longer than the original 4.27-second retry window.
      return new Response(JSON.stringify({ access_token: jwt,
        refresh_token: 'synthetic-rotated', expires_in: 3600 }), { status: 200 });
    };
    first = resolvePiCredential({ provider: 'openai-codex', path });
    await refreshing;
    const second = resolvePiCredential({ provider: 'openai-codex', path });
    const results = await Promise.all([first, second]);
    assert.ok(results.every((entry) => entry.key === jwt));
    assert.equal(requests, 1);
  } finally {
    if (first) await first.catch(() => {});
    globalThis.fetch = fetch;
    await rm(dir, { recursive: true });
  }
});
