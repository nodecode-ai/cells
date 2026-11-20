import { test, after } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, writeFile, readFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { spawnSync } from 'node:child_process';
import { resolvePiCredential as resolveCredential, piAuthPath } from '../pi-auth.mjs';
import { fileURLToPath } from 'node:url';

// Pure reader tests don't require an installed Pi. The separate SDK tests
// exercise the actual installed locking and refresh implementations.
const resolvePiCredential = (request, options) => resolveCredential(request,
  { withLock: (_path, fn) => fn(), ...options });
const directories = [];
after(async () => { await Promise.all(directories.map((dir) => rm(dir, { recursive: true }))); });

const secret = 'synthetic-api-key-123456789';
const oauth = (extra = {}) => ({ type: 'oauth', access: 'synthetic-access',
  refresh: 'synthetic-refresh', expires: Date.now() + 3600000, ...extra });
const fixture = async (data) => {
  const dir = await mkdtemp(join(tmpdir(), 'nodecode-pi-test-'));
  directories.push(dir);
  const path = join(dir, 'auth.json');
  await writeFile(path, typeof data === 'string' ? data : JSON.stringify(data), { mode: 0o600 });
  return path;
};

test('Pi directory override and default are resolved without reading files', () => {
  assert.equal(piAuthPath({}, '/synthetic-home'), '/synthetic-home/.pi/agent/auth.json');
  assert.equal(piAuthPath({ PI_CODING_AGENT_DIR: '~/elsewhere' }, '/synthetic-home'),
    '/synthetic-home/elsewhere/auth.json');
});

test('discovery returns exact Codex OAuth keys only, never grants or account metadata', async () => {
  const path = await fixture({
    'openai-codex-3': oauth({ accountId: 'synthetic-private-account' }),
    'openai-codex': oauth({ expires: 1 }),
    'openai-codex-2': { type: 'api_key', key: secret },
    'openai-codex-02': oauth(), 'openai-codex-work': oauth(),
    openai: oauth(), anthropic: { type: 'api_key', key: '!do-not-run' },
  });
  const before = await readFile(path, 'utf8');
  let locked = 0;
  const result = await resolvePiCredential({ discover: true, path }, {
    withLock: async (file, fn, wait) => {
      assert.equal(file, path);
      assert.equal(wait, false, 'inventory must not wait behind a refresh');
      locked++;
      return fn();
    },
    refresh: () => assert.fail('discovery must not refresh'),
  });
  assert.deepEqual(result, { accounts: ['openai-codex', 'openai-codex-3'] });
  assert.equal(locked, 1);
  assert.equal(await readFile(path, 'utf8'), before);
  assert.ok(!JSON.stringify(result).includes('synthetic'));
});

test('numbered-only discovery does not invent a base account', async () => {
  const path = await fixture({ 'openai-codex-2': oauth() });
  assert.deepEqual(await resolvePiCredential({ discover: true, path }),
    { accounts: ['openai-codex-2'] });
});

test('literal and environment API keys resolve without copying or changing the store', async () => {
  const path = await fixture({ openrouter: { type: 'api_key', key: secret },
    custom: { type: 'api_key', key: '${TEST_KEY}', env: { TEST_KEY: secret } } });
  const before = await readFile(path, 'utf8');
  assert.deepEqual(await resolvePiCredential({ provider: 'openrouter', path }),
    { key: secret, type: 'api_key' });
  assert.equal((await resolvePiCredential({ provider: 'custom', path })).key, secret);
  assert.equal(await readFile(path, 'utf8'), before);
});

test('probe returns metadata only and never refreshes or executes commands', async () => {
  const path = await fixture({ 'openai-codex': oauth({ expires: 1, accountId: 'account' }),
    command: { type: 'api_key', key: '!echo should-never-run' } });
  const refresh = () => assert.fail('probe refreshed credentials');
  const result = await resolvePiCredential({ provider: 'openai-codex', path, probe: true }, { refresh });
  assert.deepEqual(result, { type: 'oauth', available: true });
  assert.ok(!JSON.stringify(result).includes('synthetic'));
  await assert.rejects(resolvePiCredential({ provider: 'command', path }));
});

test('Codex credentials include only access token and account ID, never refresh token', async () => {
  const path = await fixture({ 'openai-codex': oauth({ accountId: 'synthetic-account' }) });
  assert.deepEqual(await resolvePiCredential({ provider: 'openai-codex', path }),
    { key: 'synthetic-access', type: 'oauth', accountId: 'synthetic-account' });
});

test('Codex account ID can be derived from the access JWT', async () => {
  const payload = Buffer.from(JSON.stringify({ 'https://api.openai.com/auth':
    { chatgpt_account_id: 'jwt-account' } })).toString('base64url');
  const path = await fixture({ 'openai-codex': oauth({ access: `header.${payload}.signature` }) });
  assert.equal((await resolvePiCredential({ provider: 'openai-codex', path })).accountId, 'jwt-account');
});

test('conflicting stored and JWT account identities fail in both probes and requests', async () => {
  const payload = Buffer.from(JSON.stringify({ 'https://api.openai.com/auth':
    { chatgpt_account_id: 'jwt-account' } })).toString('base64url');
  const path = await fixture({ 'openai-codex-2': oauth({
    access: `header.${payload}.signature`, accountId: 'different-account' }) });
  await assert.rejects(resolvePiCredential({ provider: 'openai-codex-2', path }));
  await assert.rejects(resolvePiCredential({ provider: 'openai-codex-2', path, probe: true }));
});

test('numbered Codex accounts select only their own credential and account ID', async () => {
  const path = await fixture(Object.fromEntries(['openai-codex', 'openai-codex-2', 'openai-codex-3']
    .map((name) => [name, oauth({ access: `synthetic-${name}`, accountId: `account-${name}` })])));
  for (const provider of ['openai-codex-3', 'openai-codex', 'openai-codex-2']) {
    assert.deepEqual(await resolvePiCredential({ provider, path }),
      { key: `synthetic-${provider}`, type: 'oauth', accountId: `account-${provider}` });
  }
});

test('a numbered-only store works without a base credential', async () => {
  const path = await fixture({ 'openai-codex-2': oauth({ accountId: 'only-account' }) });
  assert.equal((await resolvePiCredential({ provider: 'openai-codex-2', path })).accountId, 'only-account');
  assert.deepEqual(await resolvePiCredential({ provider: 'openai-codex-2', path, probe: true }),
    { type: 'oauth', available: true });
  await assert.rejects(resolvePiCredential({ provider: 'openai-codex', path }));
});

test('probes reject unusable Codex account identity without returning or refreshing secrets', async () => {
  for (const provider of ['openai-codex', 'openai-codex-2']) {
    for (const extra of [{}, { accountId: 'bad\naccount' }]) {
      const path = await fixture({ [provider]: oauth(extra) });
      await assert.rejects(resolvePiCredential({ provider, path, probe: true }, {
        refresh: async () => assert.fail('probe must not refresh'),
      }));
    }
  }
});

test('missing or malformed numbered accounts never borrow the base or environment', async () => {
  const path = await fixture({ 'openai-codex': oauth({ accountId: 'base-account' }),
    'openai-codex-3': { type: 'oauth', access: 'broken' },
    'openai-codex-0': oauth({ accountId: 'invalid-alias' }),
    'openai-codex-02': oauth({ accountId: 'invalid-alias' }),
    'openai-codex-work': oauth({ accountId: 'invalid-alias' }) });
  for (const provider of ['openai-codex-2', 'openai-codex-3', 'openai-codex-0',
    'openai-codex-02', 'openai-codex-work']) {
    await assert.rejects(resolvePiCredential({ provider, path }, { env: { OPENAI_API_KEY: secret } }));
  }
});

test('numbered refresh delegates the exact storage key, not the base flow name', async () => {
  const path = await fixture({ 'openai-codex-2': oauth({ expires: 1, accountId: 'old-account' }) });
  const result = await resolvePiCredential({ provider: 'openai-codex-2', path }, {
    refresh: async (provider, file) => {
      assert.equal(provider, 'openai-codex-2');
      assert.equal(file, path);
      return oauth({ access: 'synthetic-account-2-rotated', accountId: 'account-2' });
    },
  });
  assert.deepEqual(result,
    { key: 'synthetic-account-2-rotated', type: 'oauth', accountId: 'account-2' });
});

test('direct ChatGPT OAuth is distinct from Codex', async () => {
  const path = await fixture({ openai: oauth({ clientId: 'issued-client',
    scopes: ['chatgpt.tokens.use.direct'] }) });
  assert.deepEqual(await resolvePiCredential({ provider: 'openai', path }),
    { key: 'synthetic-access', type: 'oauth' });
});

test('near-expiry OAuth delegates refresh and uses the rotated result', async () => {
  const path = await fixture({ 'openai-codex': oauth({ expires: 1, accountId: 'old-account' }) });
  let calls = 0;
  const refresh = async (provider, file) => {
    assert.equal(provider, 'openai-codex');
    assert.equal(file, path);
    calls++;
    return oauth({ access: 'rotated-access', accountId: 'new-account' });
  };
  const result = await resolvePiCredential({ provider: 'openai-codex', path }, { refresh });
  assert.equal(calls, 1);
  assert.equal(result.key, 'rotated-access');
  assert.equal(result.accountId, 'new-account');
  await assert.rejects(resolvePiCredential({ provider: 'openai-codex', path },
    { refresh: async () => { throw Error('synthetic refresh failure'); } }));
});

test('missing, corrupt, oversized, invalid and unsupported entries fail closed', async () => {
  for (const data of ['{bad JSON synthetic-secret', '[]', ' '.repeat(1048577),
    {}, { p: { type: 'oauth', access: 'synthetic', refresh: 'synthetic', expires: 'bad' } },
    { p: { type: 'api_key', key: 'line\nbreak' } },
    { p: { type: 'api_key', key: '$MISSING_PI_TEST_VARIABLE' } },
    { p: oauth() }, { p: { type: 'api_key', env: { K: secret } } }]) {
    const path = await fixture(data);
    await assert.rejects(resolvePiCredential({ provider: 'p', path }));
  }
  const path = await fixture({ openai: oauth() });
  await assert.rejects(resolvePiCredential({ provider: 'openai', path }));
});

test('CLI errors contain no credential contents or parser excerpts', async () => {
  const path = await fixture('{bad JSON synthetic-secret');
  const result = spawnSync(process.execPath, [fileURLToPath(new URL('../pi-auth.mjs', import.meta.url))], {
    input: JSON.stringify({ provider: 'openai', path }), encoding: 'utf8' });
  assert.notEqual(result.status, 0);
  assert.equal(result.stdout, '');
  assert.equal(result.stderr, 'Pi authentication unavailable; check the Pi login and integration setup.\n');
});
