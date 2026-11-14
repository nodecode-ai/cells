// Private subprocess transport. stdout carries credentials ONLY to Nodecode's
// credential resolver, never to a model or a user-facing diagnostic. Do not run
// this helper by hand against a real store. Tests use synthetic stores only.
import { open, realpath, readFile, access } from 'node:fs/promises';
import { homedir } from 'node:os';
import { join, dirname, delimiter, resolve } from 'node:path';
import { pathToFileURL } from 'node:url';
import { createRequire } from 'node:module';

export function piAuthPath(env = process.env, home = homedir()) {
  const configured = env.PI_CODING_AGENT_DIR?.trim();
  const directory = configured === '~' ? home : configured?.startsWith('~/')
    ? join(home, configured.slice(2)) : configured || join(home, '.pi/agent');
  return join(directory, 'auth.json');
}

function token(value) {
  if (typeof value !== 'string' || !value.trim() || /[\x00-\x20\x7f]/.test(value))
    throw Error('Invalid credential');
  return value;
}

function oauthProviderForAccount(account) {
  // The suffix selects an exact multi-pass store entry. Strip it ONLY for
  // OAuth-flow/wire dispatch, never for reads, refresh persistence or fallback.
  token(account);
  if (/^openai-codex(?:-[1-9][0-9]*)?$/.test(account)) return 'openai-codex';
  if (account === 'openai') return 'openai';
  throw Error('Unsupported OAuth provider');
}

async function readStore(path) {
  // Bound the read itself, not merely the decoded string. No parser error or
  // filesystem exception is allowed to escape through the CLI's diagnostics.
  const file = await open(path, 'r');
  try {
    const buffer = Buffer.alloc(1048577);
    let length = 0;
    while (length < buffer.length) {
      const { bytesRead } = await file.read(buffer, length, buffer.length - length, null);
      if (!bytesRead) break;
      length += bytesRead;
    }
    if (length > 1048576) throw Error('Store too large');
    const store = JSON.parse(buffer.subarray(0, length).toString('utf8'));
    if (!store || typeof store !== 'object' || Array.isArray(store)) throw Error('Invalid store');
    return store;
  } finally { await file.close(); }
}

// Locate only installed program code, not another credential store. An explicit
// package root also supports Pi installations whose launcher is a wrapper.
async function piPackageRoot() {
  if (process.env.NODECODE_PI_PACKAGE_DIR) return resolve(process.env.NODECODE_PI_PACKAGE_DIR);
  for (const dir of (process.env.PATH || '').split(delimiter)) {
    try {
      let root = dirname(await realpath(join(dir, process.platform === 'win32' ? 'pi.cmd' : 'pi')));
      for (let i = 0; i < 5; i++, root = dirname(root)) {
        try {
          const pkg = JSON.parse(await readFile(join(root, 'package.json'), 'utf8'));
          if (pkg.name === '@earendil-works/pi-coding-agent') return root;
        } catch { /* not the package root */ }
      }
    } catch { /* try the next PATH entry */ }
  }
  throw Error('Pi package not found');
}

export async function withPiReadLock(path, fn, wait = true) {
  // Pi writes in place under proper-lockfile, so even a bounded read must
  // participate in its lock protocol. A read never creates the store.
  await access(path);
  const root = await piPackageRoot();
  const lockfile = createRequire(join(root, 'package.json'))('proper-lockfile');
  const release = await lockfile.lock(path, { realpath: false, stale: 30000,
    // A Pi refresh may hold the lock for 15 seconds; match Pi's 30-second
    // lock-wait window rather than failing during an ordinary slow refresh.
    retries: wait ? { retries: 30, factor: 1, minTimeout: 1000, maxTimeout: 1000 } : 0 });
  try { return await fn(); } finally { await release(); }
}

export async function refreshWithPi(provider, path) {
  const baseProvider = oauthProviderForAccount(provider);
  // Pi owns locking, refresh-token rotation and persistence. Never copy
  // the grant into Nodecode or independently write a refreshed auth.json.
  const root = await piPackageRoot();
  const ai = join(root, 'node_modules/@earendil-works/pi-ai/dist/auth');
  const load = (file) => import(pathToFileURL(file).href);
  const [{ AuthStorage }, { refreshStoredOAuthCredential }, module] = await Promise.all([
    load(join(root, 'dist/core/auth-storage.js')),
    load(join(ai, 'resolve.js')),
    load(join(ai, 'oauth', baseProvider === 'openai' ? 'openai-chatgpt.js' : 'openai-codex.js')),
  ]);
  // AuthStorage.create can create a missing store: explicitly refuse that here.
  await access(path);
  const oauth = baseProvider === 'openai' ? module.openaiChatGPTOAuth : module.openaiCodexOAuth;
  return refreshStoredOAuthCredential(AuthStorage.create(path), provider, oauth,
    (credential) => Date.now() + 300000 >= credential.expires,
    AbortSignal.timeout(30000));
}

function apiKey(entry, env) {
  if (typeof entry.key !== 'string' || entry.key.trimStart().startsWith('!'))
    throw Error('Credential commands and env-only entries are not supported');
  // Deliberately support literals and single environment references only.
  // Never execute a command found in the foreign credential store.
  const match = /^(?:\$([A-Za-z_][A-Za-z0-9_]*)|\$\{([A-Za-z_][A-Za-z0-9_]*)\})$/.exec(entry.key);
  if (match) return token(entry.env?.[match[1] || match[2]] ?? env[match[1] || match[2]]);
  if (entry.key.includes('$')) throw Error('Unsupported key expression');
  return token(entry.key);
}

function codexAccountId(entry) {
  // One identity for transport and account-scoped state. Opaque legacy tokens
  // need stored metadata; a JWT and stored metadata must not disagree.
  let claimed;
  try {
    claimed = JSON.parse(Buffer.from(entry.access.split('.')[1] || '', 'base64url')
      .toString('utf8'))['https://api.openai.com/auth']?.chatgpt_account_id;
  } catch { /* an opaque legacy access token */ }
  if (claimed) {
    token(claimed);
    if (entry.accountId !== undefined && token(entry.accountId) !== claimed)
      throw Error('Inconsistent Codex account identity');
    return claimed;
  }
  return token(entry.accountId);
}

function validateOAuth(entry, provider) {
  if (entry?.type !== 'oauth' || !Number.isFinite(entry.expires)) throw Error('Invalid OAuth');
  token(entry.access);
  token(entry.refresh);
  const baseProvider = oauthProviderForAccount(provider);
  if (baseProvider === 'openai-codex') codexAccountId(entry);
  if (baseProvider === 'openai' && (!Array.isArray(entry.scopes) ||
      !entry.scopes.includes('chatgpt.tokens.use.direct') || !entry.clientId))
    throw Error('Reconnect ChatGPT direct login in Pi');
}

export async function resolvePiCredential(request, {
  refresh = refreshWithPi, env = process.env, withLock = withPiReadLock,
} = {}) {
  const { provider, probe = false, discover = false } = request;
  if (!discover && (typeof provider !== 'string' || !provider)) throw Error('Provider required');
  const path = resolve(request.path || piAuthPath(env));
  const store = await withLock(path, () => readStore(path), !discover);
  if (discover) {
    // Inventory is presence only. Never return grants, email, JWT claims or
    // account IDs, refresh anything, or wait behind a long-running writer.
    return { accounts: Object.keys(store).filter((key) =>
      /^openai-codex(?:-[1-9][0-9]*)?$/.test(key) && store[key]?.type === 'oauth').sort() };
  }
  let entry = Object.hasOwn(store, provider) ? store[provider] : undefined;
  if (entry?.type === 'api_key') {
    const key = apiKey(entry, env);
    return probe ? { type: 'api_key', available: true } : { key, type: 'api_key' };
  }
  validateOAuth(entry, provider);
  if (probe) return { type: 'oauth', available: true };
  if (Date.now() + 300000 >= entry.expires) entry = await refresh(provider, path);
  validateOAuth(entry, provider);
  if (Date.now() >= entry.expires) throw Error('Expired OAuth');
  const result = { key: entry.access, type: 'oauth' };
  if (oauthProviderForAccount(provider) === 'openai-codex') {
    result.accountId = codexAccountId(entry);
  }
  return result;
}

if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  try {
    let input = '';
    for await (const chunk of process.stdin) {
      input += chunk;
      if (input.length > 16384) throw Error('Request too large');
    }
    const result = await resolvePiCredential(JSON.parse(input));
    process.stdout.write(JSON.stringify(result));
  } catch {
    // Do NOT print exception objects, JSON snippets, key values or OAuth HTTP
    // bodies. The parent also discards stderr; this remains safe in isolation.
    process.stderr.write('Pi authentication unavailable; check the Pi login and integration setup.\n');
    process.exitCode = 1;
  }
}
