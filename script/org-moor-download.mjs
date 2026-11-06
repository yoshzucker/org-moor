// org-moor-download.mjs -- have a browser fetch a URL, as it would open it.
//
//   node org-moor-download.mjs http://127.0.0.1:PORT URL FILE
//
// Opens URL in a tab of the browser listening on its debugging port at
// that address, signed in as it is, and takes what the tab is sent before
// the browser shows it or downloads it.  A file -- anything but a page --
// is written to FILE, and printed are `file', its status, its Content-Type
// and its Content-Disposition, a line each.  A page is not loaded: printed
// are `page' and the URL it was sent from, after any redirect -- a sign-in
// page's, where the browser is not signed in.  Exits non-zero, saying why,
// when the URL cannot be opened or answers with an error.
import { createWriteStream } from "node:fs";

const [server, url, file] = process.argv.slice(2);
if (!server || !url || !file) {
  console.error("usage: node org-moor-download.mjs http://127.0.0.1:PORT URL FILE");
  process.exit(2);
}

const version = await (await fetch(new URL("/json/version", server))).json();
const socket = new WebSocket(version.webSocketDebuggerUrl);
const pending = new Map();
const listeners = [];
let lastId = 0;

function send(method, params = {}, sessionId = undefined) {
  const id = ++lastId;
  socket.send(JSON.stringify({ id, method, params, sessionId }));
  return new Promise((resolve, reject) => pending.set(id, { resolve, reject, method }));
}

socket.onmessage = (event) => {
  const message = JSON.parse(event.data);
  const waiting = message.id && pending.get(message.id);
  if (waiting) {
    pending.delete(message.id);
    if (message.error) waiting.reject(new Error(`${waiting.method}: ${message.error.message}`));
    else waiting.resolve(message.result);
  } else {
    for (const listener of listeners) listener(message);
  }
};
await new Promise((resolve, reject) => {
  socket.onopen = resolve;
  socket.onerror = () => reject(new Error("the browser did not answer"));
});

// In a tab of its own, behind any other: single-file draws only the tab
// in front.
const { targetId } = await send("Target.createTarget", { url: "about:blank", background: true });
try {
  const { sessionId } = await send("Target.attachToTarget", { targetId, flatten: true });
  // Once what the tab was sent is taken, the navigation's failing is ours.
  let decided = false;

  // What the tab's own document is sent, paused once its headers are in.
  // Its frame is the tab's, by id; a frame within it is let be, as is a
  // redirect, which the browser follows.
  async function paused({ requestId, frameId, request, responseStatusCode: status,
                          responseHeaders = [] }) {
    if (frameId !== targetId || status === undefined || (status >= 300 && status < 400)) {
      await send("Fetch.continueRequest", { requestId }, sessionId);
      return null;
    }
    const header = (name) =>
      responseHeaders.find((h) => h.name.toLowerCase() === name)?.value ?? "";
    const type = header("content-type");
    decided = true;
    if (/^\s*(text\/html|application\/xhtml\+xml)/i.test(type)) {
      await send("Fetch.failRequest", { requestId, errorReason: "Aborted" }, sessionId);
      return ["page", request.url];
    }
    if (status >= 400) {
      await send("Fetch.failRequest", { requestId, errorReason: "Aborted" }, sessionId);
      throw new Error(`${request.url} answered ${status}`);
    }
    const { stream } = await send("Fetch.takeResponseBodyAsStream", { requestId }, sessionId);
    const out = createWriteStream(file);
    for (;;) {
      const { data, base64Encoded, eof } =
        await send("IO.read", { handle: stream, size: 1 << 20 }, sessionId);
      out.write(Buffer.from(data, base64Encoded ? "base64" : "utf8"));
      if (eof) break;
    }
    await send("IO.close", { handle: stream }, sessionId);
    await new Promise((resolve, reject) => out.end((err) => (err ? reject(err) : resolve())));
    // Taken, it is not to be shown, nor downloaded.
    await send("Fetch.failRequest", { requestId, errorReason: "Aborted" }, sessionId);
    return ["file", String(status), type, header("content-disposition")];
  }

  const taken = new Promise((resolve, reject) => {
    listeners.push((message) => {
      if (message.sessionId === sessionId && message.method === "Fetch.requestPaused") {
        paused(message.params).then((got) => {
          if (got) resolve(got);
        }, reject);
      }
    });
    // A URL that cannot be opened at all: one that was taken is aborted, too.
    send("Fetch.enable", { patterns: [{ resourceType: "Document", requestStage: "Response" }] },
         sessionId)
      .then(() => send("Page.navigate", { url }, sessionId))
      .then(({ errorText }) => {
        if (errorText && !decided) reject(new Error(`${url}: ${errorText}`));
      }, reject);
  });
  process.stdout.write((await taken).join("\n") + "\n");
} catch (err) {
  console.error(err.message);
  process.exitCode = 1;
} finally {
  await send("Target.closeTarget", { targetId }).catch(() => {});
  socket.close();
}
