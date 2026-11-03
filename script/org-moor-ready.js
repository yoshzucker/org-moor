// org-moor-ready.js -- have single-file save a page once it is ready.
//
// Run in the page by --browser-script, with single-file told to take the
// page as loaded at DOMContentLoaded: rather than waiting for the network
// to go quiet, which a page that keeps fetching never does, or for the
// browser's InteractiveTime, five seconds of nothing after it is drawn,
// the capture single-file asks to begin is held until the page's own
// requests -- fetch and XMLHttpRequest -- are answered and its document
// has not changed for QUIET milliseconds.  A request open for LONG is a
// long poll or a stream, and is not waited for; after MAX the page is
// saved as it is.
if (window === window.top) {
  const QUIET = 300, LONG = 2000, MAX = 30000;
  const open = new Map();
  let seq = 0, last = performance.now();
  const begin = () => { const id = ++seq; open.set(id, performance.now()); return id; };
  const end = id => { open.delete(id); last = performance.now(); };
  const fetch = window.fetch;
  if (fetch) {
    window.fetch = function (...args) {
      const id = begin();
      return fetch.apply(this, args).finally(() => end(id));
    };
  }
  const send = XMLHttpRequest.prototype.send;
  XMLHttpRequest.prototype.send = function (...args) {
    const id = begin();
    this.addEventListener("loadend", () => end(id));
    return send.apply(this, args);
  };
  new MutationObserver(() => { last = performance.now(); })
    .observe(document, { subtree: true, childList: true, attributes: true, characterData: true });
  dispatchEvent(new CustomEvent("single-file-user-script-init"));
  addEventListener("single-file-on-before-capture-request", event => {
    event.preventDefault();
    const start = performance.now();
    const check = () => {
      const now = performance.now();
      const waiting = [...open.values()].some(since => now - since < LONG);
      if ((!waiting && now - last >= QUIET) || now - start >= MAX) {
        dispatchEvent(new CustomEvent("single-file-on-before-capture-response"));
      } else {
        setTimeout(check, 50);
      }
    };
    check();
  });
}
