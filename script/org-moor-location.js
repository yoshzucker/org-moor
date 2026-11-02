// org-moor-location.js -- say in a page single-file saves where it was.
//
// single-file writes the URL it was given into the page it saves, not the
// one the browser was sent on to -- a sign-in page, when the browser is not
// signed in.  Run in the page by --browser-script, this writes that one, as
// <meta name="org-moor-location">, for org-moor to tell a sign-in page from
// the page.
if (window === window.top) {
  document.addEventListener("DOMContentLoaded", () => {
    const meta = document.createElement("meta");
    meta.name = "org-moor-location";
    meta.content = location.href;
    document.head.appendChild(meta);
  });
}
