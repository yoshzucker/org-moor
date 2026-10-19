;;; org-moor-take.el --- Bring things in  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Taking is the step that happens when you meet a thing, which is usually
;; outside Emacs, so every command here asks as little as it can: a title,
;; and where it goes.  Where it goes is anywhere: point when you are in Org,
;; else a heading you pick from your refile targets, else
;; `org-moor-inbox-file'.  What goes there is a link to the source, on a
;; line of its own; the source itself goes in that file's sources
;; directory, and no heading is made for it.

;;; Code:

(require 'org-moor-core)
(require 'org-refile)
(require 'url-parse)
(require 'url-util)

(defvar org-protocol-protocol-alist)

(defcustom org-moor-inbox-file nil
  "File a thing goes to when there is no heading to take it to.
Nil means `org-default-notes-file'."
  :type '(choice (const :tag "org-default-notes-file" nil) file)
  :group 'org-moor)

(defun org-moor--inbox-buffer ()
  "Return the buffer of the inbox file."
  (find-file-noselect (or org-moor-inbox-file org-default-notes-file)))

(defvar org-moor-heading-history nil
  "Headings picked for taking, newest first.")

(defun org-moor--targets ()
  "Return the refile targets, read from the inbox when not in Org.
Each is (NAME FILE REGEXP POS); POS is nil for a file as a whole."
  (with-current-buffer (if (derived-mode-p 'org-mode)
                           (current-buffer)
                         (org-moor--inbox-buffer))
    (org-refile-get-targets)))

(defun org-moor--target-at (entry)
  "Return the target for refile ENTRY: a marker, or its file for the top."
  (if (nth 3 entry)
      (with-current-buffer (find-file-noselect (nth 1 entry))
        (copy-marker (nth 3 entry)))
    (nth 1 entry)))

(defun org-moor-read-heading (&optional prompt)
  "Read a heading to take to, existing or new, and return the target.
A name from the refile targets is that heading.  PARENT/NAME, where
PARENT is one, is a new heading NAME under it.  Any other name is a new
heading at the top of the inbox.  Nothing is the inbox itself.

The target is a marker at a heading, a file (its top level), or `inbox'."
  (let* ((table (org-moor--targets))
         (answer (string-trim
                  (completing-read
                   (or prompt "Take to (new name creates): ")
                   table nil nil nil 'org-moor-heading-history))))
    (cond
     ((string-empty-p answer) 'inbox)
     ((assoc answer table) (org-moor--target-at (assoc answer table)))
     ((and (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answer)
           (assoc (match-string 1 answer) table))
      (org-moor-insert-heading
       (org-moor--target-at (assoc (match-string 1 answer) table))
       (match-string 2 answer)))
     (t (org-moor-insert-heading 'inbox answer)))))

(defun org-moor--target (pick)
  "Return where a taken thing goes: a marker, a file, or `inbox'.
PICK may itself be a target, which is used as it is.  Otherwise at point
when in Org and PICK is nil, or read, see `org-moor-read-heading'.
The end of a file is not a place of its own, since it is inside the
last heading there; `inbox' is the inbox file's top level."
  (cond ((markerp pick) (copy-marker pick))
        ((or (stringp pick) (eq pick 'inbox)) pick)
        ((and (not pick) (derived-mode-p 'org-mode)) (point-marker))
        (t (org-moor-read-heading))))

(defun org-moor--release (target)
  "Let go of TARGET when it is a marker."
  (when (markerp target) (set-marker target nil)))

(defun org-moor--end-of-own-text (pos)
  "Return the end of the text of the heading at POS, before its first child."
  (save-excursion
    (goto-char pos)
    (org-back-to-heading t)
    (outline-next-heading)
    (skip-chars-backward " \t\n")
    (point)))

(defun org-moor--end-of-entry ()
  "Move to the line after this heading's own text, before its first child."
  (goto-char (org-moor--end-of-own-text (point)))
  (unless (eobp) (forward-line 1))
  (unless (bolp) (insert "\n")))

(defun org-moor--next-line ()
  "Move to the start of the line after the one point is on, unless at one.
At the end of the buffer, the line is made."
  (unless (bolp)
    (end-of-line)
    (if (eobp) (insert "\n") (forward-char 1))))

(defun org-moor-insert-heading (target title)
  "Insert a heading titled TITLE at TARGET; return its marker.
Under the heading TARGET is in, as its last child.  At the top level, at
the end of the file, when TARGET is before the first heading, a file, or
`inbox'."
  (with-current-buffer (cond ((markerp target) (marker-buffer target))
                             ((stringp target) (find-file-noselect target))
                             (t (org-moor--inbox-buffer)))
    (org-with-wide-buffer
     (let ((level (if (not (markerp target))
                      0
                    (goto-char target)
                    (if (org-before-first-heading-p)
                        0
                      (org-back-to-heading t)
                      (org-current-level)))))
       (if (zerop level)
           (goto-char (point-max))
         (org-end-of-subtree t t))
       (unless (bolp) (insert "\n"))
       (let ((start (point)))
         (insert (make-string (1+ level) ?*) " " title "\n")
         (copy-marker start))))))

(define-obsolete-function-alias 'org-moor-insert-source-heading
  #'org-moor-insert-heading "0.2")

(defun org-moor--writable (marker)
  "Return MARKER, or where to write instead when its heading is out.
A heading taken out by org-dock is written in its copy, where it can
be; the marker returned is at the same heading there."
  (with-current-buffer (marker-buffer marker)
    (if-let* ((copy (org-dock-copy-of marker)))
        (let ((line (save-excursion
                      (goto-char marker)
                      (org-back-to-heading t)
                      (buffer-substring-no-properties (point) (line-end-position)))))
          (with-current-buffer copy
            (save-excursion
              (goto-char (point-min))
              (copy-marker (if (search-forward line nil t)
                               (line-beginning-position)
                             (point-min))))))
      marker)))

(defun org-moor--target-sources (target)
  "Return the sources directory of the file TARGET is in.
TARGET is as `org-moor--target' returns it."
  (cond ((markerp target)
         (with-current-buffer (marker-buffer target) (org-moor-sources)))
        ((stringp target) (org-moor-sources (expand-file-name target)))
        (t (org-moor-sources
            (expand-file-name (or org-moor-inbox-file org-default-notes-file))))))

(defun org-moor-insert-lines (target title text)
  "Write TEXT, whole lines, at TARGET, and return a marker at its start.
TARGET is as `org-moor--target' returns it.  A marker on a heading's
line puts the lines at the end of the heading's own text, and anywhere
else after the line it is on.  A file, or `inbox', is given a new
heading at its top level titled TITLE, the lines in it."
  (let ((at (if (markerp target)
                (org-moor--writable target)
              (org-moor-insert-heading target title))))
    (prog1 (with-current-buffer (marker-buffer at)
             (org-with-wide-buffer
              (goto-char at)
              (if (save-excursion (beginning-of-line) (org-at-heading-p))
                  (progn (beginning-of-line) (org-moor--end-of-entry))
                (org-moor--next-line))
              (prog1 (point-marker)
                (insert text))))
      (unless (eq at target) (set-marker at nil)))))

(defun org-moor-insert-link-line (target title link)
  "Write LINK on a line of its own at TARGET, and return a marker at it.
As `org-moor-insert-lines' writes."
  (org-moor-insert-lines target title (concat link "\n")))

(defun org-moor--take-at (target title take)
  "Take a source at TARGET, titled TITLE, and return a marker at its link.
TAKE is called with the sources directory of TARGET, and returns the
record of what it took there.  TARGET is let go of, whatever happens."
  (unwind-protect
      (org-moor-insert-link-line
       target title (org-moor-link (funcall take (org-moor--target-sources target))))
    (org-moor--release target)))

(defun org-moor-quote-string (title text &optional time)
  "Return TEXT as a quote block titled TITLE, taken at TIME."
  (concat "#+begin_quote " title " "
          (format-time-string (org-time-stamp-format t t) time) "\n"
          (org-escape-code-in-string (string-trim-right text)) "\n"
          "#+end_quote\n"))

(defun org-moor--clipboard ()
  "Return the text on the clipboard, or signal if there is none."
  ;; Each way is tried until one gives text.  An empty string is not an
  ;; answer: macOS gives one for UTF8_STRING, a type it does not serve,
  ;; while the default type holds the text.
  (let ((text (seq-some
               (lambda (get)
                 (let ((s (ignore-errors (funcall get))))
                   (and (stringp s) (not (string-blank-p s)) s)))
               (list (lambda () (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
                     (lambda () (gui-get-selection 'CLIPBOARD))
                     (lambda () (current-kill 0 t))))))
    (if text
        (substring-no-properties text)
      (user-error "org-moor: the clipboard is empty"))))

;;;###autoload (autoload 'org-moor-quote "org-moor" nil t)
(defun org-moor-quote (title &optional pick)
  "Quote the clipboard under TITLE, at point or, with PICK, a chosen heading.
TITLE says who it is from and about what: \"Chat from Robin\"."
  (interactive (list (read-string "Title: ") current-prefix-arg))
  (let* ((text (org-moor--clipboard))
         (target (org-moor--target pick)))
    (unwind-protect
        (set-marker (org-moor-insert-lines target title
                                           (org-moor-quote-string title text))
                    nil)
      (org-moor--release target))
    (message "Quoted: %s" title)))

;;;; Files

(defcustom org-moor-move-directories nil
  "Directories whose files are moved when taken; files elsewhere are copied.
These are the places things pile up -- downloads, a desktop -- where taking
a file is tidying it away.  Anywhere else the file is somebody's place for
it, and taking it leaves it where it was."
  :type '(repeat directory)
  :group 'org-moor)

(defun org-moor-move-p (file)
  "Non-nil when taking FILE moves it, by `org-moor-move-directories'."
  (let ((file (file-truename file)))
    (seq-some (lambda (dir)
                (file-in-directory-p file (file-truename dir)))
              org-moor-move-directories)))

(defun org-moor--file-at-point ()
  "Return the file at point in Dired, or nil."
  (and (derived-mode-p 'dired-mode)
       (fboundp 'dired-get-filename)
       (dired-get-filename nil t)))

(defvar org-moor--default-title nil
  "Title offered for the next file taken, when the caller knows a better
one than its name -- a document's own title, sent from outside Emacs.")

(defun org-moor-take-file-to (file sources &optional title via move origin)
  "Take FILE into SOURCES as a source titled TITLE, and return its record.
The copy is FILE moved there with MOVE, else copied.  TITLE is the file's
name without its extension when nil; VIA says how it arrived, `file' when
nil; ORIGIN is where it came from, FILE itself when nil.  Nothing is
written in Org: a link to it is the caller's, with `org-moor-link'."
  (let* ((file (expand-file-name file))
         (name (file-name-nondirectory file))
         (record (org-moor-new-source (or title (file-name-sans-extension name))
                                      (or origin file) (or via 'file) sources))
         (copy (expand-file-name name (plist-get record :dir))))
    (condition-case err
        (if move
            (rename-file file copy)
          (copy-file file copy nil t))
      (error
       (delete-directory (plist-get record :dir) t)
       (signal (car err) (cdr err))))
    (org-moor-record-set record :file name)))

;;;###autoload (autoload 'org-moor-take-file "org-moor" nil t)
(defun org-moor-take-file (file &optional pick via flip origin)
  "Take FILE as a source: its copy in the sources, a link to it here.
At point when in Org; with PICK, or from outside Org, at a chosen
heading -- see `org-moor-insert-link-line'.  The source goes in the
sources directory of the file the link goes in.  VIA says how it
arrived, `file' when nil.  The original is moved when it is in
`org-moor-move-directories' and copied otherwise; FLIP turns that round
for this once.  ORIGIN is where it came from, when that is not FILE
itself -- a copy saved from a document opened from the web.
Return a marker at the link.

Interactively FILE is the one at point in Dired, else read.  One
\\[universal-argument] picks the heading and asks VIA; two also flip."
  (interactive
   (let ((arg (prefix-numeric-value current-prefix-arg)))
     (list (or (org-moor--file-at-point)
               (read-file-name "Take file: " nil nil t))
           (>= arg 4)
           (and (>= arg 4)
                (completing-read "Via: " '("file" "mail" "chat" "web")))
           (>= arg 16))))
  (unless (file-regular-p file)
    (user-error "org-moor: not a regular file: %s" file))
  (let* ((file (expand-file-name file))
         (name (file-name-nondirectory file))
         (move (if flip (not (org-moor-move-p file)) (org-moor-move-p file)))
         (title (read-string (format "Title (%s): " (if move "move" "copy"))
                             (or org-moor--default-title
                                 (file-name-sans-extension name))))
         (link (org-moor--take-at
                (org-moor--target pick) title
                (lambda (sources)
                  (org-moor-take-file-to file sources title via move origin)))))
    (when (and move (fboundp 'dired-remove-file))
      (dired-remove-file file))
    (message "Took %s (%s)" name (if move "moved" "copied"))
    link))

;;;; Web pages

;; What a URL is -- a page, or a file -- is told by what it sends, not by
;; how it is spelled: its Content-Type, the name its Content-Disposition
;; gives, and failing those, its first bytes.  So it is fetched first and
;; named after.

(defcustom org-moor-snapshot-function #'org-moor-snapshot-monolith
  "Function keeping a web page: called with URL, the file to write and a
callback taking non-nil on success.  When it keeps nothing, or is nil,
the page's HTML is kept as it was sent, without what it links to."
  :type '(choice function (const nil))
  :group 'org-moor)

(defcustom org-moor-monolith-program "monolith"
  "The monolith executable, which saves a page with everything it needs."
  :type 'string
  :group 'org-moor)

(defcustom org-moor-cookie-file nil
  "Cookies, in the Netscape cookies.txt format, sent with every fetch.
A browser's sign-ins are its own; written out to this file -- by a
browser extension, say -- they let curl and monolith fetch what is
behind them.  It is read, never written.  Nil sends none."
  :type '(choice (const :tag "None" nil) file)
  :group 'org-moor)

(defun org-moor--cookie-args (flag)
  "Return FLAG and `org-moor-cookie-file', when there is that file.
When it is named and is not there, say so: a fetch without it is as
nobody, and gets a sign-in page."
  (when org-moor-cookie-file
    (if (file-readable-p org-moor-cookie-file)
        (list flag (expand-file-name org-moor-cookie-file))
      (message "%s" (propertize
                     (format "org-moor: no cookie file %s; fetching without"
                             org-moor-cookie-file)
                     'face 'warning))
      nil)))

(defun org-moor-snapshot-monolith (url file done)
  "Save URL to FILE with monolith, then call DONE with success."
  (if (not (executable-find org-moor-monolith-program))
      (funcall done nil)
    (make-process
     :name "org-moor-snapshot"
     :buffer " *org-moor-snapshot*"
     :command `(,org-moor-monolith-program "-s" ,@(org-moor--cookie-args "-C")
                "-o" ,file ,url)
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (funcall done (and (zerop (process-exit-status proc))
                                      (file-exists-p file))))))))

(defconst org-moor-snapshot-file "page.html"
  "Name of a web page's copy in its directory.")

(defcustom org-moor-mime-extensions
  '(("text/html" . "html") ("application/xhtml+xml" . "html")
    ("application/pdf" . "pdf")
    ("text/plain" . "txt") ("text/markdown" . "md") ("text/csv" . "csv")
    ("application/json" . "json") ("application/xml" . "xml") ("text/xml" . "xml")
    ("application/rtf" . "rtf")
    ("application/msword" . "doc")
    ("application/vnd.openxmlformats-officedocument.wordprocessingml.document" . "docx")
    ("application/vnd.ms-powerpoint" . "ppt")
    ("application/vnd.openxmlformats-officedocument.presentationml.presentation" . "pptx")
    ("application/vnd.ms-excel" . "xls")
    ("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" . "xlsx")
    ("application/vnd.oasis.opendocument.text" . "odt")
    ("application/vnd.oasis.opendocument.presentation" . "odp")
    ("application/vnd.oasis.opendocument.spreadsheet" . "ods")
    ("application/epub+zip" . "epub") ("application/zip" . "zip")
    ("image/png" . "png") ("image/jpeg" . "jpg") ("image/gif" . "gif")
    ("image/svg+xml" . "svg") ("image/webp" . "webp")
    ("audio/mpeg" . "mp3") ("video/mp4" . "mp4"))
  "Media types and the extension a file of each is named with.
The extensions are also those a URL is known by to name a file, so that
a page sent for one -- a sign-in page -- is told from the file."
  :type '(alist :key-type string :value-type string)
  :group 'org-moor)

(defcustom org-moor-download-extensions nil
  "Obsolete: what a URL is, is told by what it sends."
  :type '(repeat string)
  :group 'org-moor)
(make-obsolete-variable 'org-moor-download-extensions 'org-moor-mime-extensions "0.3")

(defcustom org-moor-curl-program "curl"
  "The curl executable, used to fetch a URL."
  :type 'string
  :group 'org-moor)

(defun org-moor--known-name-p (name)
  "Non-nil when NAME has an extension of `org-moor-mime-extensions'."
  (rassoc (org-moor-extension name) org-moor-mime-extensions))

(defun org-moor--url-name (url)
  "Return the last part of URL's path, decoded, or \"\" when it has none."
  (file-name-nondirectory
   (decode-coding-string
    (url-unhex-string (or (car (url-path-and-query (url-generic-parse-url url))) ""))
    'utf-8)))

(defun org-moor-url-file-name (url)
  "Return the file name URL is spelled with, or nil if it names none.
That is the last part of its path, decoded, when its extension is one of
`org-moor-mime-extensions'."
  (let ((name (org-moor--url-name url)))
    (and (org-moor--known-name-p name) name)))

(defun org-moor-url-unwrap (url)
  "Return the URL of the file URL shows, when URL is a viewer of one.
A viewer names what it shows in its query: a value there that is itself
a URL spelled as a file, see `org-moor-url-file-name', is the one meant.
Anything else is itself."
  (let ((query (cdr (url-path-and-query (url-generic-parse-url url)))))
    (or (seq-some (lambda (pair)
                    (let ((inner (decode-coding-string (or (cadr pair) "") 'utf-8)))
                      (and (string-match-p "\\`https?://" inner)
                           (org-moor-url-file-name inner)
                           inner)))
                  (and query (url-parse-query-string query)))
        url)))

(defun org-moor-sniff-type (file)
  "Return the media type FILE's first bytes show, or nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 512)
    (goto-char (point-min))
    (skip-chars-forward "\xef\xbb\xbf \t\r\n")
    (let ((case-fold-search t))
      (cond ((looking-at-p "%PDF-") "application/pdf")
            ((looking-at-p "<\\(!doctype html\\|html\\|head\\|body\\|!--\\)") "text/html")
            ((looking-at-p "PK\3\4") "application/zip")))))

(defun org-moor--disposition-name (header)
  "Return the file name a Content-Disposition HEADER gives, or nil."
  (when header
    (let ((case-fold-search t))
      (cond ((string-match "filename\\*=\\([^']*\\)'[^']*'\\([^;]+\\)" header)
             (let ((charset (intern (downcase (match-string 1 header))))
                   (name (string-trim (match-string 2 header))))
               (decode-coding-string (url-unhex-string name)
                                     (if (coding-system-p charset) charset 'utf-8))))
            ((string-match "filename=\"\\([^\"]+\\)\"" header) (match-string 1 header))
            ((string-match "filename=\\([^;]+\\)" header)
             (string-trim (match-string 1 header)))))))

(defun org-moor-copy-name (url type &optional disposition)
  "Return the name a copy of URL is given, sent as TYPE with DISPOSITION.
A page is `org-moor-snapshot-file'.  A file is named as its
Content-Disposition names it, else as its URL is spelled, else
\"file\"; given the extension of TYPE when its own is not one known."
  (let* ((ext (cdr (assoc type org-moor-mime-extensions)))
         (name (or (org-moor--disposition-name disposition)
                   (let ((last (org-moor--url-name url)))
                     (and (not (string-empty-p last)) last))
                   "file")))
    (cond ((equal ext "html") org-moor-snapshot-file)
          ((or (not ext) (org-moor--known-name-p name)) name)
          (t (concat name "." ext)))))

(defun org-moor--curl (url file done)
  "Fetch URL to FILE by curl; call DONE with success, type and disposition."
  (make-process
   :name "org-moor-fetch"
   :buffer (generate-new-buffer " *org-moor-fetch*")
   :command `(,org-moor-curl-program "-fsSL" ,@(org-moor--cookie-args "-b")
              "-o" ,file "-w" "%{content_type}\\n%header{content-disposition}" ,url)
   :sentinel (lambda (proc _event)
               (unless (process-live-p proc)
                 (let ((lines (with-current-buffer (process-buffer proc)
                                (split-string (buffer-string) "\n"))))
                   (kill-buffer (process-buffer proc))
                   (funcall done (zerop (process-exit-status proc))
                            (car lines) (cadr lines)))))))

(defun org-moor--fetch (url file done)
  "Fetch URL to FILE; call DONE with success, media type and disposition.
By curl when there is one, following redirects; else by `url-copy-file',
which tells neither."
  (if (executable-find org-moor-curl-program)
      (org-moor--curl url file done)
    (funcall done (ignore-errors (url-copy-file url file t) t) nil nil)))

(defun org-moor-fetch (url dir done)
  "Keep a copy of URL in DIR, then call DONE with its name, or nil.
What it is, is what it sends: a page is kept by
`org-moor-snapshot-function', or else as its HTML; a file under the name
`org-moor-copy-name' gives it.  A page sent for a URL spelled as a file
-- a sign-in page -- is not the file, and nothing is kept."
  (let ((fetched (make-temp-file (expand-file-name ".fetch" dir))))
    (org-moor--fetch
     url fetched
     (lambda (ok type disposition)
       (let* ((type (and ok (file-exists-p fetched)
                         (let ((sent (and type (car (split-string type "[; ]" t)))))
                           (if (or (not sent) (equal sent "application/octet-stream"))
                               (or (org-moor-sniff-type fetched) sent)
                             (downcase sent)))))
              (name (and type (org-moor-copy-name url type disposition)))
              (page (equal name org-moor-snapshot-file))
              (spelled (org-moor-url-file-name url)))
         (cond
          ((or (not name)
               (and page spelled
                    (not (equal (cdr (assoc "text/html" org-moor-mime-extensions))
                                (org-moor-extension spelled)))))
           (ignore-errors (delete-file fetched))
           (funcall done nil))
          (t
           (rename-file fetched (expand-file-name name dir) t)
           (if (not (and page org-moor-snapshot-function))
               (funcall done name)
             (let ((whole (make-temp-file (expand-file-name ".page" dir))))
               (funcall org-moor-snapshot-function url whole
                        (lambda (ok)
                          (if ok
                              (rename-file whole (expand-file-name name dir) t)
                            (ignore-errors (delete-file whole)))
                          (funcall done name))))))))))))

(defconst org-moor-partial-download-regexp
  "\\.\\(crdownload\\|part\\|partial\\|tmp\\|download\\)\\'"
  "Names a browser gives a download until it is whole.")

(defcustom org-moor-arriving-timeout 120
  "Seconds to wait for a copy a browser is downloading."
  :type 'integer
  :group 'org-moor)

(defun org-moor--arrived (dir since)
  "Return the newest file come into DIR since SINCE, or `partial', or nil.
`partial' while a download there is not yet whole."
  (let ((new (seq-filter
              (lambda (f)
                (and (file-regular-p f)
                     (not (time-less-p (file-attribute-modification-time
                                        (file-attributes f))
                                       since))))
              (directory-files dir t "\\`[^.]"))))
    (cond ((seq-some (lambda (f) (string-match-p org-moor-partial-download-regexp f))
                     new)
           'partial)
          (new (car (sort new #'file-newer-than-file-p))))))

(defun org-moor-arriving (dir since)
  "Return a way to keep a copy that a browser is downloading into DIR.
The copy is the file come into DIR since SINCE, a time, once it is whole;
it is moved to the source.  For `org-moor-take-url-to', as KEEP."
  (let ((since (time-subtract since 2)))
    (lambda (_url into done)
      (let ((deadline (+ (float-time) org-moor-arriving-timeout))
            (size nil)
            (timer nil))
        (setq timer
              (run-with-timer
               0.5 0.5
               (lambda ()
                 (condition-case err
                     (let ((file (org-moor--arrived dir since)))
                       (cond
                        ((and (stringp file)
                              (eql size (file-attribute-size (file-attributes file)))
                              (> size 0))
                         (cancel-timer timer)
                         (let ((name (file-name-nondirectory file)))
                           (rename-file file (expand-file-name name into))
                           (funcall done name)))
                        ((> (float-time) deadline)
                         (cancel-timer timer)
                         (funcall done nil))
                        (t (setq size (and (stringp file)
                                           (file-attribute-size
                                            (file-attributes file)))))))
                   (error (cancel-timer timer)
                          (message "org-moor: %s" (error-message-string err))
                          (funcall done nil))))))))))

(defun org-moor--clipboard-url ()
  "Return the clipboard's text when it is a URL, or nil."
  (let ((text (ignore-errors (string-trim (org-moor--clipboard)))))
    (and text (string-match-p "\\`https?://[^ \t\n]+\\'" text) text)))

(defun org-moor-take-url-to (url sources &optional title wait keep)
  "Take URL into SOURCES as a source titled TITLE, and return its record.
Its copy is kept by KEEP, `org-moor-fetch' when nil, in the background,
or with WAIT before this returns: called with URL, the source's
directory and a function to call with the copy's name there, or nil.
TITLE is the file's name, for a URL spelled as one, else the URL, when
nil.  Nothing is written in Org."
  (let* ((url (org-moor-url-unwrap url))
         (name (org-moor-url-file-name url))
         (title (or title (and name (file-name-sans-extension name)) url))
         (record (org-moor-new-source title url 'web sources))
         (done nil))
    (funcall (or keep #'org-moor-fetch) url (plist-get record :dir)
             (lambda (file)
               (if (not file)
                   (message "org-moor: kept only the URL of %s" title)
                 (org-moor-record-set record :file file)
                 (message "Kept %s" title))
               (setq done t)))
    (when wait
      (while (not done) (accept-process-output nil 0.1)))
    (or (org-moor-record (plist-get record :id) sources) record)))

;;;###autoload (autoload 'org-moor-take-url "org-moor" nil t)
(defun org-moor-take-url (url &optional title pick keep)
  "Take URL as a source, titled TITLE, and keep a copy of it.
The copy is what URL sends -- see `org-moor-fetch' -- or what KEEP
keeps, as `org-moor-take-url-to' says.  At point when in Org; with PICK,
at a chosen heading, as `org-moor-take-file' takes a file.
Interactively the URL on the clipboard is offered.  Return a marker at
the link."
  (interactive
   (list (read-string "URL: " (org-moor--clipboard-url)) nil current-prefix-arg))
  (let* ((url (org-moor-url-unwrap url))
         (name (org-moor-url-file-name url))
         (title (read-string "Title: " (or title
                                           (and name (file-name-sans-extension name))))))
    (org-moor--take-at (org-moor--target pick) title
                       (lambda (sources)
                         (org-moor-take-url-to url sources title nil keep)))))

;;;; From outside Emacs

(defun org-moor-batch-take ()
  "Take a file or a web page into a sources directory, and print a link to it.
For an agent, which cannot ask Emacs:

  emacs --batch -L DIR -L ORG-DOCK-DIR -l org-moor \\
        -f org-moor-batch-take SOURCES FILE-OR-URL [TITLE]

SOURCES is the sources directory; a file is copied, never moved, and
titled by its name unless TITLE is given.  What is printed is a link to
the source named by its title, [[moor:ID][TITLE]]."
  (let* ((sources (file-name-as-directory
                   (expand-file-name (or (pop command-line-args-left)
                                         (error "org-moor: no sources directory")))))
         (what (or (pop command-line-args-left)
                   (error "org-moor: no file or URL to take")))
         (title (pop command-line-args-left))
         (record (if (string-match-p "\\`https?://" what)
                     (org-moor-take-url-to what sources title t)
                   (unless (file-regular-p what)
                     (error "org-moor: not a file: %s" what))
                   (org-moor-take-file-to what sources title))))
    (princ (concat (org-moor-link record) "\n"))))

;;;; org-protocol

(defun org-moor--file-url-path (url)
  "Return the file a file: URL names: file:///C:/a%20b.pdf is C:/a b.pdf."
  (let ((path (decode-coding-string
               (url-unhex-string (replace-regexp-in-string
                                  "\\`file:\\(//[^/]*\\)?" "" url))
               'utf-8)))
    (if (string-match-p "\\`/[A-Za-z]:" path) (substring path 1) path)))

;;;###autoload (autoload 'org-moor-protocol "org-moor" nil nil)
(defun org-moor-protocol (info)
  "org-protocol handler.  INFO is a plist from the protocol URL.

  org-protocol://moor?url=URL&title=TITLE
  org-protocol://moor?url=URL&title=TITLE&arriving=DIR&since=SECONDS
  org-protocol://moor?path=PATH&title=TITLE&origin=ORIGIN&via=VIA

The first takes a page or a file on the web.  The second takes a file on
the web that a browser, signed in where it is, is downloading into DIR:
its copy is the file come there since SECONDS, a Unix time, moved to the
source once whole -- see `org-moor-arriving'.  The third takes a file on
disk, as does a file: URL, which a browser shows for a file it opened;
ORIGIN, when given, is where that file was opened from.  Either arrives
from outside Emacs, with no point to go to, so a heading is always picked,
and Emacs is brought to the front to ask."
  (select-frame-set-input-focus (selected-frame))
  (let ((url (plist-get info :url))
        (path (plist-get info :path))
        (title (plist-get info :title)))
    ;; A file opened in a browser has a file: URL, and is a file on disk.
    (when (and url (string-match "\\`file:" url))
      (setq path (org-moor--file-url-path url) url nil))
    (cond
     (path
      (let ((org-moor--default-title title))
        (org-moor-take-file path t (plist-get info :via) nil
                            (plist-get info :origin))))
     (url (org-moor-take-url
           url title t
           (when-let* ((dir (plist-get info :arriving)))
             (org-moor-arriving
              dir (if-let* ((since (plist-get info :since)))
                      (seconds-to-time (string-to-number since))
                    (current-time))))))
     (t (message "org-moor: protocol URL had no url= or path="))))
  nil)

;;;###autoload (autoload 'org-moor-protocol-setup "org-moor" nil nil)
(defun org-moor-protocol-setup ()
  "Register the `moor' org-protocol."
  (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist
               '("org-moor" :protocol "moor" :function org-moor-protocol)))

(provide 'org-moor-take)
;;; org-moor-take.el ends here
