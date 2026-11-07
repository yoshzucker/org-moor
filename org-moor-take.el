;;; org-moor-take.el --- Bring things in  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Taking is the step that happens when you meet a thing, which is usually
;; outside Emacs, so every command here asks as little as it can: a title,
;; and where it goes.  Where it goes is anywhere: point when you are in Org,
;; else a heading you pick from `org-moor-take-targets', else
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

(defcustom org-moor-take-targets nil
  "Headings a thing can be taken to, in the form of `org-refile-targets'.
Nil means `org-refile-targets'.  Set it to offer more than refiling
does -- every note, say, not only the files with tasks in them."
  :type '(choice (const :tag "org-refile-targets" nil) sexp)
  :group 'org-moor)

(defun org-moor--targets ()
  "Return the targets to take to, read from the inbox when not in Org.
Each is (NAME FILE REGEXP POS); POS is nil for a file as a whole."
  (with-current-buffer (if (derived-mode-p 'org-mode)
                           (current-buffer)
                         (org-moor--inbox-buffer))
    (let ((org-refile-targets (or org-moor-take-targets org-refile-targets)))
      (org-refile-get-targets))))

(defun org-moor--target-at (entry)
  "Return the target for refile ENTRY: a marker, or its file for the top."
  (if (nth 3 entry)
      (with-current-buffer (find-file-noselect (nth 1 entry))
        (copy-marker (nth 3 entry)))
    (nth 1 entry)))

(defun org-moor-read-heading (&optional prompt)
  "Read a heading to take to, existing or new, and return the target.
A name from `org-moor-take-targets' is that heading.  PARENT/NAME, where
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
      ;; Its name is read first: visiting the parent's file may change the
      ;; match data, as it does on Windows.
      (let ((parent (assoc (match-string 1 answer) table))
            (name (match-string 2 answer)))
        (org-moor-insert-heading (org-moor--target-at parent) name)))
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

(defvar evil-state)

(declare-function evil-visual-state-p "evil-states")
(declare-function evil-exit-visual-state "evil-states")

(defun org-moor--here-p (target)
  "Non-nil when TARGET is point in this buffer, where a link is typed.
Not in a heading's stars, where text would make it no heading, nor in a
heading taken out by org-dock, which is written in its copy."
  (and (markerp target)
       (eq (marker-buffer target) (current-buffer))
       (= target (point))
       (not (save-excursion
              (beginning-of-line)
              (and (looking-at org-outline-regexp)
                   (< target (match-end 0)))))
       (not (org-dock-copy-of target))))

(defun org-moor--on-character-p ()
  "Non-nil when the cursor is on a character, not between two.
So it is in every Evil state but insert, replace and Emacs, whatever
other states there are."
  (and (bound-and-true-p evil-local-mode)
       (not (memq evil-state '(insert replace emacs)))))

(defun org-moor-insert-link-here (link)
  "Insert LINK at point, as typed, leaving point after it.
When the cursor is on a character -- Evil's normal, motion, visual
state -- after that character, as \`a' would type it; visual state is
left first.  Return a marker at the link."
  (when (and (fboundp 'evil-visual-state-p) (evil-visual-state-p))
    (evil-exit-visual-state))
  (when (and (org-moor--on-character-p) (not (eolp)))
    (forward-char 1))
  (prog1 (point-marker)
    (insert link)))

(defun org-moor--take-at (target title take)
  "Take a source at TARGET, titled TITLE, and return a marker at its link.
TAKE is called with the sources directory of TARGET, and returns the
record of what it took there.  At point, the link goes in the text, see
`org-moor-insert-link-here'; elsewhere on a line of its own.  TARGET is
let go of, whatever happens."
  (unwind-protect
      (let ((here (org-moor--here-p target)))
        (org-moor--check-writable target here)
        (let ((link (org-moor-link (funcall take (org-moor--target-sources target)))))
          (if here
              (org-moor-insert-link-here link)
            (org-moor-insert-link-line target title link))))
    (org-moor--release target)))

(defun org-moor--check-writable (target here)
  "Signal when the link to be written at TARGET cannot be, before a source is made.
HERE is non-nil when it goes at point.  A heading taken out by org-dock
is written in its copy, which is read-only while an agent writes it:
taken then, the source would be made and its link written nowhere."
  (when (markerp target)
    (let ((buffer (if here
                      (current-buffer)
                    (with-current-buffer (marker-buffer target)
                      (or (org-dock-copy-of target) (current-buffer))))))
      (with-current-buffer buffer
        (when buffer-read-only
          (user-error "org-moor: %s is read-only%s; nothing taken"
                      (buffer-name)
                      (if-let* ((who (and buffer-file-name (org-dock-busy buffer-file-name))))
                          (format " while %s writes it" who)
                        "")))))))

;;;; The clipboard

;; What is copied -- a chat's message, a mail's paragraph, a screenshot --
;; is taken as any file is: a source of its own, its text or its image the
;; replica, so that it is kept, and read, where every other source is.

(defun org-moor--clipboard-text ()
  "Return the text on the system's clipboard, or nil."
  ;; An empty string is not an answer: macOS gives one for UTF8_STRING, a
  ;; type it does not serve, while the default type holds the text.
  (when-let* ((text (seq-some
                     (lambda (type)
                       (let ((s (ignore-errors (gui-get-selection 'CLIPBOARD type))))
                         (and (stringp s) (not (string-blank-p s)) s)))
                     '(UTF8_STRING STRING))))
    (substring-no-properties text)))

(defun org-moor--clipboard ()
  "Return the text on the clipboard, or the latest kill, or signal."
  (or (org-moor--clipboard-text)
      (let ((s (ignore-errors (current-kill 0 t))))
        (and (stringp s) (not (string-blank-p s)) (substring-no-properties s)))
      (user-error "org-moor: the clipboard is empty")))

(defun org-moor--clipboard-image (file)
  "Write the image on the clipboard to FILE as PNG; non-nil when there was one.
Asked of Emacs first, which knows image/png on most systems; macOS's
clipboard, which Emacs there does not read images from, is asked through
AppleScript, and Windows's through PowerShell, when Emacs had nothing."
  (let ((data (ignore-errors (gui-get-selection 'CLIPBOARD 'image/png))))
    (cond
     ((and (stringp data) (> (length data) 0))
      (let ((coding-system-for-write 'no-conversion))
        (with-temp-file file
          (set-buffer-multibyte nil)
          (insert data))))
     ((and (eq system-type 'darwin) (executable-find "osascript"))
      (call-process "osascript" nil nil nil
                    "-e" "try"
                    "-e" "set d to the clipboard as «class PNGf»"
                    "-e" (format "set f to open for access (POSIX file %S) with write permission"
                                 file)
                    "-e" "write d to f"
                    "-e" "close access f"
                    "-e" "end try"))
     ((and (eq system-type 'windows-nt) (executable-find "powershell"))
      (let ((coding-system-for-write (org-moor-argument-coding)))
        (call-process "powershell" nil nil nil "-NoProfile" "-Command"
                      (concat "Add-Type -AssemblyName System.Windows.Forms;"
                              "$i = [Windows.Forms.Clipboard]::GetImage();"
                              "if ($i) { $i.Save('"
                              (replace-regexp-in-string "'" "''" (convert-standard-filename file))
                              "', [Drawing.Imaging.ImageFormat]::Png) }"))))))
  (if (and (file-exists-p file) (> (file-attribute-size (file-attributes file)) 0))
      file
    (when (file-exists-p file) (delete-file file))
    nil))

(defun org-moor--file-name-of (title extension)
  "Return a file name made of TITLE, with EXTENSION."
  (let ((name (string-trim (replace-regexp-in-string
                            "[ /\\\\:*?\"<>|[:cntrl:]]+" " " title))))
    (concat (if (string-empty-p name) "clipboard" (truncate-string-to-width name 60))
            "." extension)))

(defun org-moor-take-written-to (write extension sources title &optional via origin)
  "Take what WRITE writes into SOURCES as a source titled TITLE; return its record.
WRITE is called with the replica's file, named after TITLE with EXTENSION.
VIA says how it arrived, `clipboard' when nil; ORIGIN, where it came
from, is left out when nil.  Nothing is written in Org."
  (let* ((record (org-moor-new-source title origin sources))
         (name (org-moor--file-name-of title extension)))
    (condition-case err
        (funcall write (expand-file-name name (plist-get record :dir)))
      (error
       (delete-directory (plist-get record :dir) t)
       (signal (car err) (cdr err))))
    (org-moor-record-replica record name (or via 'clipboard))))

(defun org-moor-take-text-to (text sources title &optional via origin)
  "Take TEXT into SOURCES as a source titled TITLE, a text file its replica.
VIA and ORIGIN as `org-moor-take-written-to' takes them."
  (org-moor-take-written-to
   (lambda (file) (org-moor--write-text file (concat (string-trim-right text) "\n")))
   "txt" sources title via origin))

;;;###autoload (autoload 'org-moor-take-clipboard "org-moor" nil t)
(defun org-moor-take-clipboard (&optional pick via origin)
  "Take the clipboard as a source: its text, or its image, a link to it here.
The text is kept as a text file, the image as a PNG.  When the clipboard
holds both -- cells copied from a spreadsheet -- you are asked which.
At point when in Org; with PICK, at a chosen heading, as
`org-moor-take-file' takes a file.  VIA says how it arrived, `clipboard'
when nil; ORIGIN is where it came from -- a link to the message -- and
left out when nil.  Return a marker at the link.

Interactively one \\[universal-argument] picks the heading and asks VIA
and ORIGIN."
  (interactive
   (let ((ask (and current-prefix-arg t)))
     (list ask
           (and ask (completing-read "Via: " '("chat" "mail" "web" "clipboard")))
           (and ask (let ((o (string-trim (read-string "Origin (none): "))))
                      (and (not (string-empty-p o)) o))))))
  (let* ((image (make-temp-file "moor" nil ".png"))
         (text (org-moor--clipboard-text))
         (has-image (org-moor--clipboard-image image)))
    (unwind-protect
        (let* ((as (cond ((and text has-image)
                          (if (eq (read-char-choice
                                   "Take the clipboard as (t)ext or (i)mage? " '(?t ?i))
                                  ?i)
                              'image 'text))
                         (has-image 'image)
                         (t (setq text (org-moor--clipboard)) 'text)))
               (title (read-string (if (eq as 'image) "Title (image): " "Title: "))))
          (prog1 (org-moor--take-at
                  (org-moor--target pick) title
                  (lambda (sources)
                    (if (eq as 'image)
                        (org-moor-take-written-to
                         (lambda (file) (copy-file image file))
                         "png" sources title via origin)
                      (org-moor-take-text-to text sources title via origin))))
            (message "Took the clipboard's %s: %s" as title)))
      (when (file-exists-p image) (delete-file image)))))

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
The replica is FILE moved there with MOVE, else copied.  TITLE is the file's
name without its extension when nil; VIA says how it arrived, `file' when
nil; ORIGIN is where it came from, FILE itself when nil.  Nothing is
written in Org: a link to it is the caller's, with `org-moor-link'."
  (let* ((file (expand-file-name file))
         (name (file-name-nondirectory file))
         (record (org-moor-new-source (or title (file-name-sans-extension name))
                                      (or origin file) sources))
         (replica (expand-file-name name (plist-get record :dir))))
    (condition-case err
        (if move
            (rename-file file replica)
          (copy-file file replica nil t))
      (error
       (delete-directory (plist-get record :dir) t)
       (signal (car err) (cdr err))))
    (org-moor-record-replica record name (or via 'file))))

;;;###autoload (autoload 'org-moor-take-file "org-moor" nil t)
(defun org-moor-take-file (file &optional pick via flip origin)
  "Take FILE as a source: its replica in the sources, a link to it here.
At point when in Org; with PICK, or from outside Org, at a chosen
heading -- see `org-moor-insert-link-line'.  The source goes in the
sources directory of the file the link goes in.  VIA says how it
arrived, `file' when nil.  The original is moved when it is in
`org-moor-move-directories' and copied otherwise; FLIP turns that round
for this once.  ORIGIN is where it came from, when that is not FILE
itself -- a replica saved from a document opened from the web.
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

;;;; Taken for an agent

(defvar org-moor--agent nil
  "Non-nil while a source is taken for an agent.
What is said of it then goes to *Messages* alone, not over what the user
is doing, since the agent is told it.")

(defun org-moor--say (format-string &rest args)
  "Say FORMAT-STRING with ARGS as `message' does; in *Messages* alone for an agent."
  (let ((inhibit-message (or inhibit-message org-moor--agent)))
    (apply #'message format-string args)))

;;;; From the web

;; org-moor fetches nothing.  A page, or a file on the web, is opened in
;; the browser you read in, and only when you say so; its replica is what
;; the browser saves -- the page by SingleFile, the file downloaded -- into
;; `org-moor-downloads-directory', taken from there once you say it is
;; the one.  Until then the source has its origin alone, and its replica
;; is awaited: see `org-moor-awaiting'.  What a source is made readable as
;; for a machine, an agent fetches itself, and hands over as a file.

(defcustom org-moor-downloads-directory "~/Downloads/"
  "Directory the browser saves into, where a replica is waited for."
  :type 'directory
  :group 'org-moor)

(defcustom org-moor-file-extensions
  '("pdf" "txt" "md" "csv" "json" "xml" "rtf" "doc" "docx" "ppt" "pptx"
    "xls" "xlsx" "odt" "odp" "ods" "epub" "zip" "png" "jpg" "gif" "svg"
    "webp" "mp3" "mp4")
  "Extensions a URL is known by to name a file, which a source is titled by."
  :type '(repeat string)
  :group 'org-moor)

(defun org-moor--known-name-p (name)
  "Non-nil when NAME has an extension of `org-moor-file-extensions'."
  (member (org-moor-extension name) org-moor-file-extensions))

(defun org-moor--url-name (url)
  "Return the last part of URL's path, decoded, or \"\" when it has none."
  (file-name-nondirectory
   (decode-coding-string
    (url-unhex-string (or (car (url-path-and-query (url-generic-parse-url url))) ""))
    'utf-8)))

(defun org-moor-url-file-name (url)
  "Return the file name URL is spelled with, or nil if it names none.
That is the last part of its path, decoded, when its extension is one of
`org-moor-file-extensions'."
  (let ((name (org-moor--url-name url)))
    (and (org-moor--known-name-p name) name)))

(defun org-moor-url-unwrap (url)
  "Return the URL of the file URL shows, when URL is a viewer of one.
A viewer names what it shows in its query: a value there that is itself
a URL spelled as a file, see `org-moor-url-file-name', is the one meant;
so is a path on URL's own host spelled as one, as a document library's
viewer names its file.  Anything else is itself."
  (let* ((parsed (url-generic-parse-url url))
         (query (cdr (url-path-and-query parsed)))
         (origin (concat (url-type parsed) "://" (url-host parsed)
                         (if (url-portspec parsed) (format ":%d" (url-portspec parsed)) ""))))
    (or (seq-some (lambda (pair)
                    (let* ((inner (decode-coding-string (or (cadr pair) "") 'utf-8))
                           (inner (if (string-match-p "\\`/[^/]" inner)
                                      (concat origin inner)
                                    inner)))
                      (and (string-match-p "\\`https?://" inner)
                           (org-moor-url-file-name inner)
                           inner)))
                  (and query (url-parse-query-string query)))
        url)))

(defun org-moor--clipboard-url ()
  "Return the clipboard's text when it is a URL, or nil."
  (let ((text (ignore-errors (string-trim (org-moor--clipboard)))))
    (and text (string-match-p "\\`https?://[^ \t\n]+\\'" text) text)))

;;;;; Where the browser is sent

;; A file behind a sign-in that only the browser has -- a document in a
;; library, a wiki page its site exports -- is downloaded by the browser
;; from a URL that says to, which the site's entry in the configuration of
;; script/org-moor-front.ps1 gives.  The URL is only spelled differently
;; here; the browser is what goes there.

(defconst org-moor-script-directory
  (expand-file-name "script" (file-name-directory (or load-file-name buffer-file-name)))
  "The package's scripts.")

(defun org-moor-front-download (url done)
  "Call DONE with where a browser downloads URL from.
As script/org-moor-front.ps1 says, by the configuration it keeps sites in.
With (DOWNLOAD DIR): the URL to send the browser to, and the directory
it downloads into; or with nil, when the script says none, or cannot be
asked.  What it writes as errors is not read.  See
`org-moor-browser-url-function'."
  (let ((script (expand-file-name "org-moor-front.ps1" org-moor-script-directory))
        (out (generate-new-buffer " *org-moor-resolve*"))
        (err (generate-new-buffer " *org-moor-resolve-errors*")))
    (unless (and (file-readable-p script)
                 (ignore-errors
                   (make-process
                    :name "org-moor-resolve"
                    :buffer out
                    :stderr err
                    :command `("powershell.exe" "-NoProfile" "-ExecutionPolicy" "Bypass"
                               "-File" ,script "-Resolve" ,url)
                    :coding (cons 'utf-8 (org-moor-argument-coding))
                    :noquery t
                    :sentinel
                    (lambda (proc _event)
                      (unless (process-live-p proc)
                        (let ((lines (with-current-buffer out
                                       (split-string (buffer-string) "\r?\n" t))))
                          (kill-buffer out)
                          (when-let* ((errors (get-buffer-process err)))
                            (delete-process errors))
                          (kill-buffer err)
                          (funcall done (pcase lines
                                          (`(,download ,dir) (list download dir))))))))))
      (kill-buffer out)
      (kill-buffer err)
      (funcall done nil))))

(defcustom org-moor-browser-url-function
  (and (eq system-type 'windows-nt) #'org-moor-front-download)
  "Function telling where the browser is sent for a URL's replica, or nil.
Called with a URL and a function it calls, once it knows, with
\(DOWNLOAD DIR), the URL the browser downloads the replica from and the
directory it downloads into, or with nil for the URL itself, saved into
`org-moor-downloads-directory'.  On Windows, the script for the
document in front is asked, by the configuration it keeps sites in."
  :type '(choice function (const nil))
  :group 'org-moor)

;;;;; Waiting for a replica

(defconst org-moor-partial-download-regexp
  "\\.\\(crdownload\\|part\\|partial\\|tmp\\|download\\)\\'"
  "Names a browser gives a download until it is whole.")

(defvar org-moor--waiting nil
  "The replica waited for, or nil: a plist.
:record, the source; :dir, where it is to arrive; :since, the time from
which a file there is new; :seen, the files said not to be it; :file and
:size, the newest file and its size when last looked at; :timer.")

(defun org-moor--arrived (dir since seen)
  "Return the newest file come into DIR since SINCE, not in SEEN, or nil.
Nil too while a download there is not yet whole."
  (let ((new (seq-filter
              (lambda (f)
                (and (file-regular-p f)
                     (not (member f seen))
                     (not (time-less-p (file-attribute-modification-time
                                        (file-attributes f))
                                       since))))
              (directory-files dir t "\\`[^.]"))))
    (unless (seq-some (lambda (f) (string-match-p org-moor-partial-download-regexp f))
                      new)
      (car (sort new #'file-newer-than-file-p)))))

(defun org-moor--waiting-line ()
  "Return what the mode line says of the replica waited for, or nil."
  (when org-moor--waiting
    (propertize (format " moor:waiting[%s]"
                        (plist-get (plist-get org-moor--waiting :record) :title))
                'face 'warning
                'help-echo "org-moor waits for this replica; M-x org-moor-stop-waiting")))

(defvar org-moor--mode-line '(:eval (org-moor--waiting-line))
  "What `global-mode-string' is given while a replica is waited for.")
(put 'org-moor--mode-line 'risky-local-variable t)

(defun org-moor-stop-waiting ()
  "Stop waiting for the replica that is waited for."
  (interactive)
  (when org-moor--waiting
    (cancel-timer (plist-get org-moor--waiting :timer))
    (message "org-moor: no longer waiting for %s"
             (plist-get (plist-get org-moor--waiting :record) :title))
    (setq org-moor--waiting nil))
  (setq global-mode-string (delete 'org-moor--mode-line global-mode-string))
  (force-mode-line-update t))

(defun org-moor--take-arrived (file)
  "Take FILE as the replica waited for, and stop waiting."
  (let* ((record (plist-get org-moor--waiting :record))
         (name (file-name-nondirectory file)))
    (org-moor-stop-waiting)
    (rename-file file (expand-file-name name (plist-get record :dir)) t)
    (org-moor-record-replica (org-moor--record-in (plist-get record :dir) (plist-get record :id))
                             name 'web)
    (message "org-moor: %s is the replica of %s" name (plist-get record :title))))

(defun org-moor--wait-tick ()
  "Look for the replica waited for, and ask about a file come whole.
Whole once its size stays the same from one look to the next.  Not while
the minibuffer is in use: it is asked the next time."
  (when-let* ((w org-moor--waiting))
    (condition-case err
        (let* ((file (org-moor--arrived (plist-get w :dir) (plist-get w :since)
                                        (plist-get w :seen)))
               (size (and file (file-attribute-size (file-attributes file)))))
          (if (and file (equal file (plist-get w :file)) (eql size (plist-get w :size))
                   (> size 0) (not (active-minibuffer-window)))
              (if (y-or-n-p (format "Take %s as the replica of %s? "
                                    (file-name-nondirectory file)
                                    (plist-get (plist-get w :record) :title)))
                  (org-moor--take-arrived file)
                (setq org-moor--waiting
                      (plist-put w :seen (cons file (plist-get w :seen)))))
            (setq org-moor--waiting (plist-put (plist-put w :file file) :size size))))
      (error (org-moor-stop-waiting)
             (message "org-moor: %s" (error-message-string err))))))

(defun org-moor-wait-for-replica (record &optional dir since)
  "Wait for RECORD's replica to arrive in DIR, a file come there since SINCE.
DIR is `org-moor-downloads-directory' when nil, and SINCE now.  A file
come whole is asked about before it is taken, since what else arrives
there meanwhile is not told from it otherwise; one said not to be it is
passed over.  It is waited for until it is taken, or
`org-moor-stop-waiting'; the mode line says so meanwhile.  One replica
is waited for at a time: you are asked before another is let go of."
  (when (and org-moor--waiting
             (not (equal (plist-get (plist-get org-moor--waiting :record) :id)
                         (plist-get record :id))))
    (unless (y-or-n-p (format "Waiting for %s; wait for %s instead? "
                              (plist-get (plist-get org-moor--waiting :record) :title)
                              (plist-get record :title)))
      (user-error "org-moor: still waiting for %s"
                  (plist-get (plist-get org-moor--waiting :record) :title))))
  (org-moor-stop-waiting)
  (let ((dir (expand-file-name (or dir org-moor-downloads-directory))))
    (unless (file-directory-p dir)
      (user-error "org-moor: no directory %s to wait in" dir))
    (setq org-moor--waiting
          (list :record record :dir dir
                ;; A browser may date what it saves a moment before it says so.
                :since (time-subtract (or since (current-time)) 2)
                :timer (run-with-timer 1 1 #'org-moor--wait-tick)))
    (add-to-list 'global-mode-string 'org-moor--mode-line t)
    (force-mode-line-update t)
    (message "org-moor: waiting for the replica of %s in %s"
             (plist-get record :title) (abbreviate-file-name dir))))

(defun org-moor-open-in-browser (record)
  "Open RECORD's origin in the browser, to save its replica, and wait for it.
In the browser `browse-url' opens, the one you read in; sent where
`org-moor-browser-url-function' says for a file to download.  Save the
page there with SingleFile, or let the file download, and say it is the
one when asked; see `org-moor-wait-for-replica'."
  (let ((url (plist-get record :origin-url)))
    (unless (and url (string-match-p "\\`https?://" url))
      (user-error "org-moor: %s has no URL to open" (plist-get record :title)))
    (let ((go (lambda (how)
                (let ((since (current-time)))
                  (browse-url (url-encode-url (or (car how) url)))
                  (org-moor-wait-for-replica record (cadr how) since)))))
      (if org-moor-browser-url-function
          (funcall org-moor-browser-url-function url go)
        (funcall go nil)))))

;;;;; Taking a URL

(defun org-moor-take-url-to (url sources &optional title)
  "Take URL into SOURCES as a source titled TITLE, and return its record.
Its replica is awaited: nothing is fetched.  TITLE is the file's name,
for a URL spelled as one, else the URL, when nil.  Nothing is written in
Org."
  (let* ((url (org-moor-url-unwrap url))
         (name (org-moor-url-file-name url))
         (title (or title (and name (file-name-sans-extension name)) url)))
    (org-moor-new-source title url sources)))

;;;###autoload (autoload 'org-moor-take-url "org-moor" nil t)
(defun org-moor--awaited-shown (url pick)
  "Return the source whose replica of URL is awaited, for a page in front, or nil.
The one waited for; else one awaited in this buffer's sources, where the
page would be taken -- in Org, and PICK nil."
  (let ((waited (plist-get org-moor--waiting :record)))
    (if (and waited (equal (plist-get waited :origin-url) url))
        waited
      (and (not pick) (derived-mode-p 'org-mode)
           (org-moor--awaited-from (org-moor-sources) url)))))

(defun org-moor-take-url (url &optional title pick shown dir since)
  "Take URL as a source, titled TITLE, and have the browser save its replica.
The browser opens it, see `org-moor-open-in-browser'; with SHOWN, it is
open there already -- a page in front -- and the replica is waited for
alone, in DIR since SINCE, see `org-moor-wait-for-replica'.  A page in
front whose replica is awaited already -- opened from `org-moor-awaiting',
say -- is that source: nothing is taken again, and its replica is waited
for.  At point when in Org; with PICK, at a chosen heading, as
`org-moor-take-file' takes a file.  Interactively the URL on the
clipboard is offered.  Return a marker at the link, or nil when nothing
was taken."
  (interactive
   (list (read-string "URL: " (org-moor--clipboard-url)) nil current-prefix-arg))
  (let* ((url (org-moor-url-unwrap url))
         (awaited (and shown (org-moor--awaited-shown url pick))))
    (if awaited
        (let ((since (or since
                         (and (equal (plist-get (plist-get org-moor--waiting :record) :id)
                                     (plist-get awaited :id))
                              (plist-get org-moor--waiting :since)))))
          (org-moor-wait-for-replica awaited dir since)
          (message "org-moor: %s is taken already; waiting for its replica"
                   (plist-get awaited :title))
          nil)
      (let* ((name (org-moor-url-file-name url))
             (title (read-string "Title: " (or title
                                               (and name (file-name-sans-extension name)))))
             (record nil)
             (link (org-moor--take-at (org-moor--target pick) title
                                      (lambda (sources)
                                        (setq record (org-moor-take-url-to url sources title))))))
        (if shown
            (org-moor-wait-for-replica record dir since)
          (org-moor-open-in-browser record))
        link))))

;;;; From outside Emacs

(defun org-moor-take-machine-readable (record file url &optional via)
  "Give RECORD the file FILE as what a machine reads of it, fetched from URL.
FILE is copied into its directory; VIA says how it was fetched, `agent'
when nil.  What was made readable from its replica goes.  Return RECORD."
  (let* ((name (file-name-nondirectory file))
         (old (and (plist-get record :machine-readable-made-from)
                   (plist-get record :machine-readable))))
    (unless (file-regular-p file)
      (error "org-moor: not a file: %s" file))
    (when (and old (not (equal (file-name-nondirectory old) name)))
      (delete-file old))
    (copy-file file (expand-file-name name (plist-get record :dir)) t)
    (dolist (key '(:machine-readable-made-from :machine-readable-made-by
                   :machine-readable-made))
      (setq record (org-moor-record-set record key nil)))
    (let* ((record (org-moor-record-set record :machine-readable-url url))
           (record (org-moor-record-set record :machine-readable-file name))
           (record (org-moor-record-set record :machine-readable-via (format "%s" (or via 'agent)))))
      (org-moor-record-set record :machine-readable-taken (org-moor-now)))))

(defun org-moor--awaited-from (sources url)
  "Return the source in SOURCES whose replica of URL is awaited, or nil."
  (seq-find (lambda (r) (and (not (plist-get r :replica-file))
                             (equal (plist-get r :origin-url) url)))
            (org-moor-sources-in sources)))

(defun org-moor--take-what (what sources title &optional origin)
  "Take WHAT, a file or a URL, into SOURCES as a source titled TITLE.
Return its record.  A file is copied, never moved, and titled by its
name unless TITLE is given; ORIGIN is where it is seen, when not the file
itself.  A URL's replica is awaited, see `org-moor-take-url-to'; one
awaited already in SOURCES is that source.  The directory SOURCES is in
must be there already: a name mangled on the way in is not made into a
tree of its own."
  (let* ((sources (file-name-as-directory (expand-file-name sources)))
         (parent (file-name-directory (directory-file-name sources))))
    (unless (file-directory-p parent)
      (error "org-moor: not a directory: %s" parent))
    (if (string-match-p "\\`https?://" what)
        (or (org-moor--awaited-from sources (org-moor-url-unwrap what))
            (org-moor-take-url-to what sources title))
      (unless (file-regular-p what)
        (error "org-moor: not a file: %s" what))
      (org-moor-take-file-to what sources title 'agent nil origin))))

(defvar server-use-tcp)
(defvar server-auth-dir)
(defvar server-socket-dir)
(declare-function server-eval-at "server" (server form))

(defun org-moor--ask-server (file form)
  "Return what FORM comes to in the Emacs whose server is FILE.
Signal `file-error' when it cannot be reached.  FILE is the server's
socket, or its authentication file when it is reached by TCP.

FORM goes, and what it comes to comes back, printed in ASCII alone.
`server-eval-at' sends its bytes as they are, the server decodes them by
its `file-name-coding-system', and the answer is decoded as
`emacs-internal': where that coding is not UTF-8, as cp932 is, a name
that is not ASCII would arrive as another name."
  (require 'server)
  (let ((server-use-tcp (file-regular-p file))
        (server-auth-dir (file-name-directory file))
        (server-socket-dir (file-name-directory file))
        (print-escape-multibyte t)
        (print-escape-nonascii t))
    (condition-case err
        (read (server-eval-at
               (file-name-nondirectory file)
               `(let ((print-escape-multibyte t)
                      (print-escape-nonascii t))
                  (prin1-to-string ,form))))
      (server-return-invalid-read-syntax (signal (car err) (cdr err)))
      (error (signal 'file-error (list "Cannot reach Emacs" file
                                       (error-message-string err)))))))

(defun org-moor-awaited-for-agent (title)
  "Tell the user an agent took TITLE, whose replica they are to save.
In the Emacs the user works in, asked by `org-moor-batch-take' --server."
  (message "%s" (propertize
                 (format "org-moor: an agent took %s; its replica awaits you -- M-x org-moor-awaiting"
                         title)
                 'face 'warning))
  t)

(defun org-moor--batch-options ()
  "Pop the options at the head of `command-line-args-left', as an alist."
  (let (options)
    (while (and command-line-args-left
                (string-match "\\`--\\([a-z-]+\\)=\\(.*\\)\\'" (car command-line-args-left)))
      (push (cons (intern (match-string 1 (car command-line-args-left)))
                  (match-string 2 (car command-line-args-left)))
            options)
      (pop command-line-args-left))
    options))

(defun org-moor-batch-take ()
  "Take a file or a web page into a sources directory, and print a link to it.
For an agent, which cannot ask Emacs:

  emacs --batch -L DIR -L ORG-DOCK-DIR -l org-moor \\
        -f org-moor-batch-take [--server=FILE] SOURCES REPLICA [TITLE]
        [--origin-url=URL] [--machine-readable=FILE --machine-readable-url=URL]

SOURCES is the sources directory.  REPLICA is a file -- copied, never
moved, and titled by its name unless TITLE is given; --origin-url is
where it is seen, when not the file -- or a URL, whose replica the user
has their browser save later: nothing is fetched.  --machine-readable is
a file a machine reads better than the replica, fetched from
--machine-readable-url.  In place of REPLICA, moor:ID takes nothing, and
says what source ID has.  Options may come before SOURCES or after
TITLE.

What is printed is a link to the source named by its title,
[[moor:ID][TITLE]], then what to read of it and its replica, as
`org-moor-agent-state-lines' tells them.  With --server, the Emacs whose
server FILE is is told of a replica awaited, so that the user sees it."
  (let* ((options (org-moor--batch-options))
         (sources (expand-file-name (or (pop command-line-args-left)
                                        (error "org-moor: no sources directory"))))
         (what (or (pop command-line-args-left)
                   (error "org-moor: no file or URL to take")))
         (title (and command-line-args-left
                     (not (string-prefix-p "--" (car command-line-args-left)))
                     (pop command-line-args-left)))
         (options (append options (org-moor--batch-options)))
         (option (lambda (name) (alist-get name options)))
         (again (and (string-match "\\`moor:\\([^]:\n]+\\)\\'" what)
                     (match-string 1 what)))
         (org-moor--agent t)
         (record
          (if again
              (or (org-moor-record again sources)
                  (error "org-moor: no source %s in %s" again sources))
            (org-moor--take-what (if (string-match-p "\\`https?://" what)
                                     what
                                   (expand-file-name what))
                                 sources title (funcall option 'origin-url)))))
    (when-let* ((file (funcall option 'machine-readable)))
      (setq record (org-moor-take-machine-readable
                    record (expand-file-name file) (funcall option 'machine-readable-url))))
    (when (and (funcall option 'server) (not again) (not (plist-get record :replica)))
      (condition-case err
          (org-moor--ask-server (funcall option 'server)
                                `(org-moor-awaited-for-agent ,(plist-get record :title)))
        (file-error (message "org-moor: %s" (error-message-string err)))))
    ;; In UTF-8, as an agent reads it, rather than the system's code page.
    (let ((coding-system-for-write 'utf-8))
      (princ (concat (org-moor-link record) "\n"
                     (org-moor-agent-state-lines record))))))

(defun org-moor-agent-state-lines (record)
  "Return the lines telling an agent what of RECORD it reads, and its replica."
  (let ((read (org-moor-file-for-machine record))
        (replica (plist-get record :replica)))
    (concat
     (if read
         (concat "Read: " read "\n")
       "Nothing to read yet: the replica is awaited from the user; do not guess what it says.\n")
     (if replica
         (concat "Replica: " replica "\n")
       "Replica: awaited from the user, who has their browser save it later.\n"))))

;;;; org-protocol

;;;###autoload (autoload 'org-moor-protocol "org-moor" nil nil)
(defun org-moor-protocol (info)
  "org-protocol handler.  INFO is a plist from the protocol URL.

  org-protocol://moor?url=URL&title=TITLE
  org-protocol://moor?url=URL&title=TITLE&arriving=DIR&since=SECONDS
  org-protocol://moor?path=PATH&title=TITLE&origin=ORIGIN&via=VIA

The first takes a page or a file on the web that is open in the browser,
in front: its replica is what the browser saves of it now, waited for in
`org-moor-downloads-directory'.  The second takes a file on the web that
the browser is downloading into DIR: its replica is the file come there
since SECONDS, a Unix time -- see `org-moor-wait-for-replica'.  The third
takes a file on disk, as does a file: URL, which a browser shows for a
file it opened; ORIGIN, when given, is where that file was opened from.
Either arrives from outside Emacs, and goes where the buffer Emacs shows
in front is left: at its point when that is Org, as a take typed there
would, else a heading is picked.  Emacs is brought to the front to ask."
  (select-frame-set-input-focus (selected-frame))
  (let ((url (plist-get info :url))
        (path (plist-get info :path))
        (title (plist-get info :title)))
    ;; A file opened in a browser has a file: URL, and is a file on disk.
    (when (and url (string-match "\\`file:" url))
      (setq path (org-moor-file-url-path url) url nil))
    ;; The server answers in no buffer of the user's; the one in front is
    ;; where they left off.
    (with-current-buffer (window-buffer (selected-window))
      (cond
       (path
        (let ((org-moor--default-title title))
          (org-moor-take-file path nil (plist-get info :via) nil
                              (plist-get info :origin))))
       (url (org-moor-take-url
             url title nil t (plist-get info :arriving)
             (and (plist-get info :since)
                  (seconds-to-time (string-to-number (plist-get info :since))))))
       (t (message "org-moor: protocol URL had no url= or path=")))))
  nil)

;;;###autoload (autoload 'org-moor-protocol-setup "org-moor" nil nil)
(defun org-moor-protocol-setup ()
  "Register the `moor' org-protocol."
  (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist
               '("org-moor" :protocol "moor" :function org-moor-protocol)))

(provide 'org-moor-take)
;;; org-moor-take.el ends here
