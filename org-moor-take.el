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
  (let* ((record (org-moor-new-source title origin (or via 'clipboard) sources))
         (name (org-moor--file-name-of title extension)))
    (condition-case err
        (funcall write (expand-file-name name (plist-get record :dir)))
      (error
       (delete-directory (plist-get record :dir) t)
       (signal (car err) (cdr err))))
    (org-moor-record-set record :file name)))

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
                                      (or origin file) (or via 'file) sources))
         (replica (expand-file-name name (plist-get record :dir))))
    (condition-case err
        (if move
            (rename-file file replica)
          (copy-file file replica nil t))
      (error
       (delete-directory (plist-get record :dir) t)
       (signal (car err) (cdr err))))
    (org-moor-record-set record :file name)))

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
  "Non-nil while a source is taken for an agent, in the Emacs the user works in.
What is said of its replica then goes to *Messages* alone, not over what
the user is doing, since the agent is told it; and a page single-file
saves for it waits behind the user's.  Bound by `org-moor-agent-take',
and again by what is called back for it later.")

(defun org-moor--say (format-string &rest args)
  "Say FORMAT-STRING with ARGS as `message' does; in *Messages* alone for an agent."
  (let ((inhibit-message (or inhibit-message org-moor--agent)))
    (apply #'message format-string args)))

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
     :coding (cons 'utf-8 (org-moor-argument-coding))
     :command `(,org-moor-monolith-program "-s" ,@(org-moor--cookie-args "-C")
                "-o" ,file ,(url-encode-url url))
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (funcall done (and (zerop (process-exit-status proc))
                                      (file-exists-p file))))))))

(defconst org-moor-snapshot-file "page.html"
  "Name of a web page's replica in its directory.")

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

(defun org-moor-replica-name (url type &optional disposition)
  "Return the name a replica of URL is given, sent as TYPE with DISPOSITION.
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
  "Fetch URL to FILE by curl; call DONE with success, type and disposition.
What it prints, the headers, is read as UTF-8, as a name in them is sent."
  (make-process
   :name "org-moor-fetch"
   :buffer (generate-new-buffer " *org-moor-fetch*")
   :coding (cons 'utf-8 (org-moor-argument-coding))
   :command `(,org-moor-curl-program "-fsSL" ,@(org-moor--cookie-args "-b")
              "-o" ,file "-w" "%{content_type}\\n%header{content-disposition}"
              ,(url-encode-url url))
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
  "Keep a replica of URL in DIR, then call DONE with its name, or nil.
What it is, is what it sends: a page is kept by
`org-moor-snapshot-function', or else as its HTML; a file under the name
`org-moor-replica-name' gives it.  A page sent for a URL spelled as a file
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
              (name (and type (org-moor-replica-name url type disposition)))
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
  "Seconds to wait for a replica a browser is downloading."
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
  "Return a way to keep a replica that a browser is downloading into DIR.
The replica is the file come into DIR since SINCE, a time, once it is whole;
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

;; A file behind a sign-in that only the browser has can be downloaded by
;; the browser, from a URL that says to: where the script for the document
;; in front has a browser keep it, see script/org-moor-front.ps1.  A URL
;; taken in Emacs is kept the same way, by the same configuration, which
;; the script is asked about.

(defconst org-moor-script-directory
  (expand-file-name "script" (file-name-directory (or load-file-name buffer-file-name)))
  "The package's scripts.")

;; A page that only a browser shows whole -- one its scripts draw, behind a
;; sign-in -- is saved by SingleFile's command line, in a browser of its own
;; that shows no window: signed in by a profile kept for it, so that nothing
;; comes in front of what you are doing while it is saved.

(defcustom org-moor-singlefile-program "single-file"
  "The single-file executable, SingleFile's command line."
  :type 'string
  :group 'org-moor)

(defcustom org-moor-singlefile-browser nil
  "The Chrome or Chromium executable single-file drives, or nil.
Nil leaves single-file to find one where browsers are installed."
  :type '(choice (const :tag "Found by single-file" nil) file)
  :group 'org-moor)

(defcustom org-moor-singlefile-profile nil
  "The browser profile directory single-file saves pages signed in as, or nil.
`org-moor-sign-in' makes it and signs it in.  A page is then saved in
the browser kept open in it -- see `org-moor-singlefile-browser-idle' --
or, without one, with a copy of it, the profile itself left as it is.
Nil saves pages signed in nowhere.  It is a sign-in, so it is to be
where only you can read it."
  :type '(choice (const :tag "None" nil) directory)
  :group 'org-moor)

(defcustom org-moor-singlefile-arguments nil
  "More arguments given single-file, before the URL.
They come after the ones given here, and so are taken over them: say,
\='(\"--browser-wait-until\" \"InteractiveTime\") to have a page taken as
loaded by the browser's measure, in place of
`org-moor-singlefile-when-ready'."
  :type '(repeat string)
  :group 'org-moor)

(defcustom org-moor-singlefile-when-ready t
  "Non-nil to have single-file save a page once the page is ready.
The page is taken as loaded at DOMContentLoaded, and script/org-moor-ready.js
holds the saving until the page's own requests are answered and its
document stops changing, rather than single-file waiting for the network
to go quiet, which a page that keeps fetching never does -- for
--browser-load-max-time, a minute -- or for InteractiveTime, five seconds
of nothing after the page is drawn.  Nil leaves single-file to wait as
`org-moor-singlefile-arguments' says."
  :type 'boolean
  :group 'org-moor)

(defcustom org-moor-singlefile-profile-caches
  '("Default/Cache" "Default/Code Cache" "Default/GPUCache"
    "Default/DawnGraphiteCache" "Default/DawnWebGPUCache"
    "Default/Service Worker/CacheStorage" "Default/Service Worker/ScriptCache"
    "GrShaderCache" "GraphiteDawnCache" "ShaderCache" "GPUPersistentCache"
    "component_crx_cache" "extensions_crx_cache"
    "optimization_guide_model_store")
  "Directories of `org-moor-singlefile-profile' let go once signed in.
Relative to the profile.  single-file copies the whole profile for each
page it saves, and these are caches the browser fills again: what a
sign-in is -- cookies, the key they are kept with, extensions -- is not
among them.  Removed by `org-moor-sign-in' once the browser is quit."
  :type '(repeat string)
  :group 'org-moor)

(defcustom org-moor-singlefile-browser-idle 120
  "Seconds the browser single-file saves pages in is kept after the last page.
With `org-moor-singlefile-browser', that browser is started here, with
no window, in `org-moor-singlefile-profile' itself, the first time a
page is saved, and single-file saves each page in it by --browser-server:
it starts no browser of its own and copies no profile for it, which is
most of the time a page takes.  The browser keeps what the sites send
it, as a browser you read in does.  It is quit once this many seconds
pass with no page being saved, when Emacs is, and before
`org-moor-sign-in' opens the profile.  While it runs, any program here
can drive it through its debugging port, as one can a browser
single-file starts while it saves a page.  Nil, or with no
`org-moor-singlefile-browser', has single-file start a browser for each
page, in a copy of the profile."
  :type '(choice (const :tag "A browser for each page" nil) integer)
  :group 'org-moor)

(defcustom org-moor-singlefile-timeout 300
  "Seconds single-file is given to save a page before it is stopped.
single-file stops waiting for a page by its own --browser-load-max-time
and --browser-capture-max-time; this is for a browser that does not
answer at all, which would else keep the source pending for good."
  :type 'integer
  :group 'org-moor)

(defun org-moor--singlefile-profile-trim ()
  "Remove `org-moor-singlefile-profile-caches' from the profile."
  (let ((profile (expand-file-name org-moor-singlefile-profile)))
    (dolist (cache org-moor-singlefile-profile-caches)
      (let ((dir (expand-file-name cache profile)))
        (when (and (file-directory-p dir) (file-in-directory-p dir profile))
          (condition-case err
              (delete-directory dir t)
            (file-error (message "org-moor: %s left: %s" dir (error-message-string err)))))))))

(defconst org-moor--location-script
  (expand-file-name "org-moor-location.js" org-moor-script-directory)
  "A script run in a page single-file saves, writing where the page is.
single-file writes the URL it was given, not the one a sign-in sent the
browser on to; this one writes that, as a meta element named
org-moor-location.")

(defconst org-moor--ready-script
  (expand-file-name "org-moor-ready.js" org-moor-script-directory)
  "A script run in a page single-file saves, holding it until the page is ready.
See `org-moor-singlefile-when-ready'.")

(defconst org-moor--browser-quiet-args '("--disable-quic")
  "Arguments every browser started for single-file is given.
HTTP/3 has the browser open UDP sockets on every address, which a
firewall asks about for a browser it has no rule for; without it, the
browser listens on nothing but its debugging port, on this machine.")

(defun org-moor--singlefile-browser-args ()
  "Return the arguments telling single-file which browser to drive."
  (and org-moor-singlefile-browser
       (list "--browser-executable-path" (expand-file-name org-moor-singlefile-browser))))

(defun org-moor-singlefile-location (file)
  "Return where the page saved in FILE was when it was saved, or nil.
As `org-moor--location-script' wrote it there."
  ;; As bytes: a page with its images in it runs to megabytes, of which
  ;; only the one element is read, and only it is decoded.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (when (re-search-forward
           "<meta name=\"?org-moor-location\"? content=\\(?:\"\\([^\"]*\\)\"\\|\\([^ >]*\\)\\)"
           nil t)
      (string-replace "&amp;" "&" (decode-coding-string
                                   (or (match-string 1) (match-string 2)) 'utf-8)))))

(defun org-moor-signed-out-p (url location)
  "Non-nil when a page asked for at URL was shown at LOCATION, elsewhere.
A browser not signed in is sent on to a sign-in page, which is often on
a host of its own; a page that stays on its host is taken for the page."
  (and location
       (not (equal (url-host (url-generic-parse-url url))
                   (url-host (url-generic-parse-url location))))))

(defun org-moor-signed-out-by-host (url location done)
  "Call DONE with whether URL, shown at LOCATION, was a sign-in page.
As `org-moor-signed-out-p' tells it.  For `org-moor-signed-out-function'."
  (funcall done (org-moor-signed-out-p url location)))

(defcustom org-moor-signed-out-function
  (if (eq system-type 'windows-nt) #'org-moor-front-signed-out #'org-moor-signed-out-by-host)
  "Function telling whether a page single-file saved was a sign-in page.
Called with the URL asked for, the URL the page was at when saved, or nil
when that is not known, and a function it calls with non-nil when the
page was a sign-in page in place of the one asked for.  On Windows, the
script for the document in front is asked whether the page is still one
of its entry, as a sign-in page on the site's own host is not."
  :type 'function
  :group 'org-moor)

;;;;; The browser kept for single-file

(defconst org-moor--singlefile-server-args
  '("--headless" "--remote-debugging-port=0"
    "--no-first-run" "--no-default-browser-check"
    ;; What single-file starts a browser with for a page to be saved whole
    ;; -- frames loaded however far down, timers run with no window --
    ;; and with nothing fetched for the browser itself.
    "--disable-features=LazyFrameLoading,AutoExpandDetailsElement,PaintHolding,Translate,MediaRouter"
    "--disable-background-timer-throttling" "--disable-renderer-backgrounding"
    "--disable-backgrounding-occluded-windows" "--disable-component-update"
    "--mute-audio" "--window-size=1280,720")
  "Arguments the browser kept for single-file is started with.
Not single-file's --no-sandbox: started here, the browser keeps it.")

(defvar org-moor--singlefile-server nil
  "The browser process kept for single-file, or nil.")

(defvar org-moor--singlefile-server-url nil
  "The address single-file reaches the browser kept for it at, once it answers.")

(defvar org-moor--singlefile-server-waiting nil
  "Functions waiting for the browser kept for single-file to answer.")

(defvar org-moor--singlefile-server-timer nil
  "The timer quitting the browser kept for single-file, or nil.")

(defvar org-moor--singlefile-server-queue nil
  "Pages waiting for the browser kept for single-file.
Each is (URL FILE DONE AGAIN AGENT): AGAIN is non-nil for a page being
saved a second time, and AGENT for one saved for an agent, see
`org-moor--agent'.")

(defvar org-moor--singlefile-server-busy nil
  "Non-nil while single-file saves a page in the browser kept for it.")

(defvar org-moor--signing-in nil
  "Non-nil while `org-moor-sign-in' has the profile open in a browser.
The pages queued for the browser kept for single-file wait meanwhile, to
be saved signed in once it is quit.")

(defun org-moor--singlefile-server-p ()
  "Non-nil when single-file saves pages in a browser kept for it."
  (and org-moor-singlefile-browser org-moor-singlefile-browser-idle t))

(defun org-moor--singlefile-server-answered (url)
  "Tell what waits for the browser kept for single-file that it is at URL.
Nil when it is not there."
  (setq org-moor--singlefile-server-url url)
  (let ((waiting (nreverse org-moor--singlefile-server-waiting)))
    (setq org-moor--singlefile-server-waiting nil)
    (dolist (then waiting) (funcall then url))))

(defun org-moor--singlefile-server-gone (proc)
  "Let go of PROC, the browser kept for single-file, which has quit.
Its temporary profile goes with it."
  (when-let* ((temp (process-get proc 'org-moor-temp)))
    (ignore-errors (delete-directory temp t)))
  (when (eq proc org-moor--singlefile-server)
    (setq org-moor--singlefile-server nil)
    (org-moor--singlefile-server-answered nil)))

(defun org-moor--singlefile-server-stop ()
  "Quit the browser kept for single-file, if it runs."
  (when (timerp org-moor--singlefile-server-timer)
    (cancel-timer org-moor--singlefile-server-timer))
  (setq org-moor--singlefile-server-timer nil)
  (when-let* ((proc org-moor--singlefile-server))
    (when (process-live-p proc)
      (delete-process proc)
      ;; So that the profile is free for the browser started next in it.
      (with-timeout (5 nil)
        (while (process-live-p proc) (accept-process-output nil 0.05))))
    (org-moor--singlefile-server-gone proc)))

(defun org-moor--singlefile-server-rest ()
  "Have the browser kept for single-file quit after its idle time.
Counted from now, unless a page waits for it, or is saved in it, then."
  (when (timerp org-moor--singlefile-server-timer)
    (cancel-timer org-moor--singlefile-server-timer))
  (setq org-moor--singlefile-server-timer
        (and org-moor--singlefile-server
             (run-with-timer
              org-moor-singlefile-browser-idle nil
              (lambda ()
                (setq org-moor--singlefile-server-timer nil)
                (unless (or org-moor--singlefile-server-busy
                            org-moor--singlefile-server-queue)
                  (org-moor--singlefile-server-stop)))))))

(defun org-moor--singlefile-server-port (file)
  "Return the port the browser wrote to FILE, its DevToolsActivePort, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (ignore-errors (insert-file-contents file))
      (goto-char (point-min))
      (and (looking-at "\\([0-9]+\\)\n") (match-string 1)))))

(defun org-moor--profile-in-use-p (dir)
  "Non-nil when a browser runs in the profile DIR.
Chrome holds the profile's lockfile open on Windows, and elsewhere points
its SingletonLock at HOST-PID."
  (if (memq system-type '(windows-nt ms-dos))
      (let ((lock (expand-file-name "lockfile" dir)))
        ;; One a browser that is gone left behind can be deleted.
        (and (file-exists-p lock)
             (condition-case nil (progn (delete-file lock) nil)
               (file-error t))))
    (let ((target (file-symlink-p (expand-file-name "SingletonLock" dir))))
      (and (stringp target)
           (string-match "-\\([0-9]+\\)\\'" target)
           (process-attributes (string-to-number (match-string 1 target)))
           t))))

(defun org-moor--profile-in-use-message (dir)
  "Return the words saying a browser left running has the profile DIR open."
  (format "org-moor: a browser left running has %s open -- quit the one started with --user-data-dir=%s"
          dir dir))

(defun org-moor--singlefile-server-start ()
  "Start the browser kept for single-file, and wait for it to answer.
Return its process, or nil when it cannot be started.  It is not when a
browser left running -- by an Emacs that was stopped, not quit -- has
the profile open: a browser started there would hand its page to that
one and quit.  Pages are then saved with a copy of the profile, and the
echo area says so."
  (let* ((temp (and (not org-moor-singlefile-profile)
                    (make-temp-file "org-moor-browser" t)))
         (dir (directory-file-name
               (or temp (expand-file-name org-moor-singlefile-profile))))
         (port-file (expand-file-name "DevToolsActivePort" dir))
         (proc nil))
    (make-directory dir t)
    ;; One a browser quit without a word left behind names its port.
    (ignore-errors (delete-file port-file))
    (if (and (not temp) (org-moor--profile-in-use-p dir))
        (message "%s" (propertize
                       (concat (org-moor--profile-in-use-message dir)
                               "; pages are saved with a copy of it meanwhile")
                       'face 'warning))
      (condition-case err
          (setq proc (make-process
                      :name "org-moor-singlefile-browser"
                      :buffer nil
                      :coding (cons 'utf-8 (org-moor-argument-coding))
                      :noquery t
                      :command `(,(expand-file-name org-moor-singlefile-browser)
                                 ,(concat "--user-data-dir=" dir)
                                 ,@org-moor--singlefile-server-args
                                 ,@org-moor--browser-quiet-args
                                 "about:blank")
                      :sentinel (lambda (proc _event)
                                  (unless (process-live-p proc)
                                    (org-moor--singlefile-server-gone proc)))))
        (error (message "org-moor: %s" (error-message-string err))
               (when temp (ignore-errors (delete-directory temp t))))))
    (when proc
      (process-put proc 'org-moor-temp temp)
      (setq org-moor--singlefile-server proc
            org-moor--singlefile-server-url nil)
      (add-hook 'kill-emacs-hook #'org-moor--singlefile-server-stop)
      (let ((deadline (+ (float-time) 30)))
        (letrec ((poll
                  (lambda ()
                    (when (eq proc org-moor--singlefile-server)
                      (if-let* ((port (org-moor--singlefile-server-port port-file)))
                          (org-moor--singlefile-server-answered
                           (concat "http://127.0.0.1:" port))
                        (if (< (float-time) deadline)
                            (run-with-timer 0.1 nil poll)
                          (message "org-moor: the browser for single-file did not answer")
                          (org-moor--singlefile-server-stop)))))))
          (run-with-timer 0.1 nil poll))))
    proc))

(defun org-moor--singlefile-server (then)
  "Call THEN with the address of the browser kept for single-file.
It is started when it does not run; THEN is given nil when it cannot be."
  (cond
   ((not (process-live-p org-moor--singlefile-server))
    (push then org-moor--singlefile-server-waiting)
    (unless (org-moor--singlefile-server-start)
      (org-moor--singlefile-server-answered nil)))
   (org-moor--singlefile-server-url
    (funcall then org-moor--singlefile-server-url))
   (t (push then org-moor--singlefile-server-waiting))))

(defun org-moor--singlefile-server-next ()
  "Have single-file save the next page queued for the browser kept for it.
Unless it is saving one, or the profile is open to sign in."
  (unless (or org-moor--singlefile-server-busy org-moor--signing-in)
    (when-let* ((page (pop org-moor--singlefile-server-queue)))
      (setq org-moor--singlefile-server-busy t)
      (org-moor--singlefile-server
       (lambda (server)
         (let ((org-moor--agent (nth 4 page)))
           (org-moor--singlefile-run
            (nth 0 page) (nth 1 page) server
            (apply-partially #'org-moor--singlefile-server-saved
                             page server org-moor--singlefile-server))))))))

(defun org-moor--singlefile-server-enqueue (page)
  "Queue PAGE, of the form of `org-moor--singlefile-server-queue'.
The user's pages are saved before an agent's: after the user's queued,
before the first of an agent's; an agent's after every other."
  (let* ((queue org-moor--singlefile-server-queue)
         (before (if (nth 4 page)
                     queue
                   (seq-take-while (lambda (p) (not (nth 4 p))) queue))))
    (setq org-moor--singlefile-server-queue
          (append before (list page) (nthcdr (length before) queue)))))

(defun org-moor--singlefile-server-saved (page server browser saved)
  "Finish PAGE, of the queue, which single-file SAVED or did not.
In the browser at SERVER, the process BROWSER, or, nil, in one of its own.
A page not saved because the browser went is queued first, once, for
the browser started again; else its DONE is called.  Then the next."
  (pcase-let ((`(,url ,file ,done ,again ,agent) page))
    (setq org-moor--singlefile-server-busy nil)
    (unwind-protect
        (let ((org-moor--agent agent))
          (if (and (not saved) server (not again) (not (process-live-p browser)))
              (progn
                (org-moor--say "org-moor: the browser for single-file went; %s is saved again" url)
                (push (list url file done t agent) org-moor--singlefile-server-queue))
            (org-moor--singlefile-server-rest)
            (funcall done saved)))
      (org-moor--singlefile-server-next))))

;;;;; Saving a page

(defun org-moor--singlefile-command (url file server)
  "Return the single-file command saving the page at URL to FILE.
In the browser at SERVER, the address of one kept for it, or, nil, in
one of single-file's own."
  `(,org-moor-singlefile-program
    ,@(if server
          (list "--browser-server" server)
        `(,@(org-moor--singlefile-browser-args)
          ,@(and org-moor-singlefile-profile
                 (list "--browser-profile"
                       (expand-file-name org-moor-singlefile-profile)))
          ;; Joined, as a value that is an option itself.
          ,@(mapcar (lambda (arg) (concat "--browser-arg=" arg))
                    org-moor--browser-quiet-args)))
    "--browser-script" ,org-moor--location-script
    ;; Else a file there already has the page saved beside it.
    "--filename-conflict-action" "overwrite"
    ,@(and org-moor-singlefile-when-ready
           (list "--browser-wait-until" "DOMContentLoaded"
                 "--browser-wait-until-delay" "0"
                 "--browser-script" org-moor--ready-script))
    ,@org-moor-singlefile-arguments
    ,(url-encode-url url) ,file))

(defun org-moor--singlefile-output (buffer)
  "Return the last lines single-file wrote in BUFFER, for a message."
  (with-current-buffer buffer
    (let ((lines (last (split-string (buffer-string) "[\r\n]+" t "[ \t]+") 3)))
      (and lines (string-join lines " / ")))))

(defun org-moor--singlefile-run (url file server done)
  "Have single-file save the page at URL to FILE, then call DONE.
In the browser at SERVER, as `org-moor--singlefile-command' says.  DONE
is given non-nil when FILE has the page; when not, the echo area says
why.  single-file is stopped after `org-moor-singlefile-timeout'."
  (let* ((out (generate-new-buffer " *org-moor-singlefile*"))
         (timer nil)
         (agent org-moor--agent)
         (failed
          (lambda (why)
            (ignore-errors (delete-file file))
            (let ((org-moor--agent agent))
              (org-moor--say "%s" (propertize
                                   (format "org-moor: single-file did not save %s%s"
                                           url (if why (concat ": " why) ""))
                                   'face 'warning)))))
         (proc
          (condition-case err
              (make-process
               :name "org-moor-singlefile"
               :buffer out
               ;; What it writes is read only for a message, and an error the
               ;; system gives it comes in the system's code page.
               :coding (cons 'undecided (org-moor-argument-coding))
               :noquery t
               :command (org-moor--singlefile-command url file server)
               :sentinel
               (lambda (proc _event)
                 (unless (process-live-p proc)
                   (when timer (cancel-timer timer))
                   (let ((org-moor--agent agent)
                         (saved (and (zerop (process-exit-status proc))
                                     (file-exists-p file)
                                     (> (file-attribute-size (file-attributes file)) 0))))
                     (unless saved
                       (funcall failed (if (process-get proc 'org-moor-timeout)
                                           (format "in %d seconds" org-moor-singlefile-timeout)
                                         (org-moor--singlefile-output out))))
                     (kill-buffer out)
                     (funcall done saved)))))
            (error (kill-buffer out)
                   (funcall failed (error-message-string err))
                   (funcall done nil)
                   nil))))
    (when proc
      (setq timer (run-with-timer org-moor-singlefile-timeout nil
                                  (lambda ()
                                    (when (process-live-p proc)
                                      (process-put proc 'org-moor-timeout t)
                                      (delete-process proc))))))
    proc))

(defun org-moor--singlefile-checked (url file done saved)
  "Call DONE with what single-file SAVED of URL in FILE comes to.
As `org-moor-singlefile' says: a page saved is told from a sign-in page
by `org-moor-signed-out-function'."
  (if (not saved)
      (funcall done nil)
    (let ((location (org-moor-singlefile-location file))
          (agent org-moor--agent))
      (funcall org-moor-signed-out-function
               url location
               (lambda (out)
                 (let ((org-moor--agent agent))
                   (if (not out)
                       (funcall done t)
                     (org-moor--say "org-moor: %s went to %s, taken for a sign-in page"
                                    url location)
                     (delete-file file)
                     (funcall done 'signin))))))))

(defun org-moor-singlefile (url file done)
  "Save the page at URL to FILE by single-file, then call DONE.
With t when it is saved, `signin' when a sign-in page was shown in its
place -- see `org-moor-signed-out-function' -- and nil when it failed.
In the browser kept for it, as `org-moor-singlefile-browser-idle' says,
one page at a time: a page whose tab is not in front is not drawn, and
waits there all the same, the time it is given running out.  The user's
pages go before those of an agent, see `org-moor--agent'.  Should
that browser be gone when the page is not saved, it is saved once more,
first, in the browser started again.  Nothing is left in FILE but the
page."
  (let ((checked (apply-partially #'org-moor--singlefile-checked url file done)))
    (cond
     ((not (executable-find org-moor-singlefile-program))
      (message "%s" (propertize
                     (format "org-moor: no %s to save %s with"
                             org-moor-singlefile-program url)
                     'face 'warning))
      (funcall done nil))
     ((not (org-moor--singlefile-server-p))
      (org-moor--singlefile-run url file nil checked))
     (t
      (org-moor--singlefile-server-enqueue (list url file checked nil org-moor--agent))
      (org-moor--singlefile-server-next)))))

(defun org-moor-singlefile-keep (url dir done)
  "Keep a replica of the page at URL in DIR by single-file; call DONE.
With the replica's name, `signin' when a sign-in page was shown in its
place, or nil.  For `org-moor-fetch-replica', as KEEP."
  (let ((file (concat (make-temp-name (expand-file-name ".page" dir)) ".html")))
    (org-moor-singlefile
     url file
     (lambda (saved)
       (if (eq saved t)
           (progn (rename-file file (expand-file-name org-moor-snapshot-file dir) t)
                  (funcall done org-moor-snapshot-file))
         (funcall done saved))))))

(declare-function org-moor-keep-replica "org-moor-place" (records))

(defun org-moor--sign-in-url ()
  "Return the URL `org-moor-sign-in' is to sign in at, asked for.
Offered are the origins of this buffer's sources waiting for a sign-in,
by their host, the link at point's first."
  (let* ((at (when-let* ((id (and (derived-mode-p 'org-mode) (org-moor-link-at-point))))
               (plist-get (org-moor-find-record id) :origin)))
         (waiting (seq-filter (lambda (r) (plist-get r :signin))
                              (org-moor-sources-in (org-moor-sources))))
         (urls (delete-dups
                (delq nil (cons (and at (string-match-p "\\`https?://" at) at)
                                (mapcar (lambda (r) (plist-get r :origin)) waiting))))))
    (read-string "Sign in at URL: " (car urls) nil (cdr urls))))

(defun org-moor--sign-in-command (url)
  "Return the command opening a browser at URL to sign in, with a window.
With `org-moor-singlefile-browser', that browser as it is; else
single-file's, which makes the profile."
  (let ((profile (expand-file-name org-moor-singlefile-profile)))
    (if org-moor-singlefile-browser
        `(,(expand-file-name org-moor-singlefile-browser)
          ,(concat "--user-data-dir=" (directory-file-name profile))
          "--no-first-run" "--no-default-browser-check"
          ,@org-moor--browser-quiet-args
          ,(url-encode-url url))
      (list org-moor-singlefile-program "--create-browser-profile" profile
            (url-encode-url url)))))

(defun org-moor--signed-in ()
  "Have the pages queued while the profile was open to sign in saved."
  (setq org-moor--signing-in nil)
  (org-moor--singlefile-server-next))

;;;###autoload (autoload 'org-moor-sign-in "org-moor" nil t)
(defun org-moor-sign-in (url)
  "Sign in at URL in the browser single-file saves pages with.
The browser opens, with a window, in `org-moor-singlefile-profile';
sign in there and quit it, and the profile keeps the sign-in, its
caches let go -- see `org-moor-singlefile-profile-caches'.  Then the
sources of this buffer's sources directory waiting for a sign-in to
URL's host are offered to `org-moor-keep-replica'.  Interactively, the
origin of the link at point, else of a source waiting for a sign-in.

With `org-moor-singlefile-browser', the browser is started here, as it
is, so that the window you sign in in keeps its sandbox, which
single-file turns off in a browser it starts; else single-file starts it.
The browser kept for single-file, see `org-moor-singlefile-browser-idle',
is quit first, and started again for the next page."
  (interactive (list (org-moor--sign-in-url)))
  (unless org-moor-singlefile-profile
    (user-error "org-moor: no org-moor-singlefile-profile to sign in"))
  (unless (or org-moor-singlefile-browser
              (executable-find org-moor-singlefile-program))
    (user-error "org-moor: no %s" org-moor-singlefile-program))
  ;; A profile is open in one browser at a time: pages queued meanwhile
  ;; wait for this one to be quit.
  (setq org-moor--signing-in t)
  (org-moor--singlefile-server-stop)
  ;; Else the window would be handed to that browser, which shows none.
  (let ((profile (directory-file-name (expand-file-name org-moor-singlefile-profile))))
    (when (org-moor--profile-in-use-p profile)
      (org-moor--signed-in)
      (user-error "%s" (org-moor--profile-in-use-message profile))))
  (let ((host (url-host (url-generic-parse-url url)))
        (sources (org-moor-sources)))
    (condition-case err
        (make-process
         :name "org-moor-sign-in"
         :buffer " *org-moor-sign-in*"
         :coding (cons 'utf-8 (org-moor-argument-coding))
         :noquery t
         :command (org-moor--sign-in-command url)
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (org-moor--singlefile-profile-trim)
             (org-moor--signed-in)
             (let ((waiting (seq-filter (lambda (r) (equal (plist-get r :signin) host))
                                        (org-moor-sources-in sources))))
               (cond
                ((not (zerop (process-exit-status proc)))
                 (message "%s" (propertize (format "org-moor: the browser for %s did not close well"
                                                   host)
                                           'face 'warning)))
                ;; Whether you signed in there is not known here: only that
                ;; the browser was quit.
                ((and waiting
                      (y-or-n-p (format "Browser for %s closed; keep the %d replica%s waiting for it? "
                                        host (length waiting) (if (cdr waiting) "s" ""))))
                 (org-moor-keep-replica waiting))
                (t (message "Browser for %s closed; the profile keeps what was signed in there"
                            host)))))))
      (error (org-moor--signed-in)
             (signal (car err) (cdr err))))
    (message "Sign in to %s in the window, then quit the browser" host)))

(defun org-moor-front-download (url done)
  "Call DONE with how a browser downloads URL, as script/org-moor-front.ps1 says.
With nil, when the script says none, or cannot be asked; with `singlefile'
for a URL whose entry has single-file save it.  What it writes as errors
is not read.  See `org-moor-browser-download-function'."
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
                                          (`(,_page "singlefile") 'singlefile)
                                          (`(,download ,dir) (list download dir))))))))))
      (kill-buffer out)
      (kill-buffer err)
      (funcall done nil))))

(defun org-moor-front-signed-out (url location done)
  "Call DONE with whether URL, shown at LOCATION, was a sign-in page.
Unless LOCATION is URL itself, the script for the document in front is
asked about it, as `org-moor-front-download' asks: a page at LOCATION
that its entry has single-file save is a page of the site, and anything
else is the sign-in page.  For `org-moor-signed-out-function'."
  (if (or (not location) (equal location url))
      (funcall done nil)
    (org-moor-front-download location
                             (lambda (how) (funcall done (not (eq how 'singlefile)))))))

(defcustom org-moor-browser-download-function
  (and (eq system-type 'windows-nt) #'org-moor-front-download)
  "Function telling how a browser keeps a URL's replica, or nil.
Called with a URL and a function it calls, once it knows, with
\(DOWNLOAD DIR), the URL a browser signed in downloads the replica from
and the directory it downloads into; with `singlefile' when the page is
saved by `org-moor-singlefile-keep', in a browser of its own that shows
no window; or with nil when the URL is fetched as `org-moor-fetch'
fetches it.  DOWNLOAD is opened by `browse-url'.  On Windows, the script
for the document in front is asked, by the configuration it keeps sites in."
  :type '(choice function (const nil))
  :group 'org-moor)

(defvar org-moor--browser-queue nil
  "Downloads waiting for the browser, each (DOWNLOAD DIR URL INTO DONE).")

(defvar org-moor--browser-busy nil
  "Non-nil while a download by the browser is awaited.")

(defun org-moor--browser-next ()
  "Have the browser download the next replica queued, when none is awaited.
One at a time, since a replica is told from another only by when it came."
  (unless org-moor--browser-busy
    (when-let* ((next (pop org-moor--browser-queue)))
      (pcase-let ((`(,download ,dir ,url ,into ,done) next))
        (setq org-moor--browser-busy t)
        (let ((finish (lambda (name)
                        (setq org-moor--browser-busy nil)
                        (unwind-protect (funcall done name)
                          (org-moor--browser-next)))))
          (condition-case err
              (let ((since (current-time)))
                (browse-url download)
                (funcall (org-moor-arriving dir since) url into finish))
            (error (message "org-moor: %s" (error-message-string err))
                   (funcall finish nil))))))))

(defun org-moor-browser-or-fetch (url dir done &optional unseen)
  "Keep a replica of URL in DIR, then call DONE with its name, or nil.
By a browser when `org-moor-browser-download-function' tells one how:
by single-file, or by downloading it, the replica taken as
`org-moor-arriving' takes one, after any it is already downloading; else
by `org-moor-fetch'.  DONE is given `signin' when single-file was shown a
sign-in page.  With UNSEEN, no browser is opened in front of anyone: a
replica only a download would keep is left, and DONE given `later'."
  (if (not org-moor-browser-download-function)
      (org-moor-fetch url dir done)
    (let ((agent org-moor--agent))
      (funcall org-moor-browser-download-function url
               (lambda (how)
                 (let ((org-moor--agent agent))
                   (cond
                    ((not how) (org-moor-fetch url dir done))
                    ((eq how 'singlefile) (org-moor-singlefile-keep url dir done))
                    (unseen (funcall done 'later))
                    (t (setq org-moor--browser-queue
                             (append org-moor--browser-queue
                                     (list (list (nth 0 how) (nth 1 how) url dir done))))
                       (org-moor--browser-next)))))))))

(defun org-moor--clipboard-url ()
  "Return the clipboard's text when it is a URL, or nil."
  (let ((text (ignore-errors (string-trim (org-moor--clipboard)))))
    (and text (string-match-p "\\`https?://[^ \t\n]+\\'" text) text)))

(defvar org-moor--keeping (make-hash-table :test #'equal)
  "The sources whose replicas are being kept, in the background: records by id.")

(defun org-moor-fetch-replica (record &optional keep then)
  "Keep a replica of the source RECORD from its origin, a URL, in the background.
By KEEP: called with the URL, the source's directory and a function to
call with the replica's name there, `signin' when a sign-in page was
shown in its place, `later' when it is left for a browser to keep, or
nil; when nil, `org-moor-browser-or-fetch'.  A source shown a sign-in
page has the page's host as its :signin, for `org-moor-sign-in'.
THEN, when given, is called once it is kept, or is not."
  (let ((title (plist-get record :title))
        (url (plist-get record :origin))
        (id (plist-get record :id))
        (agent org-moor--agent))
    (puthash id record org-moor--keeping)
    (funcall (or keep #'org-moor-browser-or-fetch) url (plist-get record :dir)
             (lambda (file)
               (unwind-protect
                   (let ((org-moor--agent agent))
                     (pcase file
                       ('signin
                        (org-moor-record-set record :signin (url-host (url-generic-parse-url url)))
                        (org-moor--say "%s" (propertize
                                             (format "org-moor: %s was a sign-in page; kept only its URL -- sign in with M-x org-moor-sign-in"
                                                     title)
                                             'face 'warning)))
                       ('later
                        (org-moor--say "%s" (propertize
                                             (format "org-moor: %s is for the browser to keep; kept only its URL -- keep it with M-x org-moor-keep-replica"
                                                     title)
                                             'face 'warning)))
                       ((pred stringp)
                        (org-moor-record-set record :file file)
                        (when (plist-get record :signin)
                          (org-moor-record-set record :signin nil))
                        (org-moor--say "Kept %s" title))
                       (_
                        (org-moor--say "%s" (propertize
                                             (format "org-moor: no replica of %s; kept only its URL -- download it and give it with M-x org-moor-set-replica"
                                                     title)
                                             'face 'warning)))))
                 (remhash id org-moor--keeping)
                 (when then (funcall then)))))))

(defun org-moor-take-url-to (url sources &optional title wait keep)
  "Take URL into SOURCES as a source titled TITLE, and return its record.
Its replica is kept by KEEP in the background, or with WAIT before this
returns; see `org-moor-fetch-replica'.  TITLE is the file's name, for a URL
spelled as one, else the URL, when nil.  Nothing is written in Org."
  (let* ((url (org-moor-url-unwrap url))
         (name (org-moor-url-file-name url))
         (title (or title (and name (file-name-sans-extension name)) url))
         (record (org-moor-new-source title url 'web sources))
         (done nil))
    (org-moor-fetch-replica record keep (lambda () (setq done t)))
    (when wait
      (while (not done) (accept-process-output nil 0.1)))
    (or (org-moor-record (plist-get record :id) sources) record)))

;;;###autoload (autoload 'org-moor-take-url "org-moor" nil t)
(defun org-moor-take-url (url &optional title pick keep)
  "Take URL as a source, titled TITLE, and keep a replica of it.
The replica is what URL sends -- see `org-moor-fetch' -- or what KEEP
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

(defun org-moor--take-what (what sources title &optional wait keep)
  "Take WHAT, a file or a URL, into SOURCES as a source titled TITLE.
Return its record.  A file is copied, never moved, and titled by its
name unless TITLE is given; a URL's replica is kept by KEEP, as
`org-moor-fetch-replica' says, with WAIT before this returns, else
after.  The directory SOURCES is in must be there
already: a name mangled on the way in is not made into a tree of its own."
  (let* ((sources (file-name-as-directory (expand-file-name sources)))
         (parent (file-name-directory (directory-file-name sources))))
    (unless (file-directory-p parent)
      (error "org-moor: not a directory: %s" parent))
    (if (string-match-p "\\`https?://" what)
        (org-moor-take-url-to what sources title wait keep)
      (unless (file-regular-p what)
        (error "org-moor: not a file: %s" what))
      (org-moor-take-file-to what sources title))))

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

(defcustom org-moor-agent-timeout 100
  "Seconds `org-moor-batch-take' waits for a replica being kept.
Less than the two minutes after which an agent's shell -- Claude Code's,
say -- stops a command: stopped, the agent would take the source again.
A source still being kept is waited for once more by its id."
  :type 'integer
  :group 'org-moor)

(defun org-moor-batch-take ()
  "Take a file or a web page into a sources directory, and print a link to it.
For an agent, which cannot ask Emacs:

  emacs --batch -L DIR -L ORG-DOCK-DIR -l org-moor \\
        -f org-moor-batch-take [--server=FILE] SOURCES FILE-OR-URL [TITLE]

SOURCES is the sources directory; a file is copied, never moved, and
titled by its name unless TITLE is given.  What is printed is a link to
the source named by its title, [[moor:ID][TITLE]], and on the next line
the replica to read, or why there is none, as `org-moor-agent-state'
tells it.  In place of FILE-OR-URL, moor:ID takes nothing: it waits once
more for the replica of source ID, still being kept when it was taken.

With --server, the Emacs whose server FILE is takes it, by
`org-moor-agent-take', so that a URL is fetched as the user would fetch
it, signed in, without a browser coming in front of the user; this waits
for the replica, asking that Emacs, which goes on meanwhile, every second
for `org-moor-agent-timeout' seconds at most.  Without it, or when that
Emacs cannot be reached, a URL is fetched here, as nobody."
  (let* ((server (and command-line-args-left
                      (string-prefix-p "--server=" (car command-line-args-left))
                      (substring (pop command-line-args-left) (length "--server="))))
         (sources (expand-file-name (or (pop command-line-args-left)
                                        (error "org-moor: no sources directory"))))
         (what (or (pop command-line-args-left)
                   (error "org-moor: no file or URL to take")))
         (again (and (string-match "\\`moor:\\([^]:\n]+\\)\\'" what)
                     (match-string 1 what)))
         (what (if (or again (string-match-p "\\`https?://" what)) what (expand-file-name what)))
         (title (pop command-line-args-left))
         (link (cond
                (again (org-moor-link (or (org-moor-record again sources)
                                          (error "org-moor: no source %s in %s" again sources))))
                (server
                 (condition-case err
                     (or (org-moor--ask-server
                          server `(org-moor-agent-take ,sources ,what ,title))
                         (error "org-moor: Emacs did not take %s" what))
                   (file-error
                    (message "org-moor: %s; taken here" (error-message-string err))
                    nil)))))
         (id (and link (string-match org-moor--link-re link) (match-string 1 link)))
         (state
          (if link
              (org-moor--batch-wait server sources id)
            (let* ((org-moor-browser-download-function nil)
                   (record (org-moor--take-what what sources title t)))
              (setq link (org-moor-link record)
                    id (plist-get record :id))
              (org-moor-agent-state sources id)))))
    ;; In UTF-8, as an agent reads it, rather than the system's code page.
    (let ((coding-system-for-write 'utf-8))
      (princ (concat link "\n" (org-moor-agent-state-line state id) "\n")))))

(defun org-moor--batch-wait (server sources id)
  "Return what source ID in SOURCES has, once its replica is no longer pending.
Asked every second of the Emacs whose server is SERVER, for
`org-moor-agent-timeout' seconds at most; read here when SERVER is nil
or cannot be reached, where nothing is kept meanwhile."
  (let ((deadline (+ (float-time) org-moor-agent-timeout))
        (state nil))
    (while (progn (setq state
                        (or (and server
                                 (condition-case nil
                                     (org-moor--ask-server
                                      server `(org-moor-agent-state ,sources ,id))
                                   (file-error (setq server nil))))
                            (org-moor-agent-state sources id)))
                  (and (eq (car state) 'pending)
                       (< (float-time) deadline)))
      (sleep-for 1))
    state))

(defun org-moor--unseen-keep (url dir done)
  "Keep URL's replica in DIR as `org-moor-browser-or-fetch' does, unseen.
No browser is opened in front of anyone; see its UNSEEN."
  (org-moor-browser-or-fetch url dir done t))

(defun org-moor--keeping-origin (sources url)
  "Return the source in SOURCES whose replica of URL is being kept, or nil."
  (let ((sources (file-name-as-directory (expand-file-name sources)))
        (found nil))
    (maphash (lambda (_id record)
               (when (and (not found)
                          (equal (plist-get record :origin) url)
                          (file-in-directory-p (plist-get record :dir) sources))
                 (setq found record)))
             org-moor--keeping)
    found))

(defun org-moor-agent-take (sources what &optional title)
  "Take WHAT, a file or a URL, into SOURCES, and return a link to it.
For `org-moor-batch-take' --server, in the Emacs the user works in: a
URL's replica is kept as a URL taken here is, but by nothing that comes in
front of the user -- one only a download in the browser would keep has its
origin alone, for `org-moor-keep-replica' -- and after the link is
returned, so that this Emacs goes on meanwhile.  A URL whose replica is
being kept in SOURCES already, for another agent or this one asking
again, is that source.  See `org-moor--agent' for what the user is
spared of it.  The link is [[moor:ID][TITLE]]."
  (let ((org-moor--agent t))
    (org-moor-link
     (or (and (string-match-p "\\`https?://" what)
              (org-moor--keeping-origin sources (org-moor-url-unwrap what)))
         (org-moor--take-what what sources title nil #'org-moor--unseen-keep)))))

(defun org-moor-agent-state (sources id)
  "Return what source ID in SOURCES has, for an agent waiting on it.
\(kept REPLICA), (pending) while its replica is being kept, (signin HOST)
when HOST showed a sign-in page in its place, or (origin ORIGIN) when it
has only where it came from."
  (let ((record (org-moor-record id sources)))
    (cond ((gethash id org-moor--keeping) '(pending))
          ((not record) (list 'origin nil))
          ((plist-get record :replica) (list 'kept (plist-get record :replica)))
          ((plist-get record :signin) (list 'signin (plist-get record :signin)))
          (t (list 'origin (plist-get record :origin))))))

(defun org-moor-agent-state-line (state &optional id)
  "Return the line telling an agent what STATE, of `org-moor-agent-state', is.
ID is the source's, for waiting on it again."
  (pcase state
    (`(kept ,replica) (concat "Replica: " replica))
    (`(pending)
     (format "No replica yet: it is still being kept after %d seconds; run the command again with moor:%s in place of the file or URL to wait for it once more, and do not guess what it says."
             org-moor-agent-timeout (or id "ID")))
    (`(signin ,host)
     (format "No replica: %s showed a sign-in page, which the user signs in to later; do not guess what it says."
             host))
    (_ "No replica: only where it came from is kept, for the user to keep later; do not guess what it says.")))

;;;; org-protocol

;;;###autoload (autoload 'org-moor-protocol "org-moor" nil nil)
(defun org-moor-protocol (info)
  "org-protocol handler.  INFO is a plist from the protocol URL.

  org-protocol://moor?url=URL&title=TITLE
  org-protocol://moor?url=URL&title=TITLE&keep=singlefile
  org-protocol://moor?url=URL&title=TITLE&arriving=DIR&since=SECONDS
  org-protocol://moor?path=PATH&title=TITLE&origin=ORIGIN&via=VIA

The first takes a page or a file on the web.  The second takes a page
that single-file saves, by `org-moor-singlefile-keep'.  The third takes a
file on the web that a browser, signed in where it is, is downloading into
DIR: its replica is the file come there since SECONDS, a Unix time, moved
to the source once whole -- see `org-moor-arriving'.  The fourth takes a file on
disk, as does a file: URL, which a browser shows for a file it opened;
ORIGIN, when given, is where that file was opened from.  Either arrives
from outside Emacs, and goes where the buffer Emacs shows in front is
left: at its point when that is Org, as a take typed there would, else a
heading is picked.  Emacs is brought to the front to ask."
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
             url title nil
             ;; What arrives here has been looked up already, by the
             ;; script for the document in front, or it is from a page.
             (cond
              ((equal (plist-get info :keep) "singlefile") #'org-moor-singlefile-keep)
              ((plist-get info :arriving)
               (org-moor-arriving
                (plist-get info :arriving)
                (if-let* ((since (plist-get info :since)))
                    (seconds-to-time (string-to-number since))
                  (current-time))))
              (t #'org-moor-fetch))))
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
