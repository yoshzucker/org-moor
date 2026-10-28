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
      (let ((here (org-moor--here-p target))
            (link (org-moor-link (funcall take (org-moor--target-sources target)))))
        (if here
            (org-moor-insert-link-here link)
          (org-moor-insert-link-line target title link)))
    (org-moor--release target)))

;;;; The clipboard

;; What is copied -- a chat's message, a mail's paragraph, a screenshot --
;; is taken as any file is: a source of its own, its text or its image the
;; copy, so that it is kept, and read, where every other source is.

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
      (call-process "powershell" nil nil nil "-NoProfile" "-Command"
                    (concat "Add-Type -AssemblyName System.Windows.Forms;"
                            "$i = [Windows.Forms.Clipboard]::GetImage();"
                            "if ($i) { $i.Save('"
                            (replace-regexp-in-string "'" "''" (convert-standard-filename file))
                            "', [Drawing.Imaging.ImageFormat]::Png) }")))))
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
WRITE is called with the copy's file, named after TITLE with EXTENSION.
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
  "Take TEXT into SOURCES as a source titled TITLE, a text file its copy.
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

;; A file behind a sign-in that only the browser has can be downloaded by
;; the browser, from a URL that says to: where the script for the document
;; in front has a browser keep it, see script/org-moor-front.ps1.  A URL
;; taken in Emacs is kept the same way, by the same configuration, which
;; the script is asked about.

(defconst org-moor-script-directory
  (expand-file-name "script" (file-name-directory (or load-file-name buffer-file-name)))
  "The package's scripts.")

(defun org-moor-front-download (url done)
  "Call DONE with how a browser downloads URL, as script/org-moor-front.ps1 says.
With nil, when the script says none, or cannot be asked.  What it writes
as errors is not read.  See `org-moor-browser-download-function'."
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
                    :coding 'utf-8
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
                                          (`(,download ,dir) (cons download dir))))))))))
      (kill-buffer out)
      (kill-buffer err)
      (funcall done nil))))

(defcustom org-moor-browser-download-function
  (and (eq system-type 'windows-nt) #'org-moor-front-download)
  "Function telling how a browser downloads a URL's copy, or nil.
Called with a URL and a function it calls, once it knows, with
\(DOWNLOAD . DIR), the URL a browser signed in downloads the copy from
and the directory it downloads into, or with nil when the URL is fetched
as `org-moor-fetch' fetches it.  DOWNLOAD is opened by `browse-url'.  On
Windows, the script for the document in front is asked, by the
configuration it keeps sites in."
  :type '(choice function (const nil))
  :group 'org-moor)

(defvar org-moor--browser-queue nil
  "Downloads waiting for the browser, each (DOWNLOAD DIR URL INTO DONE).")

(defvar org-moor--browser-busy nil
  "Non-nil while a download by the browser is awaited.")

(defun org-moor--browser-next ()
  "Have the browser download the next copy queued, when none is awaited.
One at a time, since a copy is told from another only by when it came."
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

(defun org-moor-browser-or-fetch (url dir done)
  "Keep a copy of URL in DIR, then call DONE with its name, or nil.
By a browser when `org-moor-browser-download-function' tells one how,
the copy taken as `org-moor-arriving' takes one, after any it is already
downloading; else by `org-moor-fetch'."
  (if (not org-moor-browser-download-function)
      (org-moor-fetch url dir done)
    (funcall org-moor-browser-download-function url
             (lambda (how)
               (if (not how)
                   (org-moor-fetch url dir done)
                 (setq org-moor--browser-queue
                       (append org-moor--browser-queue
                               (list (list (car how) (cdr how) url dir done))))
                 (org-moor--browser-next))))))

(defun org-moor--clipboard-url ()
  "Return the clipboard's text when it is a URL, or nil."
  (let ((text (ignore-errors (string-trim (org-moor--clipboard)))))
    (and text (string-match-p "\\`https?://[^ \t\n]+\\'" text) text)))

(defun org-moor-fetch-copy (record &optional keep then)
  "Keep a copy of the source RECORD from its origin, a URL, in the background.
By KEEP: called with the URL, the source's directory and a function to
call with the copy's name there, or nil; when nil,
`org-moor-browser-or-fetch'.
THEN, when given, is called once it is kept, or is not."
  (let ((title (plist-get record :title))
        (url (plist-get record :origin)))
    (funcall (or keep #'org-moor-browser-or-fetch) url (plist-get record :dir)
             (lambda (file)
               (if (not file)
                   (message "%s" (propertize
                                  (format "org-moor: no copy of %s; kept only its URL -- download it and give it with M-x org-moor-set-copy"
                                          title)
                                  'face 'warning))
                 (org-moor-record-set record :file file)
                 (message "Kept %s" title))
               (when then (funcall then))))))

(defun org-moor-take-url-to (url sources &optional title wait keep)
  "Take URL into SOURCES as a source titled TITLE, and return its record.
Its copy is kept by KEEP in the background, or with WAIT before this
returns; see `org-moor-fetch-copy'.  TITLE is the file's name, for a URL
spelled as one, else the URL, when nil.  Nothing is written in Org."
  (let* ((url (org-moor-url-unwrap url))
         (name (org-moor-url-file-name url))
         (title (or title (and name (file-name-sans-extension name)) url))
         (record (org-moor-new-source title url 'web sources))
         (done nil))
    (org-moor-fetch-copy record keep (lambda () (setq done t)))
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

(defun org-moor--take-what (what sources title &optional wait)
  "Take WHAT, a file or a URL, into SOURCES as a source titled TITLE.
Return its record.  A file is copied, never moved, and titled by its
name unless TITLE is given; a URL's copy is kept with WAIT before this
returns, else after."
  (let ((sources (file-name-as-directory (expand-file-name sources))))
    (if (string-match-p "\\`https?://" what)
        (org-moor-take-url-to what sources title wait)
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
socket, or its authentication file when it is reached by TCP."
  (require 'server)
  (let ((server-use-tcp (file-regular-p file))
        (server-auth-dir (file-name-directory file))
        (server-socket-dir (file-name-directory file)))
    (condition-case err
        (server-eval-at (file-name-nondirectory file) form)
      (server-return-invalid-read-syntax (signal (car err) (cdr err)))
      (error (signal 'file-error (list "Cannot reach Emacs" file
                                       (error-message-string err)))))))

(defun org-moor-batch-take ()
  "Take a file or a web page into a sources directory, and print a link to it.
For an agent, which cannot ask Emacs:

  emacs --batch -L DIR -L ORG-DOCK-DIR -l org-moor \\
        -f org-moor-batch-take [--server=FILE] SOURCES FILE-OR-URL [TITLE]

SOURCES is the sources directory; a file is copied, never moved, and
titled by its name unless TITLE is given.  What is printed is a link to
the source named by its title, [[moor:ID][TITLE]].

With --server, the Emacs whose server FILE is takes it, by
`org-moor-agent-take', so that a URL is fetched as the user would fetch
it, by the browser among the ways, the link printed at once and the copy
kept after.  Without it, or when that Emacs cannot be reached, a URL is
fetched here, as nobody, before the link is printed."
  (let* ((server (and command-line-args-left
                      (string-prefix-p "--server=" (car command-line-args-left))
                      (substring (pop command-line-args-left) (length "--server="))))
         (sources (expand-file-name (or (pop command-line-args-left)
                                        (error "org-moor: no sources directory"))))
         (what (or (pop command-line-args-left)
                   (error "org-moor: no file or URL to take")))
         (what (if (string-match-p "\\`https?://" what) what (expand-file-name what)))
         (title (pop command-line-args-left))
         (link (or (and server
                        (condition-case err
                            (or (org-moor--ask-server
                                 server `(org-moor-agent-take ,sources ,what ,title))
                                (error "org-moor: Emacs did not take %s" what))
                          (file-error
                           (message "org-moor: %s; taken here" (error-message-string err))
                           nil)))
                   (let ((org-moor-browser-download-function nil))
                     (org-moor-link (org-moor--take-what what sources title t))))))
    (princ (concat link "\n"))))

(defun org-moor-agent-take (sources what &optional title)
  "Take WHAT, a file or a URL, into SOURCES, and return a link to it.
For `org-moor-batch-take' --server, in the Emacs the user works in: a
URL's copy is kept as a URL taken here is, after the link is returned,
so that the agent is not kept waiting.  The link is [[moor:ID][TITLE]]."
  (org-moor-link (org-moor--take-what what sources title)))

;;;; org-protocol

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
             (if-let* ((dir (plist-get info :arriving)))
                 (org-moor-arriving
                  dir (if-let* ((since (plist-get info :since)))
                          (seconds-to-time (string-to-number since))
                        (current-time)))
               #'org-moor-fetch)))
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
