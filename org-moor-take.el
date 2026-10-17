;;; org-moor-take.el --- Bring things in  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Taking is the step that happens when you meet a thing, which is usually
;; outside Emacs, so every command here asks as little as it can: a title,
;; and where it goes.  Where it goes is anywhere: the heading at point when
;; you are in Org, else one you pick from your refile targets, else
;; `org-moor-inbox-file'.

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
      (org-moor-insert-source-heading
       (org-moor--target-at (assoc (match-string 1 answer) table))
       (match-string 2 answer)))
     (t (org-moor-insert-source-heading 'inbox answer)))))

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

(defun org-moor--at-heading-p ()
  "Non-nil when point is at the start of a heading."
  (and (bolp) (org-at-heading-p)))

(defun org-moor--end-of-entry ()
  "Move to the end of this heading's own text, before its first child."
  (outline-next-heading)
  (skip-chars-backward " \t\n")
  (unless (eobp) (forward-line 1))
  (unless (bolp) (insert "\n")))

(defun org-moor-insert-source-heading (target title)
  "Insert a heading titled TITLE for a source at TARGET; return its marker.
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

;;;###autoload
(defun org-moor-quote (title &optional pick)
  "Quote the clipboard under TITLE, at point or, with PICK, a chosen heading.
TITLE says who it is from and about what: \"Chat from Robin\"."
  (interactive (list (read-string "Title: ") current-prefix-arg))
  (let* ((text (org-moor--clipboard))
         (target (org-moor--target pick))
         (target (if (markerp target)
                     target
                   (org-moor-insert-source-heading target title))))
    (with-current-buffer (marker-buffer target)
      (org-with-wide-buffer
       (goto-char target)
       (if (org-moor--at-heading-p)
           (org-moor--end-of-entry)
         (unless (bolp) (end-of-line) (insert "\n")))
       (insert (org-moor-quote-string title text))))
    (set-marker target nil)
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

(defcustom org-moor-noter-extensions '("pdf" "epub" "djvu")
  "Extensions of copies org-noter can open; NOTER_DOCUMENT is written for these."
  :type '(repeat string)
  :group 'org-moor)

(defun org-moor--write-noter (marker copy)
  "Point org-noter at COPY from the source at MARKER, when it can read it."
  (when (member (downcase (or (file-name-extension copy) ""))
                org-moor-noter-extensions)
    (org-entry-put marker "NOTER_DOCUMENT" (abbreviate-file-name copy))))

(defun org-moor--undo-source (marker id)
  "Remove the source heading at MARKER and the store directory of ID.
For a taking that failed halfway, so that it leaves nothing behind."
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))))
  (let ((dir (org-moor-store-path id)))
    (when (and (file-directory-p dir) (directory-empty-p dir))
      (delete-directory dir))))

(defvar org-moor--default-title nil
  "Title offered for the next file taken, when the caller knows a better
one than its name -- a document's own title, sent from outside Emacs.")

;;;###autoload
(defun org-moor-take-file (file &optional pick via flip origin)
  "Take FILE as a source: a heading for it and its copy in the store.
At point when in Org; with PICK, or from outside Org, under a chosen
heading.  VIA says how it arrived, `file' when nil.  The original is moved
when it is in `org-moor-move-directories' and copied otherwise; FLIP turns
that round for this once.  ORIGIN is where it came from, when that is
not FILE itself -- a copy saved from a document opened from the web.
Return the marker of the new heading.

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
         (target (org-moor--target pick))
         (marker (org-moor-insert-source-heading target title))
         (id (org-with-point-at marker
               (org-moor-set-source (or origin file) (or via 'file) name)))
         (copy (org-moor-copy-path id name)))
    (org-moor--release target)
    (condition-case err
        (progn
          (make-directory (file-name-directory copy) t)
          (if move
              (rename-file file copy)
            (copy-file file copy nil t)))
      (error
       (org-moor--undo-source marker id)
       (signal (car err) (cdr err))))
    (when (and move (fboundp 'dired-remove-file))
      (dired-remove-file file))
    (org-moor--write-noter marker copy)
    (message "Took %s (%s)" name (if move "moved" "copied"))
    marker))

;;;; Web pages

(defcustom org-moor-snapshot-function #'org-moor-snapshot-monolith
  "Function keeping a web page: called with URL, the file to write and a
callback taking non-nil on success.  Nil keeps only the URL."
  :type '(choice function (const nil))
  :group 'org-moor)

(defcustom org-moor-monolith-program "monolith"
  "The monolith executable, which saves a page with everything it needs."
  :type 'string
  :group 'org-moor)

(defun org-moor-snapshot-monolith (url file done)
  "Save URL to FILE with monolith, then call DONE with success."
  (if (not (executable-find org-moor-monolith-program))
      (progn (message "org-moor: %s not found; kept only the URL"
                      org-moor-monolith-program)
             (funcall done nil))
    (make-process
     :name "org-moor-snapshot"
     :buffer " *org-moor-snapshot*"
     :command (list org-moor-monolith-program "-s" "-o" file url)
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (funcall done (and (zerop (process-exit-status proc))
                                      (file-exists-p file))))))))

(defconst org-moor-snapshot-file "page.html"
  "Name of a web page's copy in its store directory.")

(defcustom org-moor-download-extensions
  '("pdf" "pptx" "ppt" "docx" "doc" "xlsx" "xls" "csv" "epub" "zip" "txt" "md")
  "Extensions of URLs that are files rather than pages.
Such a URL is downloaded as it is, not saved as a page: a PDF opened in a
browser is still a PDF."
  :type '(repeat string)
  :group 'org-moor)

(defcustom org-moor-curl-program "curl"
  "The curl executable, used to download a file from a URL."
  :type 'string
  :group 'org-moor)

(defun org-moor-url-file-name (url)
  "Return the file name URL downloads as, or nil if URL is a page.
A file is a URL whose path ends in one of `org-moor-download-extensions'."
  (let* ((path (car (url-path-and-query (url-generic-parse-url url))))
         (name (and path (file-name-nondirectory
                          (decode-coding-string (url-unhex-string path) 'utf-8)))))
    (and name
         (member (downcase (or (file-name-extension name) ""))
                 org-moor-download-extensions)
         name)))

(defun org-moor-download (url file done)
  "Download URL to FILE, then call DONE with success.
By curl when there is one, following redirects; else by `url-copy-file'."
  (if (executable-find org-moor-curl-program)
      (make-process
       :name "org-moor-download"
       :buffer " *org-moor-download*"
       :command (list org-moor-curl-program "-fsSL" "-o" file url)
       :sentinel (lambda (proc _event)
                   (unless (process-live-p proc)
                     (funcall done (and (zerop (process-exit-status proc))
                                        (file-exists-p file))))))
    (funcall done (ignore-errors (url-copy-file url file t) t))))

(defun org-moor--clipboard-url ()
  "Return the clipboard's text when it is a URL, or nil."
  (let ((text (ignore-errors (string-trim (org-moor--clipboard)))))
    (and text (string-match-p "\\`https?://[^ \t\n]+\\'" text) text)))

;;;###autoload
(defun org-moor-take-url (url &optional title pick)
  "Take URL as a source, titled TITLE, and keep a copy of it.
A URL that is a file -- see `org-moor-url-file-name' -- is downloaded as
that file; a page is kept by `org-moor-snapshot-function'.  At point when
in Org; with PICK, under a chosen heading.  Interactively the URL on the
clipboard is offered."
  (interactive
   (list (read-string "URL: " (org-moor--clipboard-url)) nil current-prefix-arg))
  (let* ((name (org-moor-url-file-name url))
         (title (read-string "Title: " (or title
                                           (and name (file-name-sans-extension name)))))
         (target (org-moor--target pick))
         (marker (org-moor-insert-source-heading target title))
         (id (org-with-point-at marker (org-moor-set-source url 'web)))
         (file (or name org-moor-snapshot-file))
         (keep (if name #'org-moor-download org-moor-snapshot-function)))
    (org-moor--release target)
    (when keep
      (let ((copy (org-moor-copy-path id file)))
        (make-directory (file-name-directory copy) t)
        (funcall keep url copy
                 (lambda (ok)
                   (if (not ok)
                       (message "org-moor: kept only the URL of %s" title)
                     (when (buffer-live-p (marker-buffer marker))
                       (org-entry-put marker org-moor-property-file file)
                       (org-moor--write-noter marker copy))
                     (message "Kept %s" title))))))
    marker))

;;;; org-protocol

;;;###autoload
(defun org-moor-protocol (info)
  "org-protocol handler.  INFO is a plist from the protocol URL.

  org-protocol://moor?url=URL&title=TITLE
  org-protocol://moor?path=PATH&title=TITLE&origin=ORIGIN&via=VIA

The first takes a page or a file on the web, the second a file on disk;
ORIGIN, when given, is where that file was opened from.  Either arrives
from outside Emacs, with no point to go to, so a heading is always picked,
and Emacs is brought to the front to ask."
  (select-frame-set-input-focus (selected-frame))
  (let ((url (plist-get info :url))
        (path (plist-get info :path))
        (title (plist-get info :title)))
    (cond
     (path
      (let ((org-moor--default-title title))
        (org-moor-take-file path t (plist-get info :via) nil
                            (plist-get info :origin))))
     (url (org-moor-take-url url title t))
     (t (message "org-moor: protocol URL had no url= or path="))))
  nil)

;;;###autoload
(defun org-moor-protocol-setup ()
  "Register the `moor' org-protocol."
  (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist
               '("org-moor" :protocol "moor" :function org-moor-protocol)))

(provide 'org-moor-take)
;;; org-moor-take.el ends here
