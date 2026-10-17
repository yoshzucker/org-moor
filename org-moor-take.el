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

(defvar org-protocol-protocol-alist)

(defcustom org-moor-inbox-file nil
  "File a thing goes to when there is no heading to take it to.
Nil means `org-default-notes-file'."
  :type '(choice (const :tag "org-default-notes-file" nil) file)
  :group 'org-moor)

(defun org-moor--inbox-buffer ()
  "Return the buffer of the inbox file."
  (find-file-noselect (or org-moor-inbox-file org-default-notes-file)))

(defun org-moor--target (pick)
  "Return where a taken thing goes: a marker, or the symbol `inbox'.
At point when in Org and PICK is nil.  Otherwise at the start of a heading
picked from the refile targets, or, when none is picked, `inbox': a heading
of its own at the top level of the inbox file.  The end of a file is not a
place of its own, since it is inside the last heading there."
  (if (and (not pick) (derived-mode-p 'org-mode))
      (point-marker)
    (let ((loc (ignore-errors
                 (org-refile-get-location "Take to" nil t))))
      (if (and loc (nth 3 loc))
          (with-current-buffer (find-file-noselect (nth 1 loc))
            (copy-marker (nth 3 loc)))
        'inbox))))

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
Under the heading TARGET is in, as its last child; at the top level, at
the end of the file, when TARGET is before the first heading or `inbox'."
  (with-current-buffer (if (markerp target)
                           (marker-buffer target)
                         (org-moor--inbox-buffer))
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

;;;###autoload
(defun org-moor-take-file (file &optional pick via flip)
  "Take FILE as a source: a heading for it and its copy in the store.
At point when in Org; with PICK, or from outside Org, under a chosen
heading.  VIA says how it arrived, `file' when nil.  The original is moved
when it is in `org-moor-move-directories' and copied otherwise; FLIP turns
that round for this once.  Return the marker of the new heading.

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
                             (file-name-sans-extension name)))
         (target (org-moor--target pick))
         (marker (org-moor-insert-source-heading target title))
         (id (org-with-point-at marker
               (org-moor-set-source file (or via 'file) name)))
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

;;;###autoload
(defun org-moor-take-url (url &optional title pick)
  "Take URL as a source, titled TITLE, and keep a copy of the page.
At point when in Org; with PICK, under a chosen heading."
  (interactive (list (read-string "URL: ") nil current-prefix-arg))
  (let* ((title (read-string "Title: " title))
         (target (org-moor--target pick))
         (marker (org-moor-insert-source-heading target title))
         (id (org-with-point-at marker (org-moor-set-source url 'web))))
    (org-moor--release target)
    (when org-moor-snapshot-function
      (let ((copy (org-moor-copy-path id org-moor-snapshot-file)))
        (make-directory (file-name-directory copy) t)
        (funcall org-moor-snapshot-function url copy
                 (lambda (ok)
                   (when (and ok (buffer-live-p (marker-buffer marker)))
                     (org-entry-put marker org-moor-property-file
                                    org-moor-snapshot-file)
                     (message "Kept %s" title))))))
    marker))

;;;; org-protocol

;;;###autoload
(defun org-moor-protocol (info)
  "org-protocol handler.  INFO is a plist from the protocol URL.

  org-protocol://moor?url=URL&title=TITLE

A page reached from a browser has no point to go to, so a heading is
always picked."
  (if-let* ((url (plist-get info :url)))
      (org-moor-take-url url (plist-get info :title) t)
    (message "org-moor: protocol URL had no url="))
  nil)

;;;###autoload
(defun org-moor-protocol-setup ()
  "Register the `moor' org-protocol."
  (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist
               '("org-moor" :protocol "moor" :function org-moor-protocol)))

(provide 'org-moor-take)
;;; org-moor-take.el ends here
