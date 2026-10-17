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

(defun org-moor--inbox ()
  "Return a marker at the end of the inbox file."
  (let ((file (or org-moor-inbox-file org-default-notes-file)))
    (with-current-buffer (find-file-noselect file)
      (copy-marker (point-max)))))

(defun org-moor--target (pick)
  "Return a marker where a taken thing goes.
At point when in Org and PICK is nil.  Otherwise at the start of a heading
picked from the refile targets, or, when none is picked, at the end of the
inbox."
  (if (and (not pick) (derived-mode-p 'org-mode))
      (point-marker)
    (let ((loc (ignore-errors
                 (org-refile-get-location "Take to" nil t))))
      (if (and loc (nth 3 loc))
          (with-current-buffer (find-file-noselect (nth 1 loc))
            (copy-marker (nth 3 loc)))
        (org-moor--inbox)))))

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
Under the heading TARGET is in, as its last child; at the end of the file
when TARGET is before the first heading."
  (with-current-buffer (marker-buffer target)
    (org-with-wide-buffer
     (goto-char target)
     (let ((level (if (org-before-first-heading-p)
                      0
                    (org-back-to-heading t)
                    (org-current-level))))
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
  (let ((text (or (ignore-errors (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
                  (ignore-errors (current-kill 0 t)))))
    (if (and text (not (string-blank-p text)))
        (substring-no-properties text)
      (user-error "org-moor: the clipboard is empty"))))

;;;###autoload
(defun org-moor-quote (title &optional pick)
  "Quote the clipboard under TITLE, at point or, with PICK, a chosen heading.
TITLE says who it is from and about what: \"Chat from Robin\"."
  (interactive (list (read-string "Title: ") current-prefix-arg))
  (let ((text (org-moor--clipboard))
        (target (org-moor--target pick)))
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

(defcustom org-moor-file-action 'copy
  "What taking a file does with the original: `copy' it or `move' it."
  :type '(choice (const copy) (const move))
  :group 'org-moor)

(defcustom org-moor-noter-extensions '("pdf" "epub" "djvu")
  "Extensions of copies org-noter can open; NOTER_DOCUMENT is written for these."
  :type '(repeat string)
  :group 'org-moor)

(defun org-moor--write-noter (marker copy)
  "Point org-noter at COPY from the source at MARKER, when it can read it."
  (when (member (downcase (or (file-name-extension copy) ""))
                org-moor-noter-extensions)
    (org-entry-put marker "NOTER_DOCUMENT" (abbreviate-file-name copy))))

;;;###autoload
(defun org-moor-take-file (file &optional pick via)
  "Take FILE as a source: a heading for it and its copy in the store.
At point when in Org; with PICK, under a chosen heading.  VIA says how it
arrived, `file' when nil.  The original is copied or moved as
`org-moor-file-action' says.  Return the marker of the new heading."
  (interactive (list (read-file-name "Take file: " nil nil t)
                     current-prefix-arg
                     (and current-prefix-arg
                          (completing-read "Via: " '("file" "mail" "chat" "web")))))
  (let* ((file (expand-file-name file))
         (name (file-name-nondirectory file))
         (title (read-string "Title: " (file-name-sans-extension name)))
         (target (org-moor--target pick))
         (marker (org-moor-insert-source-heading target title))
         (id (org-with-point-at marker
               (org-moor-set-source file (or via 'file) name)))
         (copy (org-moor-copy-path id name)))
    (set-marker target nil)
    (make-directory (file-name-directory copy) t)
    (if (eq org-moor-file-action 'move)
        (rename-file file copy)
      (copy-file file copy nil t))
    (org-moor--write-noter marker copy)
    (message "Took %s" name)
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
    (set-marker target nil)
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
