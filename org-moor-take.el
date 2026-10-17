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
At point when in Org and PICK is nil; otherwise one picked from the refile
targets, falling back to the inbox."
  (cond
   ((and (not pick) (derived-mode-p 'org-mode))
    (point-marker))
   (t
    (let ((loc (ignore-errors
                 (org-refile-get-location "Take to" nil t))))
      (if (and loc (nth 3 loc))
          (with-current-buffer (find-file-noselect (nth 1 loc))
            (org-with-wide-buffer
             (goto-char (nth 3 loc))
             (org-end-of-subtree t t)
             (point-marker)))
        (org-moor--inbox))))))

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
       (unless (bolp) (end-of-line) (insert "\n"))
       (insert (org-moor-quote-string title text))))
    (set-marker target nil)
    (message "Quoted: %s" title)))

(provide 'org-moor-take)
;;; org-moor-take.el ends here
