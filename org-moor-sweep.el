;;; org-moor-sweep.el --- Clear where files pile up  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Files pile up where they arrive -- downloads, a desktop -- and clearing
;; them all at once, days later, is hours of remembering what each one
;; was.  A sweep is that pile as one Dired buffer, newest first, so that
;; each file is decided while it is still known: taken into the sources,
;; put away in a directory of your own, both, or thrown away.
;;
;; It is Dired, not a list of its own, so everything Dired does works on it:
;; opening, marking, deleting to the trash, and expanding a subdirectory
;; with whatever does that for you.  The commands here work in any Dired
;; buffer, on the file at point.
;;
;; Putting a file away and taking it are two acts, and both can be wanted.
;; A directory is where a file can be found without Org, and is renamed and
;; reorganised as work goes on; so a source taken from there keeps a copy
;; of its own in the sources, and Org never depends on where the file went.

;;; Code:

(require 'org-moor-take)
(require 'dired)

(defcustom org-moor-directory-roots nil
  "Directories under which a file is put away, with their subdirectories.
Nil reads any directory."
  :type '(repeat directory)
  :group 'org-moor)

(defcustom org-moor-directory-depth 2
  "How many levels under each root `org-moor-read-directory' offers."
  :type 'natnum
  :group 'org-moor)

(defvar org-moor-directory-history nil
  "Directories files were put away in, newest first.")

;;;; Directories

(defun org-moor--subdirectories (dir depth)
  "Return DIR's subdirectories down to DEPTH levels, as directory names."
  (when (> depth 0)
    (cl-loop for f in (ignore-errors (directory-files dir t "\\`[^.]" t))
             when (file-directory-p f)
             append (cons (file-name-as-directory f)
                          (org-moor--subdirectories f (1- depth))))))

(defun org-moor-read-directory (&optional prompt)
  "Read a directory to put a file in, existing or new, and return it.
The roots of `org-moor-directory-roots' and what is under them are offered;
a path that does not exist yet is made, once you say so.  A relative name
is taken under the first root."
  (if (null org-moor-directory-roots)
      (let ((dir (read-directory-name (or prompt "Put in: "))))
        (org-moor--ensure-directory dir))
    (let* ((roots (mapcar #'file-name-as-directory org-moor-directory-roots))
           (table (mapcar #'abbreviate-file-name
                          (append roots
                                  (cl-loop for r in roots
                                           append (org-moor--subdirectories
                                                   r org-moor-directory-depth)))))
           (answer (string-trim
                    (completing-read (or prompt "Put in (new path creates): ")
                                     table nil nil nil
                                     'org-moor-directory-history))))
      (when (string-empty-p answer)
        (user-error "org-moor: no directory"))
      (org-moor--ensure-directory
       (expand-file-name answer (car roots))))))

(defun org-moor--ensure-directory (dir)
  "Return DIR as a directory name, making it if you agree."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (unless (file-directory-p dir)
      (unless (y-or-n-p (format "Make %s? " (abbreviate-file-name dir)))
        (user-error "org-moor: not made"))
      (make-directory dir t))
    dir))

;;;; Putting away

(defun org-moor--files-here ()
  "Return the marked files in Dired, or the one at point, or nil."
  (and (derived-mode-p 'dired-mode)
       (dired-get-marked-files nil nil nil nil t)))

;;;###autoload (autoload 'org-moor-put-away "org-moor" nil t)
(defun org-moor-put-away (files dir &optional take)
  "Move FILES into DIR.  With TAKE, take each from there, as a copy.
Interactively FILES are the marked files in Dired or the one at point, and
DIR is read, see `org-moor-read-directory'.
\\[universal-argument] takes them too.
Return the files' new paths."
  (interactive
   (let ((files (or (org-moor--files-here)
                    (list (read-file-name "Put away: " nil nil t)))))
     (list files (org-moor-read-directory) current-prefix-arg)))
  (let (moved)
    (dolist (file files)
      (let ((to (expand-file-name (file-name-nondirectory file) dir)))
        (when (or (not (file-exists-p to))
                  (y-or-n-p (format "%s exists; replace? "
                                    (abbreviate-file-name to))))
          (rename-file file to t)
          (dired-remove-file file)
          (push to moved))))
    (setq moved (nreverse moved))
    (message "Put %d file%s in %s" (length moved)
             (if (= (length moved) 1) "" "s") (abbreviate-file-name dir))
    (when take
      (let ((target (org-moor-read-heading)))
        (dolist (file moved)
          (org-moor-take-file file target nil nil))))
    moved))

;;;###autoload (autoload 'org-moor-put-away-and-take "org-moor" nil t)
(defun org-moor-put-away-and-take (files dir)
  "Move FILES into DIR, and take each from there, as a copy."
  (interactive
   (let ((files (or (org-moor--files-here)
                    (list (read-file-name "Put away: " nil nil t)))))
     (list files (org-moor-read-directory))))
  (org-moor-put-away files dir t))

;;;; The sweep

(defun org-moor-sweep--files ()
  "Return what lies directly in `org-moor-move-directories'."
  (cl-loop for dir in org-moor-move-directories
           when (file-directory-p dir)
           append (directory-files (expand-file-name dir) t
                                   directory-files-no-dot-files-regexp)))

(defconst org-moor-sweep-buffer "*org-moor sweep*"
  "Name of the sweep's buffer.")

(defun org-moor-sweep--revert (&rest args)
  "List the pile afresh, then revert as Dired does, with ARGS."
  (setq dired-directory (cons (car dired-directory) (org-moor-sweep--files)))
  (apply #'dired-revert args))

;;;###autoload (autoload 'org-moor-sweep "org-moor" nil t)
(defun org-moor-sweep ()
  "Show what has piled up in `org-moor-move-directories', newest first."
  (interactive)
  (unless org-moor-move-directories
    (user-error "org-moor: `org-moor-move-directories' is empty"))
  (when-let* ((old (get-buffer org-moor-sweep-buffer)))
    (kill-buffer old))
  (let ((files (org-moor-sweep--files)))
    (unless files
      (user-error "org-moor: nothing has piled up"))
    (let ((buffer (dired-noselect (cons (expand-file-name "~/") files)
                                  (concat dired-listing-switches " -t"))))
      (with-current-buffer buffer
        (rename-buffer org-moor-sweep-buffer)
        (setq-local revert-buffer-function #'org-moor-sweep--revert))
      (pop-to-buffer-same-window buffer))))

(provide 'org-moor-sweep)
;;; org-moor-sweep.el ends here
