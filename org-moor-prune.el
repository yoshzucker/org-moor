;;; org-moor-prune.el --- Let copies go, choosing which  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The store grows: a deck or a manual is megabytes, and Org files that are
;; meant to travel should not have to carry all of them.  Letting a copy go
;; is safe when what matters is kept -- the heading, what you wrote, the
;; origin -- and those stay.  Only the bytes go, and the heading forgets it
;; had them.
;;
;; `org-moor-prune' lists every source with a copy, largest first, with when
;; it was taken and whether its origin is a URL that may still answer.
;; Mark with d, unmark with u, and x lets the marked copies go.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-return)
(require 'tabulated-list)
(require 'cl-lib)

(defun org-moor--directory-size (dir)
  "Return the size in bytes of the files under DIR."
  (cl-loop for f in (directory-files-recursively dir "" nil)
           sum (or (file-attribute-size (file-attributes f)) 0)))

(defun org-moor-copies ()
  "Return every source with a copy in the store, as plists.
Keys are those of `org-moor-source', with :marker, :heading and :size."
  (cl-loop
   for file in (funcall org-moor-files-function)
   when (file-readable-p file)
   append (with-current-buffer (find-file-noselect file)
            (org-with-wide-buffer
             (goto-char (point-min))
             (let (found)
               (while (re-search-forward
                       (concat "^[ \t]*:" org-moor-property-file ":") nil t)
                 (save-excursion
                   (unless (org-before-first-heading-p)
                     (org-back-to-heading t)
                     (let ((source (org-moor-source)))
                       (when (and source (plist-get source :id))
                         (let ((dir (org-moor-store-path (plist-get source :id))))
                           (when (file-directory-p dir)
                             (push (append
                                    (list :marker (point-marker)
                                          :heading (org-get-heading t t t t)
                                          :size (org-moor--directory-size dir))
                                    source)
                                   found))))))))
               (nreverse found))))))

(defun org-moor-let-go (marker)
  "Delete the copy of the source at MARKER, and say so on its heading.
The heading, its notes and its origin are kept."
  (org-with-point-at marker
    (let ((id (org-entry-get nil "ID")))
      (when id
        (let ((dir (org-moor-store-path id)))
          (when (file-directory-p dir)
            (delete-directory dir t))))
      (org-entry-delete nil org-moor-property-file)
      (org-entry-delete nil "NOTER_DOCUMENT"))))

;;;; The list

(defvar-local org-moor-prune--copies nil
  "The copies listed, by id.")

(defun org-moor-prune--entries ()
  "Return the list's entries, largest copy first."
  (setq org-moor-prune--copies (org-moor-copies))
  (mapcar
   (lambda (c)
     (let ((origin (or (plist-get c :origin) "")))
       (list (plist-get c :id)
             (vector (file-size-human-readable (plist-get c :size))
                     (let ((ts (plist-get c :taken)))
                       (if (and ts (string-match org-ts-regexp-both ts))
                           (substring (match-string 0 ts) 1 11)
                         ""))
                     (if (string-match-p "\\`https?:" origin) "url" "")
                     (plist-get c :heading)))))
   (sort org-moor-prune--copies
         (lambda (a b) (> (plist-get a :size) (plist-get b :size))))))

(defvar org-moor-prune-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'org-moor-prune-mark)
    (define-key map (kbd "u") #'org-moor-prune-unmark)
    (define-key map (kbd "x") #'org-moor-prune-execute)
    (define-key map (kbd "RET") #'org-moor-prune-visit)
    map)
  "Keys of the prune list.")

(define-derived-mode org-moor-prune-mode tabulated-list-mode "Moor-Prune"
  "Copies in the store, to choose which to let go."
  (setq tabulated-list-format [("Size" 9 nil :right-align t)
                               ("Taken" 11 t)
                               ("Origin" 7 t)
                               ("Heading" 0 t)]
        tabulated-list-padding 2
        tabulated-list-entries #'org-moor-prune--entries)
  (tabulated-list-init-header))

(defun org-moor-prune--copy-at-point ()
  "Return the copy on this line."
  (let ((id (tabulated-list-get-id)))
    (seq-find (lambda (c) (equal (plist-get c :id) id)) org-moor-prune--copies)))

(defun org-moor-prune-mark ()
  "Mark this copy to be let go."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun org-moor-prune-unmark ()
  "Unmark this copy."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun org-moor-prune-visit ()
  "Go to this copy's heading."
  (interactive)
  (let ((marker (plist-get (org-moor-prune--copy-at-point) :marker)))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (org-fold-show-context)))

(defun org-moor-prune-execute ()
  "Let the marked copies go."
  (interactive)
  (let (marked)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (push (org-moor-prune--copy-at-point) marked))
        (forward-line 1)))
    (if (null marked)
        (message "Nothing marked")
      (when (y-or-n-p (format "Let %d cop%s go (%s)? "
                              (length marked)
                              (if (cdr marked) "ies" "y")
                              (file-size-human-readable
                               (apply #'+ (mapcar (lambda (c) (plist-get c :size))
                                                  marked)))))
        (dolist (c marked)
          (org-moor-let-go (plist-get c :marker)))
        (tabulated-list-revert)))))

;;;###autoload (autoload 'org-moor-prune "org-moor" nil t)
(defun org-moor-prune ()
  "List the copies in the store, largest first, to choose which to let go."
  (interactive)
  (with-current-buffer (get-buffer-create "*org-moor prune*")
    (org-moor-prune-mode)
    (tabulated-list-revert)
    (pop-to-buffer-same-window (current-buffer))))

(provide 'org-moor-prune)
;;; org-moor-prune.el ends here
