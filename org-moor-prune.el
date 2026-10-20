;;; org-moor-prune.el --- Let copies go, choosing which  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The sources grow: a deck or a manual is megabytes, and Org files that
;; are meant to travel should not have to carry all of them.  Letting a
;; copy go is safe when what matters is kept -- what you wrote, and the
;; source's record with its origin -- and those stay.  Only the bytes go,
;; and a link to the source opens its origin from then on.
;;
;; `org-moor-prune' lists the sources with a copy, largest first, read from
;; the sources directory itself: when each was taken, whether its origin
;; is a URL that may still answer, and whether anything still links to it.
;; Mark with d, unmark with u, and x lets the marked copies go.  t gives a
;; source another title, and RET opens it.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-return)
(require 'org-moor-place)
(require 'tabulated-list)
(require 'cl-lib)
(require 'seq)

(defun org-moor--kept (record)
  "Return the files kept for RECORD but its record.
Its copy, and what was made readable of it."
  (seq-remove (lambda (f) (equal (file-name-nondirectory f) org-moor-record-file))
              (directory-files (plist-get record :dir) t "\\`[^.]")))

(defun org-moor-linked-ids ()
  "Return the ids linked from the files of `org-moor-searched-files', as a
hash table."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (file (org-moor-searched-files))
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file)
          (dolist (id (org-with-wide-buffer (org-moor-ids-in (point-min) (point-max))))
            (puthash id t ids)))))
    ids))

(defun org-moor-copies (&optional sources)
  "Return the sources in SOURCES with a copy, as records.
Each also has :size, the bytes kept for it, and :linked, whether
anything links to it.  SOURCES is this buffer's sources directory when
nil."
  (let ((linked (org-moor-linked-ids)))
    (cl-loop for record in (org-moor-sources-in (or sources (org-moor-sources)))
             for kept = (org-moor--kept record)
             when kept
             collect (append record
                             (list :size (cl-loop for f in kept
                                                  sum (or (file-attribute-size
                                                           (file-attributes f))
                                                          0))
                                   :linked (gethash (plist-get record :id) linked))))))

(defun org-moor-let-go (record)
  "Delete the copy of RECORD, and what was made readable of it.
Its record stays, origin and all, and links to it open the origin."
  (dolist (file (org-moor--kept record))
    (if (file-directory-p file)
        (delete-directory file t)
      (delete-file file))))

;;;; The list

(defvar-local org-moor-prune--copies nil
  "The copies listed.")

(defvar-local org-moor-prune--sources nil
  "The sources directory listed.")

(defun org-moor-prune--entries ()
  "Return the list's entries, largest copy first."
  (setq org-moor-prune--copies (org-moor-copies org-moor-prune--sources))
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
                     (if (plist-get c :linked) "" "none")
                     (or (plist-get c :title) "")))))
   (sort org-moor-prune--copies
         (lambda (a b) (> (plist-get a :size) (plist-get b :size))))))

(defvar org-moor-prune-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'org-moor-prune-mark)
    (define-key map (kbd "u") #'org-moor-prune-unmark)
    (define-key map (kbd "x") #'org-moor-prune-execute)
    (define-key map (kbd "t") #'org-moor-prune-retitle)
    (define-key map (kbd "RET") #'org-moor-prune-visit)
    map)
  "Keys of the prune list.")

(define-derived-mode org-moor-prune-mode tabulated-list-mode "Moor-Prune"
  "Sources with a copy, to choose which copies to let go."
  (setq tabulated-list-format [("Size" 9 nil :right-align t)
                               ("Taken" 11 t)
                               ("Origin" 7 t)
                               ("Links" 6 t)
                               ("Title" 0 t)]
        tabulated-list-padding 2
        tabulated-list-entries #'org-moor-prune--entries)
  (tabulated-list-init-header))

(defun org-moor-prune--copy-at-point ()
  "Return the source on this line."
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
  "Open this source's copy."
  (interactive)
  (org-moor-open-record (org-moor-prune--copy-at-point)))

(defun org-moor-prune-retitle (title)
  "Give this source the title TITLE."
  (interactive (list (read-string "Title: " (plist-get (org-moor-prune--copy-at-point) :title))))
  (org-moor-record-set (org-moor-prune--copy-at-point) :title title)
  (tabulated-list-revert))

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
        (mapc #'org-moor-let-go marked)
        (tabulated-list-revert)))))

;;;###autoload (autoload 'org-moor-prune "org-moor" nil t)
(defun org-moor-prune ()
  "List the copies of this buffer's sources, largest first, to let some go."
  (interactive)
  (let ((sources (org-moor-sources)))
    (with-current-buffer (get-buffer-create "*org-moor prune*")
      (org-moor-prune-mode)
      (setq org-moor-prune--sources sources)
      (tabulated-list-revert)
      (pop-to-buffer-same-window (current-buffer)))))

(provide 'org-moor-prune)
;;; org-moor-prune.el ends here
