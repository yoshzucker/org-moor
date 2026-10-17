;;; org-moor-core.el --- Sources, copies and the store  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The dependency root.  Everything else requires this file and this file
;; requires nothing of ours.
;;
;; A *source* is a heading carrying an ID and a MOOR_ORIGIN.  Its *copy*, if
;; any, lives in the *store*, in a directory named by that ID, the way
;; `org-attach' names its own -- so that nothing in the store ever asks to be
;; filed.  A *place* is a page, or a text, inside a source, written as a
;; `moor:' link.
;;
;; In code a source is reached by its ID, and ID is what the argument is
;; called.

;;; Code:

(require 'org)
(require 'org-id)
(require 'ol)
(require 'subr-x)

(defgroup org-moor nil
  "Bring what drifts past into your own files, and tie it to a heading."
  :group 'org
  :prefix "org-moor-")

(defcustom org-moor-store-directory
  (expand-file-name "source/" org-directory)
  "Directory the copies of sources are kept in, one directory per source."
  :type 'directory
  :group 'org-moor)

(defconst org-moor-property-origin "MOOR_ORIGIN"
  "Where a source was when it was taken: a URL or a path.")
(defconst org-moor-property-via "MOOR_VIA"
  "How a source arrived, as a word: web, file, mail, chat.")
(defconst org-moor-property-file "MOOR_FILE"
  "The file name of a source's copy inside its store directory.")
(defconst org-moor-property-taken "MOOR_TAKEN"
  "When a source was taken, as an inactive timestamp.")

;;;; The store

(defun org-moor-store-path (id)
  "Return the store directory for the source ID.
The first two characters name a directory and the rest one below it, as
`org-attach-id-uuid-folder-format' does, so that no directory grows long."
  (unless (and (stringp id) (> (length id) 2))
    (error "org-moor: not an id: %S" id))
  (file-name-as-directory
   (expand-file-name (concat (substring id 0 2) "/" (substring id 2))
                     org-moor-store-directory)))

(defun org-moor-copy-path (id file)
  "Return the path of FILE, the copy of source ID, in the store."
  (expand-file-name file (org-moor-store-path id)))

;;;; Sources

(defun org-moor-source-p (&optional pom)
  "Non-nil when the heading at POM is a source."
  (and (org-entry-get pom org-moor-property-origin) t))

(defun org-moor-source (&optional pom)
  "Return the source at POM as a plist, or nil if the heading is not one.
Keys are :id :origin :via :file :taken and :copy, the copy's full path when
the heading names one and it is still in the store."
  (when (org-moor-source-p pom)
    (let* ((id (org-entry-get pom "ID"))
           (file (org-entry-get pom org-moor-property-file))
           (copy (and id file (org-moor-copy-path id file))))
      (list :id id
            :origin (org-entry-get pom org-moor-property-origin)
            :via (org-entry-get pom org-moor-property-via)
            :file file
            :taken (org-entry-get pom org-moor-property-taken)
            :copy (and copy (file-exists-p copy) copy)))))

(defun org-moor-set-source (origin via &optional file pom)
  "Make the heading at POM a source from ORIGIN, arrived VIA.
FILE is the copy's name in the store, if there is one.  Return the id."
  (let ((id (org-id-get-create pom)))
    (org-entry-put pom org-moor-property-origin origin)
    (org-entry-put pom org-moor-property-via (format "%s" via))
    (when file
      (org-entry-put pom org-moor-property-file file))
    (org-entry-put pom org-moor-property-taken
                   (format-time-string (org-time-stamp-format t t)))
    id))

;;;; Places

(defun org-moor-link-parse (path)
  "Split the `moor:' link PATH into (ID . PLACE).
PLACE is a page number, a text to search for, or nil."
  (if (string-match "\\`\\([^:]+\\)\\(?:::\\(.+\\)\\)?\\'" path)
      (let ((place (match-string 2 path)))
        (cons (match-string 1 path)
              (if (and place (string-match-p "\\`[0-9]+\\'" place))
                  (string-to-number place)
                place)))
    (error "org-moor: not a moor link: %s" path)))

(defcustom org-moor-text-extensions
  '("txt" "md" "markdown" "org" "csv" "tsv" "json" "yaml" "yml" "rst" "tex" "log")
  "Extensions of copies Emacs reads as text, where a place is a text.
A place in such a copy is found by searching for it, so it survives the
copy's lines moving."
  :type '(repeat string)
  :group 'org-moor)

(defun org-moor-text-file-p (file)
  "Return non-nil when FILE is read as text; see `org-moor-text-extensions'."
  (member (downcase (or (file-name-extension file) "")) org-moor-text-extensions))

(defun org-moor-text-regexp (text)
  "Return a regexp finding TEXT however its whitespace is broken."
  (mapconcat #'regexp-quote (split-string text "[ \t\n]+" t) "[ \t\n]+"))

(defun org-moor-goto-text (text)
  "Move point to the first occurrence of TEXT in the buffer.
Leave point where it is, and say so, when TEXT is not found."
  (let ((pos (save-excursion
               (goto-char (point-min))
               (and (re-search-forward (org-moor-text-regexp text) nil t)
                    (match-beginning 0)))))
    (if pos
        (progn (goto-char pos) (recenter))
      (message "org-moor: not found: %s" text))))

(defcustom org-moor-open-function nil
  "Function called with a path to open a copy, or nil for `org-open-file'.
Give it your own policy for which files the OS opens and which Emacs reads,
so that it is decided once.  A place with a page in a PDF is opened in
Emacs regardless, since only there can the page be sought; so is a place
with a text in a copy read as text."
  :type '(choice (const :tag "org-open-file" nil) function)
  :group 'org-moor)

(defun org-moor-open-copy (file &optional place)
  "Open FILE, at PLACE when there is one and Emacs can show it.
PLACE is a page of a PDF, or a text in a copy read as text."
  (cond
   ((and (integerp place) (string-match-p "\\.pdf\\'" file))
    (find-file file)
    (cond ((and (derived-mode-p 'pdf-view-mode) (fboundp 'pdf-view-goto-page))
           (pdf-view-goto-page place))
          ((and (derived-mode-p 'doc-view-mode) (fboundp 'doc-view-goto-page))
           (doc-view-goto-page place))))
   ((and (stringp place) (org-moor-text-file-p file))
    (find-file file)
    (org-moor-goto-text place))
   (t (funcall (or org-moor-open-function #'org-open-file) file))))

(defun org-moor-open-origin (origin)
  "Open ORIGIN: a URL in the browser, a path where it still is."
  (cond ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]+:[^\\]" origin)
         (browse-url origin))
        ((file-exists-p origin)
         (funcall (or org-moor-open-function #'org-open-file) origin))
        (t (user-error "org-moor: the origin is gone: %s" origin))))

(defun org-moor-link-follow (path _arg)
  "Open the place PATH: the copy at its page or text, or else the origin."
  (pcase-let* ((`(,id . ,place) (org-moor-link-parse path))
               (marker (or (org-id-find id 'marker)
                           (user-error "org-moor: no source %s" id)))
               (source (org-with-point-at marker (org-moor-source))))
    (set-marker marker nil)
    (cond
     ((plist-get source :copy)
      (org-moor-open-copy (plist-get source :copy) place))
     ((plist-get source :origin)
      (org-moor-open-origin (plist-get source :origin)))
     (t (user-error "org-moor: %s has neither copy nor origin" id)))))

(org-link-set-parameters "moor" :follow #'org-moor-link-follow)

(provide 'org-moor-core)
;;; org-moor-core.el ends here
