;;; org-moor-place.el --- Point into a source, and open it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two directions between a note and its source.
;;
;; From the source: reading a copy, `org-moor-note-here' adds the page you
;; are on -- and the text you selected, if any -- to the draft for the
;; source's heading, as a `moor:' link, and opens the draft.  Done page by
;; page, the draft collects what was read where, without a session to keep
;; open; C-c C-c writes it in, as any draft.
;;
;; From the note: `org-moor-open-source' opens the copy of the source the
;; heading is, or of one under it; with a prefix, its origin -- the page on
;; the web, or the file where it was.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-take)
(require 'org-moor-fathom)
(require 'cl-lib)

(declare-function pdf-view-current-page "pdf-view")
(declare-function pdf-view-active-region-text "pdf-view")
(declare-function pdf-view-active-region-p "pdf-view")
(declare-function doc-view-current-page "doc-view")

;;;; Which source a file is

(defun org-moor-copy-id (file)
  "Return the id of the source whose copy FILE is, or nil.
Read from where FILE lies in the store: the two levels of its directory
are the id, split after its second character."
  (let ((rel (file-relative-name (file-truename file)
                                 (file-truename org-moor-store-directory))))
    (when (string-match "\\`\\([^/.][^/]\\)/\\([^/]+\\)/[^/]+\\'" rel)
      (concat (match-string 1 rel) (match-string 2 rel)))))

(defun org-moor-current-page ()
  "Return the page shown in this buffer, or nil."
  (cond ((and (derived-mode-p 'pdf-view-mode) (fboundp 'pdf-view-current-page))
         (pdf-view-current-page))
        ((and (derived-mode-p 'doc-view-mode) (fboundp 'doc-view-current-page))
         (doc-view-current-page))))

(defun org-moor-current-selection ()
  "Return the text selected in this buffer, or nil."
  (cond ((and (derived-mode-p 'pdf-view-mode)
              (fboundp 'pdf-view-active-region-p)
              (pdf-view-active-region-p))
         (string-join (pdf-view-active-region-text) "\n"))
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))))

;;;; From the source

(defun org-moor-place-string (id page selection)
  "Return the draft text for PAGE of source ID, with SELECTION quoted."
  (concat (if page
              (format "[[moor:%s::%d][p. %d]]" id page page)
            (format "[[moor:%s][source]]" id))
          "\n"
          (if selection
              (concat "#+begin_quote\n"
                      (org-escape-code-in-string (string-trim selection))
                      "\n#+end_quote\n")
            "")))

;;;###autoload
(defun org-moor-note-here ()
  "Add the page shown here, and what is selected, to the source's draft.
The buffer is a copy in the store, or a file you are then asked to take.
The draft is the one `org-moor-fathom' opens for the source's heading."
  (interactive)
  (let* ((file (or buffer-file-name
                   (user-error "org-moor: this buffer is not a file")))
         (page (org-moor-current-page))
         (selection (org-moor-current-selection))
         (windows (current-window-configuration))
         (id (org-moor-copy-id file))
         (marker (if id
                     (or (org-id-find id 'marker)
                         (user-error "org-moor: no heading for source %s" id))
                   (unless (y-or-n-p "Not a source yet; take it? ")
                     (user-error "org-moor: not taken"))
                   (org-moor-take-file file t)))
         (id (or id (org-with-point-at marker (org-id-get)))))
    (org-moor-fathom--open marker (org-moor-place-string id page selection)
                           windows t)))

;;;; From the note

(defun org-moor--sources-at ()
  "Return the sources at and under the heading at point, as markers."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let ((end (save-excursion (org-end-of-subtree t t) (point)))
         found)
     (while (< (point) end)
       (when (and (org-at-heading-p) (org-moor-source-p))
         (push (point-marker) found))
       (outline-next-heading))
     (nreverse found))))

(defun org-moor--choose-source (markers)
  "Return one of MARKERS, asking when there is more than one."
  (if (cdr markers)
      (let ((table (mapcar (lambda (m)
                             (cons (org-with-point-at m (org-get-heading t t t t)) m))
                           markers)))
        (cdr (assoc (completing-read "Source: " table nil t) table)))
    (car markers)))

;;;###autoload
(defun org-moor-open-source (&optional origin)
  "Open the copy of the source at point, or of one under it.
With ORIGIN (\\[universal-argument]), open where it came from instead."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "org-moor: not in an Org buffer"))
  (let* ((marker (org-moor--choose-source
                  (or (org-moor--sources-at)
                      (user-error "org-moor: no source here"))))
         (source (org-with-point-at marker (org-moor-source))))
    (cond
     ((and (not origin) (plist-get source :copy))
      (org-moor-open-copy (plist-get source :copy)))
     ((plist-get source :origin)
      (when (and (not origin) (not (plist-get source :copy)))
        (message "org-moor: no copy; opening the origin"))
      (org-moor-open-origin (plist-get source :origin)))
     (t (user-error "org-moor: neither copy nor origin")))))

(provide 'org-moor-place)
;;; org-moor-place.el ends here
