;;; org-moor-place.el --- Point into a source, and open it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two directions between a heading and a source it links to.
;;
;; From the source: reading a copy, `org-moor-insert-place' notes the page
;; shown, or the line point is on, as a link -- and what is selected,
;; quoted under it -- in the heading it is read for, taken out by org-dock
;; to be written in.  The heading is the one beside, when reading with
;; `org-moor-noter'; else the one the copy was opened from; else one that
;; links to the source, asked for.
;;
;; `org-moor-noter' sets the reading up as org-noter does: the copy on the
;; left, the heading on the right, taken out to be written in, and `i' to
;; note.  A text is shown read-only, with its line marked, so that moving
;; about it cannot change it.  `q' writes the heading back.
;;
;; From the heading: `org-moor-open-source' opens the copy of a source it
;; links to; with a prefix, the source's origin -- the page on the web, or
;; the file where it was.
;;
;; `org-moor-retitle' gives a source another title, since the record that
;; holds it is out of the way, and the links here that bear the old one.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-take)
(require 'org-moor-return)
(require 'cl-lib)

(declare-function pdf-view-active-region-text "pdf-view")
(declare-function pdf-view-active-region-p "pdf-view")
(declare-function image-mode-window-get "image-mode")
(declare-function org-noter-insert-note "org-noter")
(defvar org-noter--session)

;;;; Which source a file is

(defun org-moor-copy-record (file)
  "Return the source whose copy FILE is, or nil.
Read from where FILE lies: in a source's directory, beside its record,
two levels below whichever sources directory that is.  What a copy was
made readable as lies there too, and is the same source's."
  (let* ((dir (file-name-directory (file-truename file)))
         (above (file-name-directory (directory-file-name dir)))
         (two (file-name-nondirectory (directory-file-name above))))
    (when (and (= (length two) 2)
               (file-exists-p (expand-file-name org-moor-record-file dir)))
      (org-moor--record-in dir (concat two (file-name-nondirectory
                                            (directory-file-name dir)))))))

(defun org-moor-current-page ()
  "Return the page shown in this buffer, or nil."
  ;; `pdf-view-current-page' and `doc-view-current-page' are macros for this.
  (when (derived-mode-p 'pdf-view-mode 'doc-view-mode)
    (image-mode-window-get 'page)))

(defun org-moor-current-selection ()
  "Return the text selected in this buffer, or nil."
  (cond ((and (derived-mode-p 'pdf-view-mode)
              (fboundp 'pdf-view-active-region-p)
              (pdf-view-active-region-p))
         (string-join (pdf-view-active-region-text) "\n"))
        ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun org-moor-current-place ()
  "Return (PLACE . KIND) for what this buffer shows.
The page of a PDF, KIND `page'; else the line point is on, or the first
of those selected, KIND `line'."
  (if-let* ((page (org-moor-current-page)))
      (cons page 'page)
    (cons (line-number-at-pos (if (use-region-p) (region-beginning) (point)))
          'line)))

(defun org-moor-place-string (record place kind &optional selection)
  "Return a link to PLACE of the source RECORD, with SELECTION quoted under it.
KIND is `page' or `line', what PLACE is; nil PLACE links to the source."
  (concat (org-moor-link record place kind) "\n"
          (if (and selection (not (string-blank-p selection)))
              (concat "#+begin_quote\n"
                      (org-escape-code-in-string (string-trim selection))
                      "\n#+end_quote\n")
            "")))

;;;; The heading a source is read for

(defun org-moor-headings-linking (id)
  "Return markers at the headings that link to source ID.
In the files of `org-moor-files-function' and the Org files open."
  (let ((files (delete-dups
                (append (funcall org-moor-files-function)
                        (delq nil (mapcar (lambda (b)
                                            (with-current-buffer b
                                              (and (derived-mode-p 'org-mode)
                                                   buffer-file-name)))
                                          (buffer-list))))))
        (re (concat "\\[\\[moor:" (regexp-quote id) "\\(?:::\\|\\]\\)"))
        found)
    (dolist (file files)
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             (save-excursion
               (unless (org-before-first-heading-p)
                 (org-back-to-heading t)
                 (unless (seq-find (lambda (m) (and (eq (marker-buffer m) (current-buffer))
                                                    (= m (point))))
                                   found)
                   (push (point-marker) found)))))))))
    (nreverse found)))

(defun org-moor--choose-heading (markers)
  "Return one of MARKERS at headings, asking when there is more than one."
  (if (cdr markers)
      (let ((table (mapcar (lambda (m)
                             (cons (org-with-point-at m
                                     (concat (file-name-nondirectory (buffer-file-name))
                                             ": " (org-format-outline-path
                                                   (org-get-outline-path t) 1000)))
                                   m))
                           markers)))
        (cdr (assoc (completing-read "Note in: " table nil t) table)))
    (car markers)))

(defun org-moor--end-of-own-text (pos)
  "Return the end of the text of the heading at POS, before its first child."
  (save-excursion
    (goto-char pos)
    (org-back-to-heading t)
    (outline-next-heading)
    (skip-chars-backward " \t\n")
    (point)))

(defun org-moor--dock-for (marker)
  "Return the buffer the heading at MARKER is written in, taken out.
The copy MARKER is in, when it is in one; the heading's copy, when it is
out; else the heading taken out now, point at the end of its own text."
  (with-current-buffer (marker-buffer marker)
    (cond ((org-dock-origin-file) (current-buffer))
          ((org-dock-copy-of marker))
          (t (org-dock-heading marker :at (org-moor--end-of-own-text marker)
                               :display nil)))))

(defun org-moor--write-in (buffer text)
  "Write TEXT in BUFFER, as lines of their own after the line point is on.
Its point as shown, when it is shown, and the window's point follows."
  (let ((window (get-buffer-window buffer t)))
    (with-current-buffer buffer
      (when window (goto-char (window-point window)))
      (unless (bolp)
        (end-of-line)
        (if (eobp) (insert "\n") (forward-char 1)))
      (insert text)
      (unless (bolp) (insert "\n"))
      (when window (set-window-point window (point))))))

(defvar-local org-moor-noter--session nil
  "In a copy read by `org-moor-noter', a plist.
:windows, put back when reading stops; :dock, the buffer the heading is
written in beside it; :own, non-nil when the noter took the heading out,
and so writes it back when reading stops.")

;;;###autoload (autoload 'org-moor-insert-place "org-moor" nil t)
(defun org-moor-insert-place ()
  "Note the place being read in the heading it is read for.
The page shown, or the line point is on, as a link named by the source's
title, and what is selected, quoted under it.  They are written in the
heading taken out by org-dock, after the line its point is on, and the
heading is shown.  The heading is the one beside, when reading with
`org-moor-noter'; else the one the copy was opened from; else one that
links to the source, asked for.  A file that is not a source's copy yet
is taken first.  In an org-noter session, `org-noter-insert-note' instead."
  (interactive)
  (if (and (bound-and-true-p org-noter--session) (not (derived-mode-p 'org-mode)))
      (call-interactively #'org-noter-insert-note)
    (let* ((file (or buffer-file-name
                     (user-error "org-moor: this buffer is not a file")))
           (place (org-moor-current-place))
           (selection (org-moor-current-selection))
           (record (org-moor-copy-record file))
           (taken (and (not record)
                       (if (y-or-n-p "Not a source yet; take it? ")
                           (org-moor-take-file file t)
                         (user-error "org-moor: not taken"))))
           (record (or record
                       (with-current-buffer (marker-buffer taken)
                         (save-excursion
                           (goto-char taken)
                           (and (looking-at org-moor--link-re)
                                (org-moor-record (match-string 1)))))))
           (dock (let ((beside (plist-get org-moor-noter--session :dock)))
                   (if (buffer-live-p beside)
                       beside
                     (org-moor--dock-for
                      (or taken
                          (and (markerp org-moor--read-for)
                               (buffer-live-p (marker-buffer org-moor--read-for))
                               org-moor--read-for)
                          (org-moor--choose-heading
                           (org-moor-headings-linking (plist-get record :id)))
                          (user-error "org-moor: no heading links to this source")))))))
      (org-moor--write-in dock (org-moor-place-string record (car place) (cdr place)
                                                      selection))
      (unless (get-buffer-window dock)
        (display-buffer dock '((display-buffer-reuse-window display-buffer-pop-up-window)
                               (inhibit-same-window . t))))
      (when (use-region-p) (deactivate-mark))
      (message "org-moor: noted %s %d" (if (eq (cdr place) 'page) "p." "l.") (car place)))))

(define-obsolete-function-alias 'org-moor-insert-note #'org-moor-insert-place "0.2")

;;;; From the heading

(defun org-moor--choose-record (records)
  "Return one of RECORDS, asking when there is more than one."
  (if (cdr records)
      (let ((table (mapcar (lambda (r)
                             (cons (format "%s  %s" (or (plist-get r :title) "")
                                           (substring (plist-get r :id) 0 8))
                                   r))
                           records)))
        (cdr (assoc (completing-read "Source: " table nil t) table)))
    (car records)))

(defun org-moor--record-here ()
  "Return the source of the link at point, or one the heading links to.
The heading at point, or a heading under it; asked for when several."
  (unless (derived-mode-p 'org-mode)
    (user-error "org-moor: not in an Org buffer"))
  (if-let* ((id (org-moor-link-at-point)))
      (or (org-moor-find-record id) (user-error "org-moor: no source %s" id))
    (let* ((ids (org-with-wide-buffer
                 (if (org-before-first-heading-p)
                     (org-moor-ids-in (point-min) (point-max))
                   (org-back-to-heading t)
                   (org-moor-ids-in (point) (save-excursion (org-end-of-subtree t t) (point))))))
           (records (delq nil (mapcar #'org-moor-find-record ids))))
      (org-moor--choose-record (or records (user-error "org-moor: no source here"))))))

;;;###autoload (autoload 'org-moor-open-source "org-moor" nil t)
(defun org-moor-open-source (&optional origin)
  "Open the copy of the source at point, or one the heading links to.
With ORIGIN (\\[universal-argument]), open where it came from instead."
  (interactive "P")
  (let ((record (org-moor--record-here)))
    (if origin
        (org-moor-open-origin (or (plist-get record :origin)
                                  (user-error "org-moor: no origin")))
      (unless (plist-get record :copy)
        (message "org-moor: no copy; opening the origin"))
      (org-moor-open-record record nil (org-moor--heading-marker)))))

;;;; A source's title

(defun org-moor--record-at-point ()
  "Return the source of the link at point, of the copy shown, or the heading's."
  (or (and buffer-file-name (not (derived-mode-p 'org-mode))
           (org-moor-copy-record buffer-file-name))
      (and (derived-mode-p 'org-mode) (org-moor--record-here))
      (user-error "org-moor: no source here")))

;;;###autoload (autoload 'org-moor-retitle "org-moor" nil t)
(defun org-moor-retitle (record title)
  "Give the source RECORD the title TITLE, here and in its record.
Interactively, the source of the link at point, of the copy shown, or
one the heading links to.  A link to it in this buffer whose name begins
with the old title is named with the new one, its place kept; links in
other files keep their names, which are only names."
  (interactive
   (let ((record (org-moor--record-at-point)))
     (list record (read-string "Title: " (plist-get record :title)))))
  (let ((old (plist-get record :title))
        (id (plist-get record :id))
        (n 0))
    (org-moor-record-set record :title title)
    (when (and old (not (string-empty-p old)) (derived-mode-p 'org-mode))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-link-bracket-re nil t)
         (let ((link (org-link-unescape (match-string-no-properties 1)))
               (name (match-string-no-properties 2)))
           (when (and name
                      (string-prefix-p "moor:" link)
                      (equal (save-match-data
                               (car (org-moor-link-parse (substring link 5))))
                             id)
                      (string-prefix-p old name)
                      (not (get-text-property (match-beginning 0) 'read-only)))
             (replace-match (org-link-make-string
                             link (concat title (substring name (length old))))
                            t t)
             (cl-incf n))))))
    (message "org-moor: titled %s%s" title
             (if (> n 0) (format "; %d link%s here renamed" n (if (= n 1) "" "s")) ""))))

;;;; Reading beside the heading

(declare-function evil-define-key* "evil-core")
(declare-function evil-normalize-keymaps "evil-core")

(defvar org-moor-noter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'org-moor-insert-place)
    (define-key map "q" #'org-moor-noter-quit)
    map)
  "Keymap of `org-moor-noter-mode'.
Only `i' and `q' are bound, so that the keys you move with stay yours.")

(with-eval-after-load 'evil
  (evil-define-key* 'normal org-moor-noter-mode-map
                    "i" #'org-moor-insert-place
                    "q" #'org-moor-noter-quit))

(defvar-local org-moor-noter--read-only nil
  "Whether the buffer was read-only before `org-moor-noter-mode'.")

(define-minor-mode org-moor-noter-mode
  "Read a copy without changing it; `i' notes the place at point, `q' stops.
A text is read-only, and its line marked, while the mode is on."
  :lighter " Moor"
  (unless (derived-mode-p 'pdf-view-mode 'doc-view-mode)
    (if org-moor-noter-mode
        (progn (setq org-moor-noter--read-only buffer-read-only)
               (read-only-mode 1)
               (hl-line-mode 1))
      (read-only-mode (if org-moor-noter--read-only 1 -1))
      (hl-line-mode -1)))
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps)))

(defun org-moor-noter--end (source)
  "Stop reading SOURCE, a buffer, beside its heading; put the windows back."
  (when (buffer-live-p source)
    (with-current-buffer source
      (let ((session org-moor-noter--session))
        (setq org-moor-noter--session nil)
        (org-moor-noter-mode -1)
        (when-let* ((windows (plist-get session :windows)))
          (set-window-configuration windows))))))

(defun org-moor-noter-quit ()
  "Stop reading: write the heading beside back, and put the windows back.
A heading that was out before reading began is left out, as it was."
  (interactive)
  (let* ((session org-moor-noter--session)
         (dock (plist-get session :dock)))
    (if (and (plist-get session :own) (buffer-live-p dock))
        (with-current-buffer dock (org-dock-finish))
      (org-moor-noter--end (current-buffer)))))

;;;###autoload (autoload 'org-moor-noter "org-moor" nil t)
(defun org-moor-noter ()
  "Read a source of the heading at point beside the heading, to note it.
The source of the link at point, or one the heading, or a heading under
it, links to.  A PDF, or a copy read as text, opens on the left in
`org-moor-noter-mode'; on the right, the heading, taken out by org-dock
to be written in.  `i' notes the place there, as `org-moor-insert-place';
`q' writes the heading back and puts the windows back, as C-c C-c in it
does.  A PDF is shown by pdf-tools when it is there, else by DocView.
Any other copy is opened as `org-moor-open-source' would."
  (interactive)
  (let* ((record (org-moor--record-here))
         (copy (plist-get record :copy)))
    (if (not (and copy (or (org-moor-text-file-p copy) (org-moor-pdf-p copy))))
        (org-moor-open-record record nil (org-moor--heading-marker))
      (let* ((windows (current-window-configuration))
             (in-copy (org-dock-origin-file))
             (heading (org-moor--heading-marker))
             (source nil)
             (dock (if in-copy
                       (current-buffer)
                     (org-dock-heading
                      heading :at (org-moor--end-of-own-text heading)
                      :display nil :restore nil
                      :finish (lambda (&rest _) (org-moor-noter--end source))
                      :abort (lambda () (org-moor-noter--end source))))))
        ;; The copy is shown only once its window is final: a PDF shown in
        ;; a window that is then split shows nothing in the new one.
        (delete-other-windows)
        (set-window-buffer (split-window-right) dock)
        (find-file copy)
        (setq source (current-buffer)
              org-moor--read-for heading)
        (org-moor-noter-mode 1)
        (setq org-moor-noter--session
              (list :windows windows :dock dock :own (not in-copy)))))))

(provide 'org-moor-place)
;;; org-moor-place.el ends here
