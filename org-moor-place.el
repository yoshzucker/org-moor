;;; org-moor-place.el --- Point into a source, and open it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two directions between a note and its source.
;;
;; From the source: reading a copy, `org-moor-insert-note' adds the page you
;; are on -- and the text you selected, if any -- to the draft for the
;; source's heading, as a `moor:' link, and opens the draft.  Done page by
;; page, the draft collects what was read where, without a session to keep
;; open; C-c C-c writes it in, as any draft.  In a copy read as text there
;; are no pages: the place is the words at point, as few as find it.  In an
;; org-noter session it leaves the note to org-noter, so that one command
;; serves wherever you read.
;;
;; `org-moor-noter' sets the reading up as org-noter does: the copy on the
;; left, the source's heading on the right, and `i' to note -- a link in
;; the draft, the same for a PDF as for a text.  A text is shown read-only,
;; with the line marked, so that moving about it cannot change it.
;;
;; From the note: `org-moor-open-source' opens the copy of the source the
;; heading is, or of one under it; with a prefix, its origin -- the page on
;; the web, or the file where it was.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-take)
(require 'org-moor-fathom)
(require 'cl-lib)

(declare-function pdf-view-active-region-text "pdf-view")
(declare-function pdf-view-active-region-p "pdf-view")
(declare-function image-mode-window-get "image-mode")
(declare-function org-noter "org-noter")
(declare-function org-noter-insert-note "org-noter")
(defvar org-noter--session)

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

(defcustom org-moor-place-text-length 80
  "Most characters a place in a text copy is given to find itself."
  :type 'natnum
  :group 'org-moor)

(defun org-moor-current-text ()
  "Return the words at point that find it in this buffer, or nil.
From the start of the selection, or of the line, as few words as make
their first occurrence this one -- but no more than
`org-moor-place-text-length' characters, which then find the first one
alike."
  (unless (derived-mode-p 'image-mode 'pdf-view-mode 'doc-view-mode)
    (save-excursion
      (if (use-region-p) (goto-char (region-beginning)) (back-to-indentation))
      (skip-chars-forward " \t\n")
      (let* ((start (point))
             (words (split-string
                     (buffer-substring-no-properties
                      start (min (point-max) (+ start (* 2 org-moor-place-text-length))))
                     "[ \t\n]+" t))
             text)
        (catch 'found
          (dolist (word words)
            (let ((longer (if text (concat text " " word) word)))
              (when (> (length longer) org-moor-place-text-length)
                (throw 'found nil))
              (setq text longer)
              (when (= start (save-excursion
                               (goto-char (point-min))
                               (re-search-forward (org-moor-text-regexp text))
                               (match-beginning 0)))
                (throw 'found nil)))))
        text))))

;;;; From the source

(defun org-moor-place-string (id place selection &optional line)
  "Return the draft text for PLACE of source ID, with SELECTION quoted.
PLACE is a page number, a text to find, or nil for the source itself.
LINE, the line a text was found on, names its link; the text still finds
it when the lines have moved."
  (concat (cond ((integerp place)
                 (org-link-make-string (format "moor:%s::%d" id place)
                                       (format "p. %d" place)))
                (place
                 (org-link-make-string (format "moor:%s::%s" id place)
                                       (if line
                                           (format "l. %d" line)
                                         (truncate-string-to-width place 30 nil nil "…"))))
                (t (org-link-make-string (format "moor:%s" id) "source")))
          "\n"
          (if selection
              (concat "#+begin_quote\n"
                      (org-escape-code-in-string (string-trim selection))
                      "\n#+end_quote\n")
            "")))

;;;###autoload
(defun org-moor-insert-note ()
  "Add the place shown here, and what is selected, to the source's draft.
The place is the page, or in a text the words at point.  The buffer is a
copy in the store, or a file you are then asked to take.  The draft is
the one `org-moor-fathom' opens for the source's heading.  In an
org-noter session, `org-noter-insert-note' instead."
  (interactive)
  (if (bound-and-true-p org-noter--session)
      (call-interactively #'org-noter-insert-note)
    (org-moor--insert-note)))

(define-obsolete-function-alias 'org-moor-note-here #'org-moor-insert-note "0.1")

(defun org-moor--insert-note ()
  "Add the place shown here to the source's draft; see `org-moor-insert-note'."
  (let* ((file (or buffer-file-name
                   (user-error "org-moor: this buffer is not a file")))
         (page (or (org-moor-current-page) (org-moor-current-text)))
         (line (and (stringp page)
                    (line-number-at-pos (if (use-region-p) (region-beginning) (point)))))
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
    (org-moor-fathom--open marker (org-moor-place-string id page selection line)
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
  (org-moor--open-source-at (org-moor--source-here) origin))

(defun org-moor--open-source-at (marker &optional origin)
  "Open the copy of the source at MARKER, or with ORIGIN its origin."
  (let ((source (org-with-point-at marker (org-moor-source))))
    (cond
     ((and (not origin) (plist-get source :copy))
      (org-moor-open-copy (plist-get source :copy)))
     ((plist-get source :origin)
      (when (and (not origin) (not (plist-get source :copy)))
        (message "org-moor: no copy; opening the origin"))
      (org-moor-open-origin (plist-get source :origin)))
     (t (user-error "org-moor: neither copy nor origin")))))

(defun org-moor--source-here ()
  "Return the marker of the source at point, or of one under it."
  (unless (derived-mode-p 'org-mode)
    (user-error "org-moor: not in an Org buffer"))
  (org-moor--choose-source
   (or (org-moor--sources-at)
       (user-error "org-moor: no source here"))))

;;;; Reading beside the heading

(declare-function evil-define-key* "evil-core")
(declare-function evil-normalize-keymaps "evil-core")

(defvar org-moor-noter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'org-moor-insert-note)
    (define-key map "q" #'org-moor-noter-quit)
    map)
  "Keymap of `org-moor-noter-mode'.
Only `i' and `q' are bound, so that the keys you move with stay yours.")

(with-eval-after-load 'evil
  (evil-define-key* 'normal org-moor-noter-mode-map
                    "i" #'org-moor-insert-note
                    "q" #'org-moor-noter-quit))

(defvar-local org-moor-noter--read-only nil
  "Whether the buffer was read-only before `org-moor-noter-mode'.")

(defvar-local org-moor-noter--session nil
  "In a copy read by `org-moor-noter': (WINDOWS . HEADING-BUFFER).
WINDOWS are the windows from before, put back on `org-moor-noter-quit'.")

(defun org-moor-noter-quit ()
  "Stop reading: close the heading beside, and put the windows back."
  (interactive)
  (let ((session org-moor-noter--session))
    (setq org-moor-noter--session nil)
    (org-moor-noter-mode -1)
    (when (buffer-live-p (cdr session))
      (kill-buffer (cdr session)))
    (if (car session)
        (set-window-configuration (car session))
      (quit-window))))

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

(defvar-local org-moor--heading-marker nil
  "In a buffer from `org-moor--heading-buffer', the heading it shows.")

(defun org-moor--heading-show ()
  "Narrow this buffer to its heading again, and show all of it."
  (widen)
  (goto-char org-moor--heading-marker)
  (org-narrow-to-subtree)
  (org-fold-show-subtree))

(defun org-moor--heading-buffer (marker)
  "Return a buffer showing only the source heading at MARKER."
  (let* ((title (org-with-point-at marker (org-get-heading t t t t)))
         (name (format "*moor: %s*" title))
         (buffer (get-buffer name)))
    (unless (buffer-live-p buffer)
      (setq buffer (make-indirect-buffer (marker-buffer marker) name t))
      (with-current-buffer buffer
        (setq org-moor--heading-marker (copy-marker marker))
        (org-moor--heading-show)))
    buffer))

(defun org-moor--heading-refresh ()
  "Show what was just written at the heading here in its heading buffers.
Text written at the end of a narrowed indirect buffer falls outside it."
  (let ((base (or (buffer-base-buffer) (current-buffer)))
        (pos (point)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and org-moor--heading-marker
                   (eq (buffer-base-buffer) base)
                   (= org-moor--heading-marker pos))
          (org-moor--heading-show)
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-point window (point-max))))))))

(add-hook 'org-moor-fathom-written-hook #'org-moor--heading-refresh)

;;;###autoload
(defun org-moor-noter ()
  "Read the source at point, or one under it, beside its heading.
A PDF, or a copy read as text, opens on the left in
`org-moor-noter-mode', the heading on the right.  `i' notes the place,
as `org-moor-insert-note'; `q' stops, as `org-moor-noter-quit'.  A PDF is
shown by pdf-tools when it is there, else by DocView.  Any other copy is
opened as `org-moor-open-source' would."
  (interactive)
  (let* ((marker (org-moor--source-here))
         (copy (plist-get (org-with-point-at marker (org-moor-source)) :copy)))
    (if (and copy (or (org-moor-text-file-p copy)
                      (string-match-p "\\.pdf\\'" copy)))
        ;; The copy is shown only once its window is final: a PDF shown in
        ;; a window that is then split shows nothing in the new one.
        (let ((windows (current-window-configuration))
              (heading (org-moor--heading-buffer marker)))
          (delete-other-windows)
          (set-window-buffer (split-window-right) heading)
          (find-file copy)
          (org-moor-noter-mode 1)
          (setq org-moor-noter--session (cons windows heading)))
      (org-moor--open-source-at marker))))

(provide 'org-moor-place)
;;; org-moor-place.el ends here
