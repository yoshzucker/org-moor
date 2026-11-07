;;; org-moor-place.el --- Point into a source, and open it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Two directions between a heading and a source it links to.
;;
;; From the source: reading a replica in Emacs -- a PDF, a text --
;; `org-moor-insert-place' notes what is selected, quoted, with a link to
;; it, in the heading it is read for, taken out by org-dock to be written
;; in.  The heading is the one beside, when reading with `org-moor-noter';
;; else the one the replica was opened from; else one that links to the
;; source, asked for.  A replica read in its own program -- a page in the
;; browser, a document in Word -- is quoted from the clipboard, by
;; `org-moor-insert-quote' in the heading.  The place is the words quoted,
;; found again in any form of the source.
;;
;; `org-moor-noter' sets the reading up as org-noter does: the replica on the
;; left, the heading on the right, taken out to be written in, and `i' to
;; note.  A text is shown read-only, with its line marked, so that moving
;; about it cannot change it.  `q' writes the heading back.
;;
;; From the heading: `org-moor-open-replica' opens the replica of a source it
;; links to; `org-moor-open-origin', the source's origin -- the page on the
;; web, or the file where it was; `org-moor-open-machine-readable', what a
;; machine reads of it.  `org-moor-awaiting' lists the sources whose
;; replicas are still to be saved, and opens each in the browser.
;;
;; `org-moor-set-title' gives a source another title, since the record that
;; holds it is out of the way, and the links here that bear the old one.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-take)
(require 'org-moor-return)
(require 'cl-lib)
(require 'tabulated-list)

(declare-function pdf-view-active-region-text "pdf-view")
(declare-function pdf-view-active-region-p "pdf-view")
(declare-function image-mode-window-get "image-mode")
(declare-function org-noter-insert-note "org-noter")
(defvar org-noter--session)

;;;; Which source a file is

(defun org-moor-replica-record (file)
  "Return the source whose replica FILE is, or nil.
Read from where FILE lies: in a source's directory, beside its record,
two levels below whichever sources directory that is.  What a replica was
made readable as lies there too, and is the same source's."
  (let* ((dir (file-name-directory (file-truename file)))
         (id (org-moor--dir-id dir)))
    (and id (org-moor--record-in dir id))))

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

(defun org-moor-place-string (record selection &optional by-quote)
  "Return a link to the words SELECTION in the source RECORD, quoted under it.
The link's place is the start of the words; with BY-QUOTE, it has none,
the quote being the place -- see `org-moor-place-by-quote-p'."
  (let ((words (string-trim selection)))
    (concat (org-moor-link record (and (not by-quote) (org-moor-text-place words))) "\n"
            "#+begin_quote\n"
            (org-escape-code-in-string words)
            "\n#+end_quote\n")))

;;;; The heading a source is read for

(defun org-moor-headings-linking (id)
  "Return markers at the headings that link to source ID.
In the files of `org-moor-searched-files'."
  (let ((files (org-moor-searched-files))
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
  (org-moor-choose markers
                   (lambda (m)
                     (org-with-point-at m
                       (concat (file-name-nondirectory (buffer-file-name))
                               ": " (org-format-outline-path (org-get-outline-path t) 1000))))
                   "Note in: "))

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
      (org-moor--next-line)
      (insert text)
      (unless (bolp) (insert "\n"))
      (when window (set-window-point window (point))))))

(defvar-local org-moor-noter--session nil
  "In a replica read by `org-moor-noter', a plist.
:windows, put back when reading stops; :dock, the buffer the heading is
written in beside it; :own, non-nil when the noter took the heading out,
and so writes it back when reading stops.")

;;;###autoload (autoload 'org-moor-insert-place "org-moor" nil t)
(defun org-moor-insert-place ()
  "Note what is selected, quoted, in the heading the replica is read for.
With a link to the words, named by the source's title; see
`org-moor-place-string'.  They are written in the heading taken out by
org-dock, after the line its point is on, and the heading is shown.
The heading is the one beside, when reading with
`org-moor-noter'; else the one the replica was opened from; else one that
links to the source, asked for.  A file that is not a source's replica yet
is taken first.  In an org-noter session, `org-noter-insert-note' instead."
  (interactive)
  (if (and (bound-and-true-p org-noter--session) (not (derived-mode-p 'org-mode)))
      (call-interactively #'org-noter-insert-note)
    (let* ((file (or (org-moor-shown-file)
                     (user-error "org-moor: this buffer is not a file")))
           (selection (let ((s (org-moor-current-selection)))
                        (if (and s (not (string-blank-p s)))
                            s
                          (user-error "org-moor: select the words to quote"))))
           (record (org-moor-replica-record file))
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
      (org-moor--write-in dock (org-moor-place-string
                                record selection
                                (with-current-buffer dock (org-moor-place-by-quote-p))))
      (unless (get-buffer-window dock)
        (display-buffer dock '((display-buffer-reuse-window display-buffer-pop-up-window)
                               (inhibit-same-window . t))))
      (when (use-region-p) (deactivate-mark))
      (message "org-moor: noted %s" (org-moor-link-name record (org-moor-text-place selection))))))

;;;###autoload (autoload 'org-moor-insert-quote "org-moor" nil t)
(defun org-moor-insert-quote (record words)
  "Quote WORDS of the source RECORD here, with a link to them.
For a replica read in its own program -- a page in the browser, a
document in Word -- where the words to quote are copied.  After the line
point is on, as `org-moor-place-string' writes them.  Interactively, the
source of the link at point, or one the heading links to, and the words
on the clipboard."
  (interactive (list (org-moor--record-here) (org-moor--clipboard)))
  (when (string-blank-p words)
    (user-error "org-moor: no words to quote"))
  (org-moor--write-in (current-buffer)
                      (org-moor-place-string record words (org-moor-place-by-quote-p)))
  (message "org-moor: quoted %s" (org-moor-link-name record (org-moor-text-place words))))

(define-obsolete-function-alias 'org-moor-insert-note #'org-moor-insert-place "0.2")

;;;; From the heading

(defun org-moor--choose-record (records)
  "Return one of RECORDS, asking when there is more than one."
  (org-moor-choose records
                   (lambda (r) (format "%s  %s" (or (plist-get r :title) "")
                                       (substring (plist-get r :id) 0 8)))
                   "Source: "))

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

;;;###autoload (autoload 'org-moor-open-replica "org-moor" nil t)
(defun org-moor-open-replica (&optional how)
  "Open the replica of the source at point, or one the heading links to.
As following a link to it does: HOW, the prefix, nil for what
`org-file-apps' says, \\[universal-argument] for Emacs, two for the program
the system opens it with."
  (interactive "P")
  (org-moor-open-record (org-moor--record-here) nil (org-moor--heading-marker) how))

(define-obsolete-function-alias 'org-moor-open-source #'org-moor-open-replica "0.3")

;;;###autoload (autoload 'org-moor-open-origin "org-moor" nil t)
(defun org-moor-open-origin (&optional how)
  "Open where the source at point, or one the heading links to, came from.
A URL in the browser; a path where it still is, HOW as for
`org-moor-open-replica'."
  (interactive "P")
  (org-moor--open-origin (or (plist-get (org-moor--record-here) :origin-url)
                             (user-error "org-moor: no origin"))
                         how))

;;;###autoload (autoload 'org-moor-open-machine-readable "org-moor" nil t)
(defun org-moor-open-machine-readable ()
  "Open what a machine reads of the source at point, or that a heading links to.
Its machine readable file, in Emacs; a source without one is read by a
machine as its replica is, which `org-moor-open-replica' opens."
  (interactive)
  (let ((record (org-moor--record-here)))
    (find-file (or (plist-get record :machine-readable)
                   (user-error "org-moor: %s has no machine readable file; its replica is read as it is"
                               (plist-get record :title))))))

;;;###autoload (autoload 'org-moor-clip-origin "org-moor" nil t)
(defun org-moor-clip-origin ()
  "Put the origin of the source at point, or of one the heading links to,
on the clipboard.  Onto the kill ring too, as written in its record."
  (interactive)
  (let ((origin (or (plist-get (org-moor--record-here) :origin-url)
                    (user-error "org-moor: no origin"))))
    (kill-new origin)
    (message "org-moor: on the clipboard: %s" origin)))

;;;; A source's title

(defun org-moor--record-at-point ()
  "Return the source of the link at point, of the replica shown, or the heading's."
  (or (and (not (derived-mode-p 'org-mode)) (org-moor-shown-file)
           (org-moor-replica-record (org-moor-shown-file)))
      (and (derived-mode-p 'org-mode) (org-moor--record-here))
      (user-error "org-moor: no source here")))

;;;###autoload (autoload 'org-moor-set-origin "org-moor" nil t)
(defun org-moor-set-origin (records origin)
  "Give each source of RECORDS the origin ORIGIN, in its record.
Interactively, the sources linked in the region, else the source at
point as `org-moor-set-title' finds it; ORIGIN is read, the clipboard's
text offered.  For files taken from where they had been put, not from
where they came -- downloaded from a shared folder, unpacked from an
archive -- whose origin is that folder instead."
  (interactive
   (let ((records (if (use-region-p)
                      (or (delq nil (mapcar #'org-moor-record
                                            (org-moor-ids-in (region-beginning)
                                                             (region-end))))
                          (user-error "org-moor: no source linked in the region"))
                    (list (org-moor--record-at-point))))
         (clip (ignore-errors (string-trim (or (gui-get-selection 'CLIPBOARD) "")))))
     (list records
           (read-string (format "Origin of %d source%s: " (length records)
                                (if (cdr records) "s" ""))
                        (and clip (not (string-empty-p clip))
                             (not (string-match-p "\n" clip))
                             clip)))))
  (when (string-empty-p (string-trim origin))
    (user-error "org-moor: no origin given"))
  (dolist (record records)
    (org-moor-record-set record :origin-url (org-moor-path-url (string-trim origin))))
  (when (use-region-p) (deactivate-mark))
  (message "org-moor: origin of %d source%s set to %s"
           (length records) (if (cdr records) "s" "") origin))

(defun org-moor--newest-piled ()
  "Return the newest file in `org-moor-move-directories', or nil."
  (car (sort (seq-filter #'file-regular-p
                         (seq-mapcat (lambda (dir)
                                       (and (file-directory-p dir)
                                            (directory-files dir t "\\`[^.]")))
                                     org-moor-move-directories))
             #'file-newer-than-file-p)))

;;;###autoload (autoload 'org-moor-set-replica "org-moor" nil t)
(defun org-moor-set-replica (record file)
  "Give the source RECORD the file FILE as its replica.
For a source whose replica could not be kept -- a file behind a sign-in,
downloaded by hand after -- or is to be replaced.  FILE is moved when it
is in `org-moor-move-directories', else copied, as taking it would.
Interactively in Dired, the file at point, and the source is read by
its title; elsewhere, the source at point as `org-moor-set-title' finds
it, and the file is read, the newest of `org-moor-move-directories'
offered."
  (interactive
   (if-let* ((file (org-moor--file-at-point)))
       (list (org-moor-read-source "Replica of: ") file)
     (let ((record (org-moor--record-at-point))
           (newest (org-moor--newest-piled)))
       (list record
             (read-file-name (format "Replica of %s: " (plist-get record :title))
                             (and newest (file-name-directory newest))
                             newest t
                             (and newest (file-name-nondirectory newest)))))))
  (let* ((file (expand-file-name file))
         (name (file-name-nondirectory file))
         (old (plist-get record :replica))
         (replica (expand-file-name name (plist-get record :dir))))
    (unless (file-regular-p file)
      (user-error "org-moor: not a file: %s" file))
    (when (and old (not (y-or-n-p (format "Replace the replica %s? "
                                          (file-name-nondirectory old)))))
      (user-error "org-moor: replica kept"))
    (when (and old (not (equal (file-truename old) (file-truename replica))))
      (delete-file old))
    (if (org-moor-move-p file)
        (rename-file file replica t)
      (copy-file file replica t t))
    (org-moor-record-replica record name 'file)
    (when (and org-moor--waiting
               (equal (plist-get (plist-get org-moor--waiting :record) :id)
                      (plist-get record :id)))
      (org-moor-stop-waiting))
    (message "org-moor: %s is the replica of %s" name (plist-get record :title))))

(defun org-moor--records-to-keep ()
  "Return the sources `org-moor-keep-replica' is about, from where point is.
Those linked in the region; else the source of the link at point; else
those the heading at point, or a heading under it, links to."
  (unless (derived-mode-p 'org-mode)
    (user-error "org-moor: not in an Org buffer"))
  (let ((ids (cond ((use-region-p)
                    (org-moor-ids-in (region-beginning) (region-end)))
                   ((org-moor-link-at-point) (list (org-moor-link-at-point)))
                   (t (org-with-wide-buffer
                       (if (org-before-first-heading-p)
                           (org-moor-ids-in (point-min) (point-max))
                         (org-back-to-heading t)
                         (org-moor-ids-in (point) (save-excursion
                                                    (org-end-of-subtree t t)
                                                    (point)))))))))
    (or (delq nil (mapcar #'org-moor-find-record ids))
        (user-error "org-moor: no source here"))))

;;;###autoload (autoload 'org-moor-keep-replica "org-moor" nil t)
(defun org-moor-keep-replica (records)
  "Keep a replica of each source of RECORDS that has none, from its origin.
For a source that has only its origin -- taken by an agent, or whose
replica was let go.  A file still where its origin says is moved when it
is in `org-moor-move-directories', else copied, as taking it would.  The
sources on the web are listed by `org-moor-awaiting', to have the browser
save each.  A source whose replica is there is left as it is;
`org-moor-set-replica' replaces a replica.  Interactively, the sources
linked in the region, else the source of the link at point, else those
the heading links to."
  (interactive (list (org-moor--records-to-keep)))
  (let ((had 0)
        (web nil))
    (dolist (record records)
      (let* ((origin (plist-get record :origin-url))
             (path (and origin (org-moor-url-path origin))))
        (cond
         ((plist-get record :replica) (cl-incf had))
         ((not origin)
          (message "org-moor: %s has no origin to keep a replica from"
                   (plist-get record :title)))
         ((not path) (push record web))
         ((file-regular-p path)
          (let ((replica (expand-file-name (file-name-nondirectory path)
                                           (plist-get record :dir))))
            (if (org-moor-move-p path)
                (rename-file path replica t)
              (copy-file path replica t t))
            (org-moor-record-replica record (file-name-nondirectory path) 'file)
            (message "Kept %s" (plist-get record :title))))
         (t (message "%s" (propertize
                           (format "org-moor: the origin of %s is gone: %s"
                                   (plist-get record :title) path)
                           'face 'warning))))))
    (when (use-region-p) (deactivate-mark))
    (cond (web (org-moor-awaiting (nreverse web)))
          ((= had (length records))
           (message "org-moor: %s already kept"
                    (if (cdr records) (format "all %d sources are" had) "its replica is"))))))

;;;; Awaited replicas

(defvar-local org-moor-awaiting--records nil
  "The sources `org-moor-awaiting' lists.")

(defun org-moor-awaiting--entries ()
  "Return the entries of the sources listed, those still without a replica."
  (delq nil
        (mapcar (lambda (record)
                  (let ((record (or (org-moor--record-in (plist-get record :dir)
                                                         (plist-get record :id))
                                    record)))
                    (unless (plist-get record :replica)
                      (list record
                            (vector (string-trim (or (plist-get record :replica-requested) "")
                                                 "\\[" "\\]")
                                    (or (plist-get record :title) "")
                                    (or (plist-get record :origin-url) ""))))))
                org-moor-awaiting--records)))

(defun org-moor-awaiting-open ()
  "Open the source on this line in the browser, and wait for its replica.
See `org-moor-open-in-browser'."
  (interactive)
  (org-moor-open-in-browser (or (tabulated-list-get-id)
                                (user-error "org-moor: no source on this line"))))

(defvar org-moor-awaiting-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-moor-awaiting-open)
    map)
  "Keymap of `org-moor-awaiting-mode'.")

(define-derived-mode org-moor-awaiting-mode tabulated-list-mode "Moor-Awaiting"
  "The sources whose replicas are awaited.
\\<org-moor-awaiting-mode-map>\\[org-moor-awaiting-open] opens one in the browser, to save its replica,
and waits for it; \\[revert-buffer] lists again those still awaited."
  (setq tabulated-list-format [("Asked for" 22 t) ("Title" 40 t) ("Origin" 0 t)]
        tabulated-list-sort-key '("Asked for"))
  (add-hook 'tabulated-list-revert-hook
            (lambda () (setq tabulated-list-entries (org-moor-awaiting--entries)))
            nil t)
  (tabulated-list-init-header))

;;;###autoload (autoload 'org-moor-awaiting "org-moor" nil t)
(defun org-moor-awaiting (records)
  "List the sources of RECORDS whose replicas are awaited, to save each.
In a buffer of its own; RET there opens one in the browser and waits for
its replica, see `org-moor-open-in-browser'.  Interactively, those linked
in the region, else the source of the link at point, else those the
heading, or the file before its first heading, links to -- as
`org-moor-keep-replica' takes them."
  (interactive (list (org-moor--records-to-keep)))
  (let ((records (seq-filter (lambda (r) (not (plist-get r :replica))) records)))
    (if (not records)
        (message "org-moor: no replica awaited here")
      (with-current-buffer (get-buffer-create "*org-moor awaiting*")
        (org-moor-awaiting-mode)
        (setq org-moor-awaiting--records records
              tabulated-list-entries (org-moor-awaiting--entries))
        (tabulated-list-print)
        (pop-to-buffer (current-buffer))))))

;;;###autoload (autoload 'org-moor-set-title "org-moor" nil t)
(defun org-moor-set-title (record title)
  "Give the source RECORD the title TITLE, here and in its record.
Interactively, the source of the link at point, of the replica shown, or
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
  "Read a replica without changing it; `i' notes the place at point, `q' stops.
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
it, links to.  The replica, as Emacs can read it, opens on the left in
`org-moor-noter-mode'; on the right, the heading, taken out by org-dock
to be written in.  `i' notes the place there, as `org-moor-insert-place';
`q' writes the heading back and puts the windows back, as C-c C-c in it
does.  A PDF is shown by pdf-tools when it is there, else by DocView.
Only a PDF and a text are read so, as they are seen; any other replica
-- a page, a document -- is opened in its own program, as
`org-moor-open-replica' opens it, to be quoted by
`org-moor-insert-quote'."
  (interactive)
  (let* ((record (org-moor--record-here))
         (replica (plist-get record :replica)))
    (if (not (and replica (org-moor-file-to-read replica)))
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
        ;; The replica is shown only once its window is final: a PDF shown in
        ;; a window that is then split shows nothing in the new one.
        (delete-other-windows)
        (set-window-buffer (split-window-right) dock)
        (org-moor-open-to-read
         (org-moor-file-to-read replica)
         (lambda ()
           (setq source (current-buffer)
                 org-moor--read-for heading)
           (org-moor-noter-mode 1)
           (setq org-moor-noter--session
                 (list :windows windows :dock dock :own (not in-copy)))))))))

(provide 'org-moor-place)
;;; org-moor-place.el ends here
