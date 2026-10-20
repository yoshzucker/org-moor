;;; org-moor-core.el --- Sources, their records and their places  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The dependency root.  Everything else requires this file, and this file
;; requires nothing of ours but org-dock, whose copies it reads as their
;; headings would be read.
;;
;; A *source* is a directory in a *sources directory*, named by the
;; source's id the way `org-attach' names its own: the id's first two
;; characters, and the rest below them.  In it are the source's *copy*, if
;; it has one, and its *record*, `moor.org', saying what it is: a title, and
;; where it came from, how and when.  Nothing in a sources directory asks to
;; be filed, and nothing in an Org file has to be a source's: what you write
;; points to a source with a `moor:' link, from wherever you write it, as
;; often as you like.
;;
;; A *place* is a page of a source, or a line, written as `moor:ID::N'.
;;
;; In code a source is reached by its id, and ID is what the argument is
;; called; RECORD is a source as a plist, read from its record.

;;; Code:

(require 'org)
(require 'org-id)
(require 'ol)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'url-util)
(require 'org-dock)

(defgroup org-moor nil
  "Bring what drifts past into your own files, and tie it to a heading."
  :group 'org
  :prefix "org-moor-")

(define-obsolete-variable-alias 'org-moor-store-directory
  'org-moor-sources-directory "0.2")

(defcustom org-moor-sources-directory
  (expand-file-name "sources/" org-directory)
  "Directory the sources are kept in, one directory each.
For every Org file that `org-moor-sources-directory-functions' says
nothing about.  Keep it beside the Org files, so that they travel
together, and out of everyday search: a directory of PDFs and slides
makes a text search slow, and is better searched on purpose."
  :type 'directory
  :group 'org-moor)

(defvar org-moor-sources-directory-functions nil
  "Functions saying where the sources of an Org file are kept.
Each is called with the file's name and returns a directory, or nil for
a file it says nothing about; `org-moor-sources-directory' is the answer
for the rest.  A package that keeps a file's sources beside the file --
a document's in a directory of its own, say -- adds one here.")

(defconst org-moor-record-file "moor.org"
  "Name of the file in a source's directory saying what the source is.")

(defconst org-moor--record-keys
  '((:origin . "MOOR_ORIGIN")
    (:via . "MOOR_VIA")
    (:file . "MOOR_FILE")
    (:taken . "MOOR_TAKEN"))
  "The properties of a record, by the keys a RECORD plist has them under.
Origin: where the source was when it was taken, a URL or a path, kept
after it changes or is gone.  Via: how it arrived, as a word: web, file,
mail, chat, clipboard.  File: the copy's name in the source's directory.  Taken:
when, as an inactive timestamp.")

;;;; Where sources are

(defun org-moor-org-file ()
  "Return the Org file this buffer stands for.
Its own, or, in a copy of a heading taken out by org-dock, the heading's."
  (or (org-dock-origin-file) buffer-file-name))

(defun org-moor-sources (&optional file)
  "Return the sources directory of FILE, by default this buffer's Org file."
  (let ((file (or file (org-moor-org-file))))
    (file-name-as-directory
     (expand-file-name
      (or (and file (run-hook-with-args-until-success
                     'org-moor-sources-directory-functions file))
          org-moor-sources-directory)))))

(defun org-moor--dir-id (dir)
  "Return the id of the source whose directory is DIR, or nil.
Read back from its name, as `org-moor-source-directory' makes it."
  (let* ((dir (directory-file-name dir))
         (two (file-name-nondirectory (directory-file-name (file-name-directory dir)))))
    (and (= (length two) 2) (concat two (file-name-nondirectory dir)))))

(defun org-moor-source-directory (id &optional sources)
  "Return the directory of source ID in SOURCES, by default this buffer's.
The id's first two characters name a directory and the rest one below
it, as `org-attach-id-uuid-folder-format' does, so that no directory
grows long."
  (unless (and (stringp id) (> (length id) 2))
    (error "org-moor: not an id: %S" id))
  (file-name-as-directory
   (expand-file-name (concat (substring id 0 2) "/" (substring id 2))
                     (or sources (org-moor-sources)))))

;;;; Records

(defun org-moor--record-in (dir id)
  "Return the record of source ID in its directory DIR, or nil."
  (let ((file (expand-file-name org-moor-record-file dir)))
    (when (file-readable-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (let ((record (list :id id :dir dir))
              (case-fold-search t))
          (pcase-dolist (`(,key . ,name) org-moor--record-keys)
            (goto-char (point-min))
            (when (re-search-forward
                   (concat "^[ \t]*:" name ":[ \t]*\\(.*?\\)[ \t]*$") nil t)
              (setq record (plist-put record key (match-string 1)))))
          (goto-char (point-min))
          (when (re-search-forward "^#\\+title:[ \t]*\\(.*?\\)[ \t]*$" nil t)
            (setq record (plist-put record :title (match-string 1))))
          (let ((copy (and (plist-get record :file)
                           (expand-file-name (plist-get record :file) dir))))
            (plist-put record :copy (and copy (file-exists-p copy) copy))))))))

(defun org-moor-record (id &optional sources)
  "Return source ID as a plist, read from its record, or nil.
In SOURCES, by default this buffer's sources directory.  Keys are :id,
:title, :origin, :via, :file, :taken, :dir, its directory, and :copy,
the copy's path while it is there."
  (org-moor--record-in (org-moor-source-directory id sources) id))

(defun org-moor-sources-in (sources)
  "Return every source in the directory SOURCES, as records."
  (let (records)
    (dolist (two (ignore-errors (directory-files sources t "\\`[^.].\\'")))
      (when (file-directory-p two)
        (dolist (dir (directory-files two t "\\`[^.]"))
          (when (file-directory-p dir)
            (when-let* ((record (org-moor--record-in (file-name-as-directory dir)
                                                     (org-moor--dir-id dir))))
              (push record records))))))
    records))

(defun org-moor-find-record (id)
  "Return source ID from this buffer's sources directory, or the default's.
A link written where one sources directory is read, and followed where
another is, still finds its source on a machine that has both."
  (or (org-moor-record id)
      (org-moor-record id org-moor-sources-directory)))

(defun org-moor--property-line (name value)
  "Return the record's property line of NAME holding VALUE, without newline."
  (format "%-13s %s" (concat ":" name ":") value))

(defun org-moor--record-text (record)
  "Return the text of a record file for RECORD."
  (concat ":PROPERTIES:\n"
          (mapconcat (pcase-lambda (`(,key . ,name))
                       (when-let* ((value (plist-get record key)))
                         (concat (org-moor--property-line name value) "\n")))
                     org-moor--record-keys "")
          ":END:\n"
          "#+title: " (or (plist-get record :title) "") "\n"))

(defun org-moor--write-text (file text)
  "Write TEXT to FILE, as UTF-8."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file (insert text))))

(defun org-moor-new-source (title origin via &optional sources)
  "Make a source titled TITLE from ORIGIN, arrived VIA, and return its record.
In SOURCES, by default this buffer's sources directory.  Its directory
and record are made; its copy is for the caller to put there."
  (let* ((id (org-id-uuid))
         (dir (org-moor-source-directory id sources))
         (record (list :id id :dir dir :title title :origin origin
                       :via (format "%s" via)
                       :taken (format-time-string (org-time-stamp-format t t)))))
    (make-directory dir t)
    (org-moor--write-text (expand-file-name org-moor-record-file dir)
                          (org-moor--record-text record))
    record))

(defun org-moor-record-set (record key value)
  "Set KEY of RECORD to VALUE in its record file, and return RECORD so.
KEY is :title or a key of `org-moor--record-keys'.  Only that line is
written, so that anything else written in the record stays."
  (let ((file (expand-file-name org-moor-record-file (plist-get record :dir)))
        (case-fold-search t))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file))
      (goto-char (point-min))
      (if (eq key :title)
          (if (re-search-forward "^#\\+title:.*$" nil t)
              (replace-match (concat "#+title: " value) t t)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "#+title: " value "\n"))
        (let ((name (or (alist-get key org-moor--record-keys)
                        (error "org-moor: not a key of a record: %S" key))))
          (if (re-search-forward (concat "^[ \t]*:" name ":.*$") nil t)
              (replace-match (org-moor--property-line name value) t t)
            (if (re-search-forward "^[ \t]*:END:" nil t)
                (progn (beginning-of-line)
                       (insert (org-moor--property-line name value) "\n"))
              (goto-char (point-min))
              (insert ":PROPERTIES:\n"
                      (org-moor--property-line name value) "\n"
                      ":END:\n")))))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region nil nil file nil 'silent))))
  (let ((record (plist-put record key value)))
    (if (eq key :file)
        (let ((copy (expand-file-name value (plist-get record :dir))))
          (plist-put record :copy (and (file-exists-p copy) copy)))
      record)))

;;;; Places

(defun org-moor-link-parse (path)
  "Split the `moor:' link PATH into (ID . PLACE).
PLACE is a number -- a page in a PDF, a line in anything else -- a text
to search for, as links written before lines were used have, or nil."
  (if (string-match "\\`\\([^:]+\\)\\(?:::\\(.+\\)\\)?\\'" path)
      (let ((place (match-string 2 path)))
        (cons (match-string 1 path)
              (if (and place (string-match-p "\\`[0-9]+\\'" place))
                  (string-to-number place)
                place)))
    (error "org-moor: not a moor link: %s" path)))

(defun org-moor-extension (file)
  "Return FILE's extension, lower case, or \"\" when it has none."
  (downcase (or (file-name-extension file) "")))

(defcustom org-moor-text-extensions
  '("txt" "md" "markdown" "org" "csv" "tsv" "json" "yaml" "yml" "rst" "tex" "log")
  "Extensions of copies Emacs reads as text, where a place is a line."
  :type '(repeat string)
  :group 'org-moor)

(defun org-moor-text-file-p (file)
  "Return non-nil when FILE is read as text; see `org-moor-text-extensions'."
  (member (org-moor-extension file) org-moor-text-extensions))

(defun org-moor-pdf-p (file)
  "Return non-nil when FILE is a PDF, where a place is a page."
  (equal (org-moor-extension file) "pdf"))

(defcustom org-moor-eww-extensions '("html" "htm")
  "Extensions of copies read in EWW, where a place is a text.
Only where a copy is read beside its heading, or a text in it is sought;
otherwise a page is opened as `org-file-apps' says."
  :type '(repeat string)
  :group 'org-moor)

(defun org-moor-eww-p (file)
  "Return non-nil when FILE is read in EWW; see `org-moor-eww-extensions'."
  (member (org-moor-extension file) org-moor-eww-extensions))

(defun org-moor-file-url-path (url)
  "Return the file a file: URL names: file:///C:/a%20b.pdf is C:/a b.pdf."
  (let ((path (decode-coding-string
               (url-unhex-string (replace-regexp-in-string
                                  "\\`file:\\(//[^/]*\\)?" "" url))
               'utf-8)))
    (if (string-match-p "\\`/[A-Za-z]:" path) (substring path 1) path)))

(defvar eww-data)

(defun org-moor-shown-file ()
  "Return the file this buffer shows: the file visited, or what EWW shows."
  (or buffer-file-name
      (and (derived-mode-p 'eww-mode)
           (let ((url (plist-get eww-data :url)))
             (and url (string-prefix-p "file:" url)
                  (org-moor-file-url-path url))))))

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

;;;; What an agent reads

(define-obsolete-variable-alias 'org-moor-draft-converters
  'org-moor-readable-converters "0.2")

(defcustom org-moor-readable-converters
  '(("docx" . org-moor-pandoc)
    ("pptx" . org-moor-pandoc)
    ("odt" . org-moor-pandoc)
    ("epub" . org-moor-pandoc)
    ("html" . org-moor-pandoc)
    ("htm" . org-moor-pandoc))
  "How a copy is made readable by an agent, by extension.
Each function is called with the copy's path and returns the path of a
file to read in its place.  A copy whose extension is not here is read
as it is: text, Markdown and PDF need nothing."
  :type '(alist :key-type string :value-type function)
  :group 'org-moor)

(defun org-moor--process-string (program &rest args)
  "Return what PROGRAM prints with ARGS, or signal with what it complained."
  (unless (executable-find program)
    (user-error "org-moor: %s is not installed" program))
  (with-temp-buffer
    (let ((status (let ((coding-system-for-read 'utf-8))
                    (apply #'call-process program nil t nil args))))
      (unless (eq status 0)
        (user-error "org-moor: %s failed: %s" program
                    (string-trim (buffer-string))))
      (buffer-string))))

(defun org-moor-pandoc (file)
  "Return FILE turned into Markdown by pandoc, beside it.
Made once, and again only when FILE is newer; it goes with the copy."
  (let ((md (concat file ".md")))
    (unless (file-newer-than-file-p md file)
      (org-moor--process-string "pandoc" "--to=gfm" "--wrap=none" "-o" md file))
    md))

(defun org-moor-readable (copy)
  "Return the file an agent reads for COPY."
  (let ((fn (cdr (assoc (org-moor-extension copy) org-moor-readable-converters))))
    (if fn (funcall fn copy) copy)))

(defun org-moor--made-readable (copy)
  "Return the file COPY was made readable as, if it was, or nil."
  (and (assoc (org-moor-extension copy) org-moor-readable-converters)
       (let ((md (concat copy ".md")))
         (and (file-exists-p md) md))))

;;;; Opening

(make-obsolete-variable 'org-moor-open-function
                        "a copy is opened by `org-open-file', as `org-file-apps' says."
                        "0.3")

(defun org-moor--eww-open (file then)
  "Show FILE in EWW, and call THEN in its buffer once it is drawn.
EWW draws a file when it has read it, which may be later; what is set in
the buffer before then is lost when it is drawn."
  (let (hook)
    (setq hook (lambda ()
                 (when (equal (org-moor-shown-file) (expand-file-name file))
                   (remove-hook 'eww-after-render-hook hook)
                   (when then (funcall then)))))
    (add-hook 'eww-after-render-hook hook)
    (eww-open-file file)))

(defun org-moor-open-copy (file &optional place then how)
  "Open FILE, at PLACE when there is one and Emacs can show it.
PLACE is a page of a PDF; a line of a copy read as text, or of what a
copy was made readable as, where an agent read it; or a text to find,
in a copy read as text or in a page, which EWW shows.  Without PLACE,
FILE is opened by `org-open-file' as a `file:' link is, HOW being its
IN-EMACS: nil for what `org-file-apps' says, (4) for Emacs, (16) for
the program the system opens it with, which is so with PLACE too.
THEN, when given, is called in the buffer that shows FILE, once it is
shown; not when FILE is opened outside Emacs."
  (let ((readable (and (integerp place) (not (org-moor-pdf-p file))
                       (not (org-moor-text-file-p file))
                       (org-moor--made-readable file)))
        (place (unless (equal how '(16)) place)))
    (cond
     ((and (integerp place) (org-moor-pdf-p file))
      (find-file file)
      (cond ((and (derived-mode-p 'pdf-view-mode) (fboundp 'pdf-view-goto-page))
             (pdf-view-goto-page place))
            ((and (derived-mode-p 'doc-view-mode) (fboundp 'doc-view-goto-page))
             (doc-view-goto-page place)))
      (when then (funcall then)))
     ((and (integerp place) (or (org-moor-text-file-p file) readable))
      (find-file (or readable file))
      (goto-char (point-min))
      (forward-line (1- place))
      (recenter)
      (when then (funcall then)))
     ((and (stringp place) (org-moor-text-file-p file))
      (find-file file)
      (org-moor-goto-text place)
      (when then (funcall then)))
     ((and (stringp place) (org-moor-eww-p file))
      (org-moor--eww-open file (lambda ()
                                 (org-moor-goto-text place)
                                 (when then (funcall then)))))
     (t (org-open-file file how)
        (when (and then (equal (org-moor-shown-file) (expand-file-name file)))
          (funcall then))))))

(defun org-moor-file-to-read (copy)
  "Return the file COPY is read beside its heading as, or nil.
COPY itself when Emacs shows it -- a PDF, a text, a page in EWW -- else
what it is made readable as, by `org-moor-readable-converters'; nil when
it is neither."
  (cond ((or (org-moor-text-file-p copy) (org-moor-pdf-p copy) (org-moor-eww-p copy))
         copy)
        ((assoc (org-moor-extension copy) org-moor-readable-converters)
         (org-moor-readable copy))))

(defun org-moor-open-to-read (file then)
  "Show FILE, from `org-moor-file-to-read', and call THEN in its buffer."
  (if (org-moor-eww-p file)
      (org-moor--eww-open file then)
    (find-file file)
    (funcall then)))

(defun org-moor--open-origin (origin &optional how)
  "Open ORIGIN: a URL in the browser, a path where it still is.
HOW is as `org-moor-open-copy' has it, for a path."
  (cond ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]+:[^\\]" origin)
         (browse-url origin))
        ((file-exists-p origin) (org-open-file origin how))
        (t (user-error "org-moor: the origin is gone: %s" origin))))

(defvar-local org-moor--read-for nil
  "In a buffer showing a copy, a marker at the heading it was opened from.
Where what is noted from it is written; see `org-moor-insert-place'.")

(defun org-moor--heading-marker ()
  "Return a marker at the heading point is in, or nil."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (unless (org-before-first-heading-p)
           (org-back-to-heading t)
           (point-marker)))))

(defun org-moor-open-record (record &optional place from how)
  "Open the copy of RECORD at PLACE, or else its origin, saying so.
FROM, a marker at a heading, is kept in the copy's buffer as the heading
it is read for.  HOW is as `org-moor-open-copy' has it."
  (cond
   ((plist-get record :copy)
    (org-moor-open-copy (plist-get record :copy) place
                        (and from (lambda () (setq org-moor--read-for from)))
                        how))
   ((plist-get record :origin)
    (org-moor--open-origin (plist-get record :origin) how)
    ;; Said last, so that nothing said in opening hides it: what is open
    ;; now is the original, not a copy that can come to no harm.
    (message "%s" (propertize
                   (format "org-moor: no copy of %s; opened the original: %s"
                           (plist-get record :title) (plist-get record :origin))
                   'face 'warning)))
   (t (user-error "org-moor: %s has neither copy nor origin" (plist-get record :id)))))

(defun org-moor-link-follow (path arg)
  "Open the place PATH: the copy at its page or line, or else the origin.
ARG is the prefix `org-open-at-point' was given, as for a `file:' link:
none opens the copy as `org-file-apps' says, \\[universal-argument] in Emacs,
two in the program the system opens it with."
  (pcase-let* ((`(,id . ,place) (org-moor-link-parse path))
               (record (or (org-moor-find-record id)
                           (user-error "org-moor: no source %s in %s" id
                                       (abbreviate-file-name (org-moor-sources))))))
    (org-moor-open-record record place (org-moor--heading-marker) arg)))

;;;; Links

(defcustom org-moor-text-place-length 40
  "Characters of a text a place is kept by, at most.
Enough to find it again; the whole of what was selected is quoted."
  :type 'integer
  :group 'org-moor)

(defun org-moor-text-place (text)
  "Return the start of TEXT, a place to find it by in a link.
On one line, without the brackets a link cannot hold, and not longer
than `org-moor-text-place-length'; nil when nothing is left."
  (let* ((words (split-string (replace-regexp-in-string "[][]" " " text) "[ \t\n]+" t))
         (place (string-join words " ")))
    (unless (string-empty-p place)
      (if (> (length place) org-moor-text-place-length)
          (string-trim-right (substring place 0 org-moor-text-place-length))
        place))))

(defun org-moor-link-name (record &optional place kind)
  "Return the name of a link to RECORD, or to PLACE in it: its title.
KIND is `page', `line' or `text', what PLACE is; a text is a string."
  (let ((title (or (plist-get record :title) "source")))
    (cond ((stringp place) (format "%s \u201c%s\u201d" title place))
          (place (format "%s %s %d" title (if (eq kind 'page) "p." "l.") place))
          (t title))))

(defun org-moor-link (record &optional place kind)
  "Return a link to RECORD, or to PLACE in it, named by the source's title.
KIND is `page', `line' or `text', what PLACE is.  The name is the title, so that
which source it is can be read where the link is: no heading says it."
  (let ((id (plist-get record :id)))
    (org-link-make-string
     (if place (format "moor:%s::%s" id place) (format "moor:%s" id))
     (org-moor-link-name record place kind))))

(defun org-moor-read-source (&optional prompt)
  "Read a source by its title, newest taken first, and return its record.
From this buffer's sources directory, and the default one."
  (let* ((records (seq-uniq
                   (append (org-moor-sources-in (org-moor-sources))
                           (org-moor-sources-in org-moor-sources-directory))
                   (lambda (a b) (equal (plist-get a :id) (plist-get b :id)))))
         (records (sort records (lambda (a b)
                                  (string> (or (plist-get a :taken) "")
                                           (or (plist-get b :taken) "")))))
         (table (mapcar (lambda (r)
                          (cons (format "%s  %s %s  %s"
                                        (or (plist-get r :title) "")
                                        (or (plist-get r :taken) "")
                                        (or (plist-get r :via) "")
                                        (substring (plist-get r :id) 0 8))
                                r))
                        records)))
    (unless table
      (user-error "org-moor: no source in %s" (abbreviate-file-name (org-moor-sources))))
    (cdr (assoc (completing-read
                 (or prompt "Source: ")
                 (lambda (string pred action)
                   (if (eq action 'metadata)
                       '(metadata (display-sort-function . identity))
                     (complete-with-action action table string pred)))
                 nil t)
                table))))

(defun org-moor-link-complete (&optional _arg)
  "Return a `moor:' link to a source read by its title, for `org-insert-link'."
  (concat "moor:" (plist-get (org-moor-read-source) :id)))

(defun org-moor-link-description (link _description)
  "Return the name a `moor:' LINK is given: its source's title, and its place."
  (pcase-let* ((`(,id . ,place) (org-moor-link-parse (string-remove-prefix "moor:" link)))
               (record (org-moor-find-record id)))
    (when record
      (cond ((numberp place)
             (org-moor-link-name record place
                                 (if (and (plist-get record :copy)
                                          (org-moor-pdf-p (plist-get record :copy)))
                                     'page 'line)))
            (place (org-moor-link-name record place 'text))
            (t (org-moor-link-name record))))))

(org-link-set-parameters "moor"
                         :follow #'org-moor-link-follow
                         :complete #'org-moor-link-complete
                         :insert-description #'org-moor-link-description)

(defconst org-moor--link-re "\\[\\[moor:\\([^]:\n]+\\)\\(?:::\\([^]\n]*\\)\\)?\\]"
  "A `moor:' link's start, its id in group 1 and its place in group 2.")

(defun org-moor-ids-in (beg end)
  "Return the ids of the sources linked from BEG to END, each once.
In the order they are first linked; a link in a property counts."
  (let (ids)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-moor--link-re end t)
        (cl-pushnew (match-string-no-properties 1) ids :test #'equal)))
    (nreverse ids)))

(defun org-moor-link-at-point ()
  "Return the id of the `moor:' link at point, or nil.
In a property too, where Org does not see a link as one."
  (when (org-in-regexp org-link-bracket-re 1)
    (let ((link (org-link-unescape (match-string-no-properties 1))))
      (and (string-prefix-p "moor:" link)
           (car (org-moor-link-parse (substring link 5)))))))

;;;; Handed to an agent with a copy

(defun org-moor-link-file (type path)
  "Return the file a `moor:' link of PATH points to, for an agent to read.
TYPE is the link's type.  As (FILE . NAME): the copy, made readable, and
the source named by its id and its title, which a place is cited by.
Nil when the source has no copy."
  (when (equal type "moor")
    (let* ((record (ignore-errors (org-moor-find-record (car (org-moor-link-parse path)))))
           (copy (and record (plist-get record :copy))))
      (and copy
           (cons
            ;; A copy that cannot be made readable is handed over as it is,
            ;; rather than stopping the asking.
            (condition-case err
                (org-moor-readable copy)
              (error
               (message "org-moor: %s handed over as it is: %s"
                        (file-name-nondirectory copy) (error-message-string err))
               copy))
            (format "moor:%s  %s" (plist-get record :id) (or (plist-get record :title) "")))))))

(defun org-moor-batch-command (sources)
  "Return the command line that takes a source into SOURCES, as this Emacs runs it.
FILE-OR-URL and TITLE stand for what is taken and its title; see
`org-moor-batch-take'."
  (let ((dir-of (lambda (library)
                  (shell-quote-argument
                   (directory-file-name (file-name-directory (locate-library library))) t))))
    (format "%s --batch -L %s -L %s -l org-moor -f org-moor-batch-take %s FILE-OR-URL TITLE"
            (shell-quote-argument (expand-file-name invocation-name invocation-directory) t)
            (funcall dir-of "org-moor") (funcall dir-of "org-dock")
            (shell-quote-argument (directory-file-name (expand-file-name sources)) t))))

(defun org-moor-request-text (request)
  "Tell an agent how to cite a source, and how to take one in.
REQUEST is the plist `org-dock-agent-function' gets.  Into the sources
directory of the file the copy's heading is from."
  (concat
   "## Sources\n\n"
   "When you write something a source says, put a link to its place right\n"
   "after it:\n\n"
   "- page 12 of a PDF:            [[moor:ID::12][TITLE p. 12]]\n"
   "- line 123 of any other file:  [[moor:ID::123][TITLE l. 123]]\n\n"
   "ID and TITLE are those of the source, as named under Linked files, and a\n"
   "line is a line of its file there.\n\n"
   "A file or a web page that is not a source yet is taken in by this\n"
   "command, which prints a link to it, [[moor:ID][TITLE]]:\n\n"
   "    " (org-moor-batch-command (org-moor-sources (plist-get request :origin))) "\n"))

(add-hook 'org-dock-link-file-functions #'org-moor-link-file)
(add-hook 'org-dock-request-functions #'org-moor-request-text 10)

;; The same, and the `moor:' link, from the package's autoloads, so that they
;; hold before anything of org-moor has been used: a link to a source can be
;; followed, and a heading with sources taken out and handed to an agent, in
;; an Emacs that has only just started.
;;;###autoload (autoload 'org-moor-link-follow "org-moor")
;;;###autoload (autoload 'org-moor-link-complete "org-moor")
;;;###autoload (autoload 'org-moor-link-description "org-moor")
;;;###autoload (autoload 'org-moor-link-file "org-moor")
;;;###autoload (autoload 'org-moor-request-text "org-moor")
;;;###autoload (with-eval-after-load 'ol (org-link-set-parameters "moor" :follow #'org-moor-link-follow :complete #'org-moor-link-complete :insert-description #'org-moor-link-description))
;;;###autoload (with-eval-after-load 'org-dock (add-hook 'org-dock-link-file-functions #'org-moor-link-file) (add-hook 'org-dock-request-functions #'org-moor-request-text 10))

(provide 'org-moor-core)
;;; org-moor-core.el ends here
