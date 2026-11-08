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
;; characters, and the rest below them.  In it are the source's *replica*, if
;; it has one, and its *record*, `moor.org', saying what it is: a title, and
;; where it came from, how and when.  The replica is the source as anyone
;; reading it sees it -- the page, the document -- and is what you open to
;; check what is said of it.  Beside it there may be the source made
;; readable by a machine -- what an agent reads -- fetched apart, from an
;; API, or made from the replica.  Nothing in a sources directory asks to
;; be filed, and nothing in an Org file has to be a source's: what you write
;; points to a source with a `moor:' link, from wherever you write it, as
;; often as you like.
;;
;; A *place* is the words quoted from a source, written as `moor:ID::WORDS':
;; found again in any form of it.
;;
;; org-moor fetches nothing.  What comes from the web is what your browser
;; saved, when you had it open it.
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
  '((:origin-url . "ORIGIN_URL")
    (:replica-file . "REPLICA_FILE")
    (:replica-via . "REPLICA_VIA")
    (:replica-taken . "REPLICA_TAKEN")
    (:replica-requested . "REPLICA_REQUESTED")
    (:machine-readable-url . "MACHINE_READABLE_URL")
    (:machine-readable-file . "MACHINE_READABLE_FILE")
    (:machine-readable-via . "MACHINE_READABLE_VIA")
    (:machine-readable-taken . "MACHINE_READABLE_TAKEN")
    (:machine-readable-made-from . "MACHINE_READABLE_MADE_FROM")
    (:machine-readable-made-by . "MACHINE_READABLE_MADE_BY")
    (:machine-readable-made . "MACHINE_READABLE_MADE"))
  "The properties of a record, by the keys a RECORD plist has them under.
Origin URL: where the source is, as anyone reading it sees it -- a page,
a document in its library, a file on disk as a file: URL -- kept after it
changes or is gone.  Replica: the source as it is seen there, kept: its
file's name in the source's directory, how it arrived (web, file, mail,
chat, clipboard, or the tool an agent took it by), and when, an inactive
timestamp; while there is none, when it was asked for.  Machine readable:
the source as a machine reads it, when that is not the replica: its file
there, and either the URL it was fetched from, how and when, or the file
it was made from, by what and when.  A record without one has its replica
read as it is.")

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
          (org-moor--record-paths record))))))

(defun org-moor--record-paths (record)
  "Set RECORD's :replica and :machine-readable, the paths of its files there."
  (pcase-dolist (`(,file . ,path) '((:replica-file . :replica)
                                    (:machine-readable-file . :machine-readable)))
    (let ((name (and (plist-get record file)
                     (expand-file-name (plist-get record file) (plist-get record :dir)))))
      (setq record (plist-put record path (and name (file-exists-p name) name)))))
  record)

(defun org-moor-record (id &optional sources)
  "Return source ID as a plist, read from its record, or nil.
In SOURCES, by default this buffer's sources directory.  Keys are :id,
:title, :dir, its directory, those of `org-moor--record-keys', and
:replica and :machine-readable, the paths of those files while they are
there."
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
  (format "%-29s %s" (concat ":" name ":") value))

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

(defun org-moor-now ()
  "Return now as an inactive timestamp, as a record has a time."
  (format-time-string (org-time-stamp-format t t)))

(defun org-moor-path-url (path)
  "Return the file: URL of PATH, or PATH itself when it is a URL already.
/a/b is file:///a/b, C:/a is file:///C:/a, and //host/share/a -- a
shared folder, \\\\host\\share\\a on Windows -- file://host/share/a.
Written as it reads, not percent-encoded, but for % itself; see
`org-moor-file-url-path', which reads it back."
  (if (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]+:[^\\\\]" path)
      path
    (let ((path (replace-regexp-in-string
                 "%" "%25" (subst-char-in-string ?\\ ?/ (expand-file-name path)))))
      (cond ((string-prefix-p "//" path) (concat "file:" path))
            ((string-match-p "\\`[A-Za-z]:" path) (concat "file:///" path))
            (t (concat "file://" path))))))

(defun org-moor-url-path (url)
  "Return the path a file: URL names, or nil for any other URL."
  (and url (string-prefix-p "file:" url) (org-moor-file-url-path url)))

(defun org-moor-new-source (title origin &optional sources)
  "Make a source titled TITLE from ORIGIN, and return its record.
ORIGIN, a URL or a path, is its origin URL; nil when it has none.  In
SOURCES, by default this buffer's sources directory.  Its directory and
record are made, its replica asked for now; the replica is for the caller
to put there, see `org-moor-record-replica'."
  (let* ((id (org-id-uuid))
         (dir (org-moor-source-directory id sources))
         (record (list :id id :dir dir :title title
                       :origin-url (and origin (org-moor-path-url origin))
                       :replica-requested (org-moor-now))))
    (make-directory dir t)
    (org-moor--write-text (expand-file-name org-moor-record-file dir)
                          (org-moor--record-text record))
    record))

(defun org-moor-record-replica (record name via)
  "Have the file NAME in RECORD's directory be its replica, arrived VIA.
Taken now; it is asked for no longer.  What it is made readable as, by
`org-moor-machine-readable-converters', is made again.  Return RECORD."
  (let* ((record (org-moor-record-set record :replica-file name))
         (record (org-moor-record-set record :replica-via (format "%s" via)))
         (record (org-moor-record-set record :replica-taken (org-moor-now)))
         (record (org-moor-record-set record :replica-requested nil)))
    (org-moor-make-machine-readable record)))

(defun org-moor-record-set (record key value)
  "Set KEY of RECORD to VALUE in its record file, and return RECORD so.
KEY is :title or a key of `org-moor--record-keys'.  Only that line is
written, so that anything else written in the record stays; a VALUE of
nil takes the line out."
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
          (cond
           ((not value)
            (when (re-search-forward (concat "^[ \t]*:" name ":.*\n?") nil t)
              (replace-match "" t t)))
           ((re-search-forward (concat "^[ \t]*:" name ":.*$") nil t)
            (replace-match (org-moor--property-line name value) t t))
           (t
            (if (re-search-forward "^[ \t]*:END:" nil t)
                (progn (beginning-of-line)
                       (insert (org-moor--property-line name value) "\n"))
              (goto-char (point-min))
              (insert ":PROPERTIES:\n"
                      (org-moor--property-line name value) "\n"
                      ":END:\n"))))))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region nil nil file nil 'silent))))
  (org-moor--record-paths (plist-put record key value)))

;;;; Places

(defun org-moor-link-parse (path)
  "Split the `moor:' link PATH into (ID . PLACE).
PLACE is the words quoted there, to be found again, or nil."
  (if (string-match "\\`\\([^:]+\\)\\(?:::\\(.+\\)\\)?\\'" path)
      (cons (match-string 1 path) (match-string 2 path))
    (error "org-moor: not a moor link: %s" path)))

(defun org-moor-extension (file)
  "Return FILE's extension, lower case, or \"\" when it has none."
  (downcase (or (file-name-extension file) "")))

(defcustom org-moor-text-extensions
  '("txt" "md" "markdown" "org" "csv" "tsv" "json" "xml" "yaml" "yml" "rst" "tex" "log")
  "Extensions of files Emacs shows as text."
  :type '(repeat string)
  :group 'org-moor)

(defun org-moor-text-file-p (file)
  "Return non-nil when FILE is shown as text; see `org-moor-text-extensions'."
  (member (org-moor-extension file) org-moor-text-extensions))

(defun org-moor-pdf-p (file)
  "Return non-nil when FILE is a PDF."
  (equal (org-moor-extension file) "pdf"))

(defun org-moor-emacs-opens-p (file)
  "Return non-nil when Org opens FILE, as a `file:' link, in Emacs.
By the first entry of `org-file-apps', then the system's own list, that
names FILE's form, as `org-open-file' reads them; by `auto-mode-alist'
when none does and `org-file-apps' has an `auto-mode' entry.  A PDF is
read in Emacs, by pdf-tools or DocView, when this says so."
  (require 'org)
  (let* ((apps (append org-file-apps
                       (pcase system-type
                         ('darwin org-file-apps-macos)
                         ('windows-nt org-file-apps-windowsnt)
                         (_ org-file-apps-gnu))))
         (name (downcase file))
         (entry (seq-find
                 (lambda (app)
                   (and (stringp (car app))
                        (string-match-p (if (string-match-p "\\W" (car app))
                                            (car app)
                                          (concat "\\." (car app) "\\'"))
                                        name)))
                 apps)))
    (if entry
        (eq (cdr entry) 'emacs)
      (and (assq 'auto-mode apps)
           (assoc-default name auto-mode-alist #'string-match)
           t))))

(defun org-moor-page-p (file)
  "Return non-nil when FILE is a page, which a browser shows."
  (member (org-moor-extension file) '("html" "htm")))

(defun org-moor-file-url-path (url)
  "Return the file a file: URL names: file:///C:/a%20b.pdf is C:/a b.pdf.
A host is a shared folder's: file://host/share/a is //host/share/a."
  (let* ((host (and (string-match "\\`file://\\([^/]+\\)" url)
                    (not (equal (match-string 1 url) "localhost"))
                    (match-string 1 url)))
         (path (decode-coding-string
                (url-unhex-string (replace-regexp-in-string
                                   "\\`file:\\(//[^/]*\\)?" "" url))
                'utf-8)))
    (cond (host (concat "//" host path))
          ((string-match-p "\\`/[A-Za-z]:" path) (substring path 1))
          (t path))))

(defun org-moor-shown-file ()
  "Return the file this buffer shows, or nil."
  buffer-file-name)

(defun org-moor-text-regexp (text)
  "Return a regexp finding TEXT however its whitespace is broken."
  (mapconcat #'regexp-quote (split-string text "[ \t\n]+" t) "[ \t\n]*"))

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

;;;; Machine readable

(defcustom org-moor-machine-readable-converters
  '(("docx" . org-moor-pandoc)
    ("pptx" . org-moor-pandoc)
    ("odt" . org-moor-pandoc)
    ("epub" . org-moor-pandoc)
    ("html" . org-moor-pandoc)
    ("htm" . org-moor-pandoc))
  "How a replica is made readable by a machine, by extension.
Each function is called with the replica's path and returns the path of
the file it made beside it; it is recorded as made by the function's name
without org-moor-.  A replica whose extension is not here is read as it
is: text, Markdown and PDF need nothing."
  :type '(alist :key-type string :value-type function)
  :group 'org-moor)

(defun org-moor-argument-coding ()
  "Return the coding a program is given its arguments in.
On Windows that is the system's code page, cp932 in Japanese, which is
not what Emacs encodes them by unless told: a name that is not ASCII
would arrive as another.  A character the code page lacks cannot be
given at all.  Elsewhere, UTF-8."
  (or (and (eq system-type 'windows-nt) locale-coding-system) 'utf-8))

(defun org-moor--process-string (program &rest args)
  "Return what PROGRAM prints with ARGS, or signal with what it complained."
  (unless (executable-find program)
    (user-error "org-moor: %s is not installed" program))
  (with-temp-buffer
    (let ((status (let ((coding-system-for-read 'utf-8)
                        (coding-system-for-write (org-moor-argument-coding)))
                    (apply #'call-process program nil t nil args))))
      (unless (eq status 0)
        (user-error "org-moor: %s failed: %s" program
                    (string-trim (buffer-string))))
      (buffer-string))))

(defun org-moor-pandoc (file)
  "Return FILE turned into Markdown by pandoc, beside it."
  (let ((md (concat file ".md")))
    (org-moor--process-string "pandoc" "--to=gfm" "--wrap=none" "-o" md file)
    md))

(defconst org-moor--made-keys
  '(:machine-readable-made-from :machine-readable-made-by
    :machine-readable-made :machine-readable-file)
  "The keys of a record saying what its replica was made readable as.")

(defun org-moor-make-machine-readable (record)
  "Make RECORD's replica readable by a machine, and return RECORD so.
By `org-moor-machine-readable-converters', recorded as made from it.
What was made from an earlier replica goes.  A record whose machine
readable file was fetched -- it has a URL -- is left as it is, as is one
whose replica cannot be made readable: an error is said, not signalled."
  (if (plist-get record :machine-readable-url)
      record
    (let ((old (and (plist-get record :machine-readable-made-from)
                    (plist-get record :machine-readable))))
      (when old (delete-file old))
      (dolist (key org-moor--made-keys)
        (when (plist-get record key)
          (setq record (org-moor-record-set record key nil))))
      (let* ((replica (plist-get record :replica))
             (fn (and replica (cdr (assoc (org-moor-extension replica)
                                          org-moor-machine-readable-converters))))
             (made (and fn (condition-case err
                               (funcall fn replica)
                             (error
                              (message "%s" (propertize
                                             (format "org-moor: %s not made readable: %s"
                                                     (file-name-nondirectory replica)
                                                     (error-message-string err))
                                             'face 'warning))
                              nil)))))
        (if (not made)
            record
          (let* ((record (org-moor-record-set record :machine-readable-made-from
                                              (plist-get record :replica-file)))
                 (record (org-moor-record-set record :machine-readable-made-by
                                              (if (symbolp fn)
                                                  (string-remove-prefix "org-moor-" (symbol-name fn))
                                                "a function")))
                 (record (org-moor-record-set record :machine-readable-file
                                              (file-name-nondirectory made))))
            (org-moor-record-set record :machine-readable-made (org-moor-now))))))))

(defun org-moor-file-for-machine (record)
  "Return the file a machine reads for RECORD: its machine readable file, else
its replica; nil when it has neither."
  (or (plist-get record :machine-readable) (plist-get record :replica)))

;;;; Finding words

(defun org-moor-normal-text (text)
  "Return TEXT as words are compared: no whitespace, width folded.
A full-width letter, digit or sign is its half-width one, as NFKC has it,
so that words copied from one form of a source are found in another."
  (replace-regexp-in-string
   "[ \t\n\r\f\u3000]+" ""
   (if (fboundp 'ucs-normalize-NFKC-string)
       (ucs-normalize-NFKC-string text)
     text)))

(defun org-moor-pdf-pages (file)
  "Return the text of each page of the PDF FILE, by pdftotext, as a list."
  (split-string (org-moor--process-string "pdftotext" "-layout" "-enc" "UTF-8" file "-")
                "\f"))

(defun org-moor-pdf-page-of (file words)
  "Return the page of the PDF FILE that WORDS are on, counted from 1, or nil."
  (let ((words (org-moor-normal-text words))
        (page 0))
    (seq-some (lambda (text)
                (setq page (1+ page))
                (and (string-search words (org-moor-normal-text text)) page))
              (org-moor-pdf-pages file))))

;;;; Words in a replica

(defun org-moor-replica-text (record)
  "Return the text of RECORD's replica, as words are looked for in it, or nil.
A PDF's by pdftotext; a text as it is; anything else by pandoc, as plain
text -- a page, a document.  Nil without a replica."
  (when-let* ((replica (plist-get record :replica)))
    (cond ((org-moor-pdf-p replica) (string-join (org-moor-pdf-pages replica) "\n"))
          ((org-moor-text-file-p replica)
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8))
               (insert-file-contents replica))
             (buffer-string)))
          (t (org-moor--process-string "pandoc" "--to=plain" "--wrap=none" replica)))))

(defvar org-moor--replica-texts (make-hash-table :test #'equal)
  "The text of replicas, as words are compared, by file and when it changed.
So a replica is read once while it does not change, however many quotes
are looked for in it.")

(defun org-moor--normal-replica-text (record)
  "Return the text of RECORD's replica by `org-moor-normal-text', or nil.
Read once while the replica does not change."
  (when-let* ((replica (plist-get record :replica)))
    (let ((key (cons replica (file-attribute-modification-time (file-attributes replica)))))
      (or (gethash key org-moor--replica-texts)
          (when-let* ((text (org-moor-replica-text record)))
            (puthash key (org-moor-normal-text text) org-moor--replica-texts))))))

(defun org-moor-words-in-replica (record words)
  "Return whether WORDS are in RECORD's replica.
`found', `not-found', or `no-replica' when it has none yet.
Compared by `org-moor-normal-text', so whitespace and width do not count;
words left out, marked … or ..., split WORDS into parts found in turn."
  (let ((text (org-moor--normal-replica-text record)))
    (if (not text)
        'no-replica
      (let ((from 0)
            (found t))
        (dolist (part (split-string words "…\\|\\.\\.\\." t "[ \t\n]+"))
          (let ((at (and found (string-search (org-moor-normal-text part) text from))))
            (if at
                (setq from (+ at (length (org-moor-normal-text part))))
              (setq found nil))))
        (if found 'found 'not-found)))))

(defun org-moor--batch-end ()
  "Refuse whatever a batch command has left on the command line.
Emacs goes on to run what is left once the command returns -- an --eval,
a -l of any file -- so a command that ignored it would let a caller
allowed to run it run anything.  Called once the command has read its
arguments, before it does anything."
  (when command-line-args-left
    (error "org-moor: unexpected arguments: %s"
           (string-join command-line-args-left " "))))

(defun org-moor-batch-find ()
  "Say whether words are in a source's replica, for an agent checking a quote.

  emacs --batch -L DIR -L ORG-DOCK-DIR -l org-moor \\
        -f org-moor-batch-find SOURCES moor:ID WORDS

Prints `Found', `Not found', or `No replica yet'; see
`org-moor-words-in-replica'."
  (let* ((sources (expand-file-name (or (pop command-line-args-left)
                                        (error "org-moor: no sources directory"))))
         (link (or (pop command-line-args-left) (error "org-moor: no moor:ID")))
         (words (or (pop command-line-args-left) (error "org-moor: no words")))
         (_ (org-moor--batch-end))
         (id (car (org-moor-link-parse (string-remove-prefix "moor:" link))))
         (record (or (org-moor-record id sources)
                     (error "org-moor: no source %s in %s" id sources))))
    (let ((coding-system-for-write 'utf-8))
      (princ (pcase (org-moor-words-in-replica record words)
               ('found "Found\n")
               ('not-found "Not found\n")
               (_ "No replica yet\n"))))))

;;;; Opening

(defun org-moor--say-to-find (words)
  "Put WORDS on the clipboard, to be found where they cannot be shown, and say so."
  (kill-new words)
  (message "org-moor: copied the words to find: %s" words))

(defun org-moor-page-url (file &optional words)
  "Return the file: URL of the page FILE, at WORDS when given.
At them by a text fragment, #:~:text=, which a browser scrolls to and
marks -- where it does."
  (concat (url-encode-url (org-moor-path-url file))
          (if words (concat "#:~:text=" (url-hexify-string words)) "")))

(defun org-moor-open-replica-file (file &optional place then how)
  "Open FILE, a replica, as it is seen, at PLACE when there is one.
PLACE is the words quoted from it.  A text is shown in Emacs at the line
PLACE is on, and a PDF at the page, where Org opens a PDF in Emacs
\(`org-moor-emacs-opens-p'); a page in the browser, at PLACE where the
browser goes to words; anything else, a PDF Org opens outside among them,
in its program, by `org-open-file' as a `file:' link is.  Where PLACE is not shown, it is
put on the clipboard to be found.  HOW is the prefix for that, as for
following a link: nil for what `org-file-apps' says, (4) for Emacs,
\(16) for the program the system opens it with -- a PDF or a text too.
THEN, when given, is called in the buffer that shows FILE, once it is
shown; not when FILE is opened outside Emacs."
  (cond
   ((and (not (equal how '(16)))
         (org-moor-pdf-p file)
         (or (equal how '(4)) (org-moor-emacs-opens-p file)))
    (let ((page (and place (org-moor-pdf-page-of file place))))
      (find-file file)
      (when page
        (cond ((and (derived-mode-p 'pdf-view-mode) (fboundp 'pdf-view-goto-page))
               (pdf-view-goto-page page))
              ((and (derived-mode-p 'doc-view-mode) (fboundp 'doc-view-goto-page))
               (doc-view-goto-page page))))
      (when (and place (not page)) (message "org-moor: not found: %s" place))
      (when then (funcall then))))
   ((and (not (equal how '(16)))
         (or (org-moor-text-file-p file) (equal how '(4))))
    (find-file file)
    (when place (org-moor-goto-text place))
    (when then (funcall then)))
   ((org-moor-page-p file)
    (browse-url (org-moor-page-url file place))
    (when place (org-moor--say-to-find place)))
   (t (org-open-file file how)
      (when place (org-moor--say-to-find place))
      (when (and then (equal (org-moor-shown-file) (expand-file-name file)))
        (funcall then)))))

(defun org-moor-file-to-read (replica)
  "Return REPLICA when Emacs shows it as it is seen, to read beside its heading.
A text, or a PDF Org opens in Emacs (`org-moor-emacs-opens-p'); nil for
anything else, which its own program shows."
  (and (or (org-moor-text-file-p replica)
           (and (org-moor-pdf-p replica) (org-moor-emacs-opens-p replica)))
       replica))

(defun org-moor-open-to-read (file then)
  "Show FILE, from `org-moor-file-to-read', and call THEN in its buffer."
  (find-file file)
  (funcall then))

(defcustom org-moor-office-url-schemes
  '(("xlsx" . "ms-excel") ("xlsm" . "ms-excel") ("xlsb" . "ms-excel")
    ("xls" . "ms-excel") ("docx" . "ms-word") ("docm" . "ms-word")
    ("doc" . "ms-word") ("pptx" . "ms-powerpoint") ("pptm" . "ms-powerpoint")
    ("ppt" . "ms-powerpoint"))
  "Office URI schemes, by extension, an http(s) origin is opened with.
A document on a web server -- in a document library, say -- is opened
from there in its Office application, as ms-excel:ofe|u|URL, to edit
and save back, not downloaded by the browser.  nil hands every URL to
the browser."
  :type '(alist :key-type (string :tag "Extension")
                :value-type (string :tag "Scheme"))
  :group 'org-moor)

(defun org-moor--office-url (url)
  "Return URL as an Office URI, by `org-moor-office-url-schemes', or nil."
  (when (string-match "\\`https?://[^?#]*" url)
    (let ((scheme (cdr (assoc (org-moor-extension
                               (url-unhex-string (match-string 0 url)))
                              org-moor-office-url-schemes))))
      (and scheme (format "%s:ofe|u|%s" scheme url)))))

(declare-function w32-shell-execute "w32fns.c")

(defun org-moor--open-externally (uri)
  "Open URI as the system does, whatever browser `browse-url' would use."
  (cond ((eq system-type 'windows-nt) (w32-shell-execute "open" uri))
        ((eq system-type 'darwin) (call-process "open" nil 0 nil uri))
        (t (call-process "xdg-open" nil 0 nil uri))))

(defun org-moor--open-origin (origin &optional how)
  "Open ORIGIN, a URL: in the browser, or a file: URL's file where it still is.
An Office document's URL in its application instead; see
`org-moor-office-url-schemes'.  HOW is as `org-moor-open-replica-file'
has it, for a file."
  (cond ((org-moor-url-path origin)
         (let ((path (org-moor-url-path origin)))
           (if (file-exists-p path)
               (org-open-file path how)
             (user-error "org-moor: the origin is gone: %s" path))))
        ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]+:[^\\]" origin)
         ;; Percent-encoded, for a character outside ASCII can be read
         ;; in another coding on its way to the browser.
         (let* ((url (url-encode-url origin))
                (office (org-moor--office-url url)))
           (if office
               (org-moor--open-externally office)
             (browse-url url))))
        ((file-exists-p origin) (org-open-file origin how))
        (t (user-error "org-moor: the origin is gone: %s" origin))))

(defvar-local org-moor--read-for nil
  "In a buffer showing a replica, a marker at the heading it was opened from.
Where what is noted from it is written; see `org-moor-insert-place'.")

(defun org-moor--heading-marker ()
  "Return a marker at the heading point is in, or nil."
  (and (derived-mode-p 'org-mode)
       (save-excursion
         (unless (org-before-first-heading-p)
           (org-back-to-heading t)
           (point-marker)))))

(defun org-moor-open-record (record &optional place from how)
  "Open the replica of RECORD at PLACE, or else its origin, saying so.
FROM, a marker at a heading, is kept in the replica's buffer as the heading
it is read for.  HOW is as `org-moor-open-replica-file' has it."
  (cond
   ((plist-get record :replica)
    (org-moor-open-replica-file (plist-get record :replica) place
                        (and from (lambda () (setq org-moor--read-for from)))
                        how))
   ((plist-get record :origin-url)
    (org-moor--open-origin (plist-get record :origin-url) how)
    (when place (org-moor--say-to-find place))
    ;; Said last, so that nothing said in opening hides it: what is open
    ;; now is the source where it is, which may have changed since.
    (message "%s" (propertize
                   (format "org-moor: no replica of %s yet; opened its origin: %s"
                           (plist-get record :title) (plist-get record :origin-url))
                   'face 'warning)))
   (t (user-error "org-moor: %s has neither replica nor origin" (plist-get record :id)))))

(defun org-moor-link-follow (path arg)
  "Open the place PATH: the replica at the words quoted, or else the origin.
ARG is the prefix `org-open-at-point' was given, as for a `file:' link:
none opens the replica as `org-file-apps' says, \\[universal-argument] in Emacs,
two in the program the system opens it with."
  (pcase-let* ((`(,id . ,place) (org-moor-link-parse path))
               (record (or (org-moor-find-record id)
                           (user-error "org-moor: no source %s in %s" id
                                       (abbreviate-file-name (org-moor-sources))))))
    (org-moor-open-record record place (org-moor--heading-marker) arg)))

;;;; Links

(defcustom org-moor-text-place-length 40
  "Characters of the words quoted a link's place is kept by, at most.
Enough to find them again; the whole of what was selected is quoted
under the link."
  :type 'integer
  :group 'org-moor)

(defun org-moor-text-place (text)
  "Return the start of TEXT, the place a link finds it by.
On one line, without the brackets a link cannot hold, and not longer
than `org-moor-text-place-length'; nil when nothing is left."
  (let* ((words (split-string (replace-regexp-in-string "[][]" " " text) "[ \t\n]+" t))
         (place (string-join words " ")))
    (unless (string-empty-p place)
      (if (> (length place) org-moor-text-place-length)
          (string-trim-right (substring place 0 org-moor-text-place-length))
        place))))

(defvar org-moor-place-by-quote-functions nil
  "Functions saying whether links in an Org file find their place by a quote.
Each is called with the file's name and returns non-nil for a file whose
links to a source carry no place: the quote under a link,
#+begin_quote, is the place, written once, and the link is named where in
the source it is -- an article, a page, a section -- as the source itself
numbers it.  A package whose files quote what they stand on adds one
here.")

(defun org-moor-place-by-quote-p (&optional file)
  "Non-nil when links in FILE find their place by the quote under them.
FILE is by default this buffer's Org file; see
`org-moor-place-by-quote-functions'."
  (let ((file (or file (org-moor-org-file))))
    (and file (run-hook-with-args-until-success 'org-moor-place-by-quote-functions file))))

(defun org-moor-link-name (record &optional place)
  "Return the name of a link to RECORD, or to PLACE in it: its title.
PLACE, the words quoted, follows it in quotation marks."
  (let ((title (or (plist-get record :title) "source")))
    (if place (format "%s \u201c%s\u201d" title place) title)))

(defun org-moor-link (record &optional place)
  "Return a link to RECORD, or to PLACE in it, named by the source's title.
PLACE is the words quoted, by `org-moor-text-place'.  The name is the
title, so that which source it is can be read where the link is: no
heading says it."
  (let ((id (plist-get record :id)))
    (org-link-make-string
     (if place (format "moor:%s::%s" id place) (format "moor:%s" id))
     (org-moor-link-name record place))))

(defun org-moor-record-time (record)
  "Return when RECORD's replica was taken, or else asked for, or \"\"."
  (or (plist-get record :replica-taken) (plist-get record :replica-requested) ""))

(defun org-moor-read-source (&optional prompt)
  "Read a source by its title, newest first, and return its record.
From this buffer's sources directory, and the default one."
  (let* ((records (seq-uniq
                   (append (org-moor-sources-in (org-moor-sources))
                           (org-moor-sources-in org-moor-sources-directory))
                   (lambda (a b) (equal (plist-get a :id) (plist-get b :id)))))
         (records (sort records (lambda (a b)
                                  (string> (org-moor-record-time a)
                                           (org-moor-record-time b)))))
         (table (mapcar (lambda (r)
                          (cons (format "%s  %s %s  %s"
                                        (or (plist-get r :title) "")
                                        (org-moor-record-time r)
                                        (or (plist-get r :replica-via) "awaited")
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
      (org-moor-link-name record place))))

;;;; What a link has

(defun org-moor-link-summary (path)
  "Return a line saying what the source of the `moor:' link PATH has.
Whether it has a replica -- the one thing a link does not show, and the
one that matters, since without it following the link opens the origin,
as it is now -- and where it came from, how and when; and what a machine
reads of it, when that is not the replica."
  (pcase-let* ((`(,id . ,_place) (org-moor-link-parse path))
               (record (org-moor-find-record id)))
    (if (not record)
        (propertize (format "moor: no source %s in %s" id
                            (abbreviate-file-name (org-moor-sources)))
                    'face 'error)
      (let* ((file (plist-get record :replica-file))
             (replica (plist-get record :replica))
             (origin (plist-get record :origin-url))
             (when (string-trim (org-moor-record-time record) "\\[" "\\]"))
             (what (cond
                    (replica (format "replica %s (%s)%s %s" file
                                     (file-size-human-readable
                                      (file-attribute-size (file-attributes replica)))
                                     (if (plist-get record :replica-via)
                                         (concat " " (plist-get record :replica-via))
                                       "")
                                     when))
                    (file (propertize (format "replica %s is gone" file) 'face 'warning))
                    (t (propertize (format "no replica, awaited since %s" when)
                                   'face 'warning))))
             (machine (plist-get record :machine-readable-file)))
        (string-join (delq nil (list (concat "moor: " what)
                                     (and machine (format "machine readable %s" machine))
                                     (or origin (propertize "no origin" 'face 'warning))))
                     "  ")))))

(defun org-moor--link-path-at (pos)
  "Return the path of the `moor:' link at POS, without its type, or nil.
In a property too, where Org does not see a link as one."
  (save-excursion
    (goto-char pos)
    (when (org-in-regexp org-link-bracket-re 1)
      (let ((link (org-link-unescape (match-string-no-properties 1))))
        (and (string-prefix-p "moor:" link) (substring link 5))))))

(defun org-moor-link-help-echo (window object pos)
  "Return what the source of the `moor:' link at POS has, for its tooltip.
WINDOW, OBJECT and POS are as `help-echo' gives them."
  (with-current-buffer (if (bufferp object) object (window-buffer window))
    (when-let* ((path (org-moor--link-path-at pos)))
      (ignore-errors (org-moor-link-summary path)))))

(defun org-moor-eldoc (&rest _)
  "Return what the source of the `moor:' link at point has, for ElDoc."
  (when-let* ((path (org-moor--link-path-at (point))))
    (ignore-errors (org-moor-link-summary path))))

(defun org-moor-eldoc-setup ()
  "Say in the echo area what the source of a `moor:' link at point has.
For `org-mode-hook'."
  (add-hook 'eldoc-documentation-functions #'org-moor-eldoc nil t)
  (eldoc-mode 1))

(org-link-set-parameters "moor"
                         :follow #'org-moor-link-follow
                         :complete #'org-moor-link-complete
                         :insert-description #'org-moor-link-description
                         :help-echo #'org-moor-link-help-echo)

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
  (when-let* ((path (org-moor--link-path-at (point))))
    (car (org-moor-link-parse path))))

;;;; Handed to an agent

(defun org-moor-link-file (type path)
  "Return the file a `moor:' link of PATH points to, for an agent to read.
TYPE is the link's type.  As (FILE . NAME): what a machine reads of the
source -- see `org-moor-file-for-machine' -- and the source named by its
id and its title, which a place is cited by.  Nil when it has nothing to
read yet."
  (when (equal type "moor")
    (let* ((record (ignore-errors (org-moor-find-record (car (org-moor-link-parse path)))))
           (file (and record (org-moor-file-for-machine record))))
      (and file
           (cons file
                 (format "moor:%s  %s" (plist-get record :id) (or (plist-get record :title) "")))))))

(defvar server-process)
(defvar server-name)
(defvar server-use-tcp)
(defvar server-auth-dir)
(defvar server-socket-dir)

(defun org-moor--server-file ()
  "Return this Emacs's server, its socket or its authentication file, or nil.
Nil when the server is not running."
  (and (boundp 'server-process) (process-live-p server-process)
       (expand-file-name server-name (if server-use-tcp server-auth-dir server-socket-dir))))

(defun org-moor--batch-program (function)
  "Return the command line running FUNCTION in a batch Emacs with org-moor."
  (let ((dir-of (lambda (library)
                  (shell-quote-argument
                   (directory-file-name (file-name-directory (locate-library library))) t))))
    (format "%s --batch -L %s -L %s -l org-moor -f %s"
            (shell-quote-argument (expand-file-name invocation-name invocation-directory) t)
            (funcall dir-of "org-moor") (funcall dir-of "org-dock")
            function)))

(defun org-moor-batch-command (sources)
  "Return the command line that takes a source into SOURCES, as this Emacs runs it.
REPLICA and TITLE stand for what is taken and its title; see
`org-moor-batch-take'.  While this Emacs's server is running, the command
has it take the source."
  (let ((server (org-moor--server-file)))
    (format "%s %s%s REPLICA TITLE"
            (org-moor--batch-program "org-moor-batch-take")
            (if server (concat "--server=" (shell-quote-argument server t) " ") "")
            (shell-quote-argument (directory-file-name (expand-file-name sources)) t))))

(defun org-moor-batch-find-command (sources)
  "Return the command line that looks for words in a replica in SOURCES.
See `org-moor-batch-find'."
  (format "%s %s moor:ID WORDS"
          (org-moor--batch-program "org-moor-batch-find")
          (shell-quote-argument (directory-file-name (expand-file-name sources)) t)))

(defun org-moor-request-text (request)
  "Tell an agent how to cite a source, take one in, and check a quote.
REQUEST is the plist `org-dock-agent-function' gets.  Into the sources
directory of the file the copy's heading is from.  The links carry no
place where `org-moor-place-by-quote-p' says so of that file."
  (let* ((origin (plist-get request :origin))
         (sources (org-moor-sources origin))
         (by-quote (and origin (org-moor-place-by-quote-p origin))))
    (concat
     "## Sources\n\n"
     "When you write something a source says, quote its words, as they are,\n"
     "and link to the source right after what you wrote:\n\n"
     (if by-quote
         (concat
          "    [[moor:ID][WHERE]]\n"
          "    #+begin_quote\n    WORDS\n    #+end_quote\n\n"
          "WHERE is where in the source the words are, as the source itself numbers\n"
          "it -- an article, a page, a section; the source's title when it numbers\n"
          "nothing.  The quote is the place: the link has none.\n\n")
       (concat
        "    [[moor:ID::START][TITLE “START”]]\n"
        "    #+begin_quote\n    WORDS\n    #+end_quote\n\n"
        "START is the first words of the quote, up to 40 characters, on one line,\n"
        "without brackets.\n\n"))
     "ID and TITLE are those of the source, as named under Linked files.  Read\n"
     "the file given there.  Copy the words as they are: change none, and mark\n"
     "words left out with ….  No line or page number: a person checks the\n"
     "words in the source as it is seen, where your line numbers are not.\n\n"
     "Check each quote is in the source as it is seen, its replica, by\n\n"
     "    " (org-moor-batch-find-command sources) "\n\n"
     "which prints `Found', or `Not found' -- then read again, and correct the\n"
     "quote -- or `No replica yet'.\n\n"
     "A file or a web page that is not a source yet is taken in by this\n"
     "command, which prints a link to it, [[moor:ID][TITLE]]:\n\n"
     "    " (org-moor-batch-command sources) "\n"
     "        [--origin-url=URL] [--machine-readable=FILE --machine-readable-url=URL]\n\n"
     "REPLICA is a file you have, or a URL.  A file is the source as it is\n"
     "seen; --origin-url is where it is seen, when you fetched it from\n"
     "elsewhere -- a document's page in its library, not the API you fetched\n"
     "it by.  A URL is opened by the user, later, in their browser, which\n"
     "saves the replica: nothing is fetched for you.  --machine-readable is\n"
     "a file you fetched yourself that a machine reads better -- an API's\n"
     "XML for the same page -- and --machine-readable-url where you fetched it.\n"
     "On the lines after the link it prints `Read: PATH', the file to read,\n"
     "and `Replica: PATH', or that the replica is awaited from the user.  A\n"
     "source with nothing to read yet you cite by its link all the same, but\n"
     "write nothing it says, and end by saying which sources are still to be\n"
     "read.  The same command with moor:ID in place of REPLICA says again\n"
     "what the source has.\n")))

(add-hook 'org-dock-link-file-functions #'org-moor-link-file)
(add-hook 'org-dock-request-functions #'org-moor-request-text 10)

;; The same, and the `moor:' link, from the package's autoloads, so that they
;; hold before anything of org-moor has been used: a link to a source can be
;; followed, and a heading with sources taken out and handed to an agent, in
;; an Emacs that has only just started.
;;;###autoload (autoload 'org-moor-link-follow "org-moor")
;;;###autoload (autoload 'org-moor-link-complete "org-moor")
;;;###autoload (autoload 'org-moor-link-description "org-moor")
;;;###autoload (autoload 'org-moor-link-help-echo "org-moor")
;;;###autoload (autoload 'org-moor-eldoc-setup "org-moor")
;;;###autoload (autoload 'org-moor-link-file "org-moor")
;;;###autoload (autoload 'org-moor-request-text "org-moor")
;;;###autoload (with-eval-after-load 'ol (org-link-set-parameters "moor" :follow #'org-moor-link-follow :complete #'org-moor-link-complete :insert-description #'org-moor-link-description :help-echo #'org-moor-link-help-echo))
;;;###autoload (with-eval-after-load 'org-dock (add-hook 'org-dock-link-file-functions #'org-moor-link-file) (add-hook 'org-dock-request-functions #'org-moor-request-text 10))

(provide 'org-moor-core)
;;; org-moor-core.el ends here
