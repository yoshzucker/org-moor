;;; org-moor-fathom.el --- Write what a source means  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A source kept is data; what you write about it is information.  Writing
;; it is the step worth making easy, because it happens again and again,
;; and because what is written has to be right: sentences you would stand
;; behind, in an order that argues.
;;
;; `org-moor-fathom' opens a draft, the way `org-capture' does, beside the
;; heading it is for.  The draft starts from the source's own words -- the
;; region, or the copy turned into text, or the heading's quotes -- so that
;; rewriting costs less typing than writing.  The text a copy is turned into
;; is not kept: it exists to be rewritten.  In a PDF each page begins with
;; a `moor:' link to it, so a sentence that keeps its link keeps its place.
;;
;; C-c C-c writes the draft at the end of the heading's own text, and C-c
;; C-k throws it away.  The draft is a file, in `org-moor-fathom-directory',
;; so that something outside Emacs -- an assistant working on files -- can
;; read and rewrite it too; `org-moor-fathom-assist-function' is the door
;; for that, and the buffer follows the file when it changes.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-take)
(require 'autorevert)
(require 'cl-lib)

(defcustom org-moor-fathom-directory
  (expand-file-name "org-moor/" temporary-file-directory)
  "Directory drafts are kept in while they are being written."
  :type 'directory
  :group 'org-moor)

(defcustom org-moor-fathom-converters
  '(("pdf" . org-moor-fathom-pdftotext)
    ("html" . org-moor-fathom-pandoc)
    ("htm" . org-moor-fathom-pandoc)
    ("docx" . org-moor-fathom-pandoc)
    ("pptx" . org-moor-fathom-pandoc)
    ("odt" . org-moor-fathom-pandoc)
    ("epub" . org-moor-fathom-pandoc)
    ("md" . org-moor-fathom-pandoc))
  "How a copy is turned into draft text, by extension.
Each function is called with the copy's path and the source's id, and
returns Org text."
  :type '(alist :key-type string :value-type function)
  :group 'org-moor)

(defcustom org-moor-fathom-assist-function nil
  "Function asked to help with a draft, or nil.
Called with the draft's file in the draft buffer, by \\[org-moor-fathom-assist].
The file can be rewritten from outside; the buffer follows it."
  :type '(choice (const nil) function)
  :group 'org-moor)

;;;; Turning copies into text

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

(defun org-moor-fathom-pandoc (file _id)
  "Return FILE as Org text, by pandoc.
The drawers pandoc gives each heading for its anchor are dropped: they
name places in the original document, which the draft is not."
  (replace-regexp-in-string
   "^[ \t]*:PROPERTIES:\n[ \t]*:CUSTOM_ID:.*\n[ \t]*:END:\n" ""
   (org-moor--process-string "pandoc" "--to=org" "--wrap=none" file)))

(defun org-moor-fathom-pdftotext (file id)
  "Return FILE's text, each page led by a `moor:' link to it under ID."
  (let ((pages (split-string
                (org-moor--process-string "pdftotext" "-enc" "UTF-8" file "-")
                "\f")))
    (mapconcat #'identity
               (cl-loop for text in pages
                        for n from 1
                        unless (string-blank-p text)
                        collect (format "[[moor:%s::%d][p. %d]]\n%s"
                                        id n n (string-trim text)))
               "\n\n")))

(defun org-moor-fathom-copy-text (source)
  "Return the draft text for SOURCE's copy, or nil if it has none."
  (when-let* ((copy (plist-get source :copy))
              (fn (cdr (assoc (downcase (or (file-name-extension copy) ""))
                              org-moor-fathom-converters))))
    (funcall fn copy (plist-get source :id))))

(defun org-moor-fathom-quotes-text ()
  "Return the contents of the quotes in this heading's own text, or nil."
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let ((end (save-excursion (outline-next-heading) (point)))
         texts)
     (while (re-search-forward
             "^[ \t]*#\\+begin_quote.*\n\\(\\(?:.*\n\\)*?\\)[ \t]*#\\+end_quote"
             end t)
       (push (string-trim-right (match-string 1)) texts))
     (and texts (string-join (nreverse texts) "\n\n")))))

;;;; The draft

(defvar-local org-moor-fathom--target nil
  "Marker of the heading this draft is written for.")

(defvar-local org-moor-fathom--windows nil
  "Window configuration to restore when the draft is done.")

(defvar org-moor-fathom-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-moor-fathom-finish)
    (define-key map (kbd "C-c C-k") #'org-moor-fathom-abort)
    (define-key map (kbd "C-c C-a") #'org-moor-fathom-assist)
    map)
  "Keys of a draft buffer.")

(define-minor-mode org-moor-fathom-mode
  "A draft of what a source means, to be written into its heading."
  :lighter " Fathom"
  (when org-moor-fathom-mode
    (auto-revert-mode 1)
    (setq header-line-format
          (substitute-command-keys
           (format "Draft for %s.  \\[org-moor-fathom-finish] write, \\[org-moor-fathom-abort] discard%s"
                   (org-with-point-at org-moor-fathom--target
                     (org-get-heading t t t t))
                   (if org-moor-fathom-assist-function
                       ", \\[org-moor-fathom-assist] assist" ""))))))

(defun org-moor-fathom--heading ()
  "Return a marker at the heading point is in, or signal."
  (unless (derived-mode-p 'org-mode)
    (user-error "org-moor: not in an Org buffer"))
  (save-excursion
    (when (org-before-first-heading-p)
      (user-error "org-moor: not under a heading"))
    (org-back-to-heading t)
    (point-marker)))

(defun org-moor-fathom-draft-text ()
  "Return the text a draft for the heading at point starts from.
The heading's copy turned into text, else its quotes, else nothing."
  (or (org-moor-fathom-copy-text (org-moor-source))
      (org-moor-fathom-quotes-text)
      ""))

;;;###autoload
(defun org-moor-fathom (&optional beg end)
  "Draft what the heading at point means, starting from its source's words.
With an active region, from the region BEG..END.  On a file in Dired, take
the file first, with `org-moor-take-file', and draft for its new heading."
  (interactive (and (use-region-p) (list (region-beginning) (region-end))))
  (let ((windows (current-window-configuration)))
    (if-let* ((file (org-moor--file-at-point)))
        (org-moor-fathom--open (org-moor-take-file file t) nil windows)
      (org-moor-fathom--open (org-moor-fathom--heading)
                             (and beg end (buffer-substring-no-properties beg end))
                             windows))))

(defun org-moor-fathom--open (target text windows &optional append)
  "Open the draft for the heading at TARGET, starting from TEXT.
Nil TEXT is what `org-moor-fathom-draft-text' gives for the heading.
WINDOWS is put back when the draft is done.  When a draft for the heading
is already open or kept, APPEND adds TEXT to its end; otherwise you are
asked whether to start again."
  (let* ((text (or text
                   (with-current-buffer (marker-buffer target)
                     (org-with-wide-buffer
                      (goto-char target)
                      (org-moor-fathom-draft-text)))))
         (id (org-with-point-at target (org-id-get-create)))
         (file (expand-file-name (concat id ".org") org-moor-fathom-directory))
         (open (find-buffer-visiting file)))
    (make-directory org-moor-fathom-directory t)
    (cond
     ((and append (or open (file-exists-p file)))
      (with-current-buffer (or open (find-file-noselect file))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (unless (bobp) (insert "\n"))
        (insert text)
        (save-buffer)))
     ((or (not (or open (file-exists-p file)))
          (y-or-n-p "A draft for this heading exists; start again? "))
      (when open
        (with-current-buffer open (set-buffer-modified-p nil))
        (kill-buffer open))
      (with-temp-file file (insert text))))
    (pop-to-buffer (find-file-noselect file))
    (unless org-moor-fathom-mode
      (setq org-moor-fathom--target target
            org-moor-fathom--windows windows)
      (org-moor-fathom-mode 1))
    (goto-char (if append (point-max) (point-min)))))

(defun org-moor-fathom--done ()
  "Close the draft: delete its file and buffer, put the windows back."
  (let ((file buffer-file-name)
        (windows org-moor-fathom--windows))
    (set-buffer-modified-p nil)
    (kill-buffer)
    (when (and file (file-exists-p file))
      (delete-file file))
    (when windows
      (set-window-configuration windows))))

(defun org-moor-fathom-fit-headings (text level)
  "Return TEXT with its headings moved below a heading of LEVEL.
A draft's top headings become that heading's children, whatever depth
they were written at, and those under them keep their distance."
  (let ((top (and (string-match "^\\(\\*+\\) " text)
                  (cl-loop with pos = 0
                           while (string-match "^\\(\\*+\\) " text pos)
                           minimize (length (match-string 1 text))
                           do (setq pos (match-end 0))))))
    (if (not top)
        text
      (replace-regexp-in-string
       "^\\(\\*+\\) "
       (lambda (m)
         (concat (make-string (+ level 1 (- (length (string-trim-right m)) top)) ?*)
                 " "))
       text t t))))

(defun org-moor-fathom-finish ()
  "Write the draft at the end of its heading's own text.
Its headings become children of that heading."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (target org-moor-fathom--target))
    (unless (string-empty-p text)
      (with-current-buffer (marker-buffer target)
        (org-with-wide-buffer
         (goto-char target)
         (let ((text (org-moor-fathom-fit-headings text (org-current-level))))
           (org-moor--end-of-entry)
           (insert text "\n")))))
    (org-moor-fathom--done)
    (message "Written into %s"
             (org-with-point-at target (org-get-heading t t t t)))))

(defun org-moor-fathom-abort ()
  "Throw the draft away."
  (interactive)
  (when (or (not (buffer-modified-p))
            (y-or-n-p "Discard this draft? "))
    (org-moor-fathom--done)))

(defun org-moor-fathom-assist ()
  "Ask `org-moor-fathom-assist-function' to help with this draft."
  (interactive)
  (unless org-moor-fathom-assist-function
    (user-error "org-moor: no `org-moor-fathom-assist-function'"))
  (save-buffer)
  (funcall org-moor-fathom-assist-function buffer-file-name))

(provide 'org-moor-fathom)
;;; org-moor-fathom.el ends here
