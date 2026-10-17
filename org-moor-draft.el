;;; org-moor-draft.el --- Write what a source means  -*- lexical-binding: t; -*-

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
;; `org-moor-draft' opens a draft, the way `org-capture' does, beside the
;; heading it is for.  It starts from the region, or the heading's quotes,
;; or nothing; the sources themselves are read where they are, beside it
;; with `org-moor-noter', and their places are added to the draft from
;; there.  C-c C-c writes the draft at the end of the heading's own text,
;; and C-c C-k throws it away.
;;
;; C-c C-a asks an assistant -- `org-moor-draft-assist-function' -- to do
;; something with the draft: rewrite it, add to it, or answer a question
;; about the sources.  It is given the draft, what you asked, and the
;; heading's sources as files to read, each made readable once by
;; `org-moor-draft-converters'.  What it gives back is set into the draft
;; as conflicts, one for each place it differs, to be kept or refused one
;; by one with `smerge-mode' -- so that nothing in the draft changes but
;; what you take, and an answer is read there and refused.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-take)
(require 'smerge-mode)
(require 'cl-lib)

(defcustom org-moor-draft-directory
  (expand-file-name "org-moor/" temporary-file-directory)
  "Directory drafts are kept in while they are being written."
  :type 'directory
  :group 'org-moor)

(defcustom org-moor-draft-converters
  '(("docx" . org-moor-draft-pandoc)
    ("pptx" . org-moor-draft-pandoc)
    ("odt" . org-moor-draft-pandoc)
    ("epub" . org-moor-draft-pandoc)
    ("html" . org-moor-draft-pandoc)
    ("htm" . org-moor-draft-pandoc))
  "How a copy is made readable by an assistant, by extension.
Each function is called with the copy's path and returns the path of a
file to read in its place.  A copy whose extension is not here is read
as it is: text, Markdown and PDF need nothing."
  :type '(alist :key-type string :value-type function)
  :group 'org-moor)

(defcustom org-moor-draft-assist-function nil
  "Function asked to help with a draft, or nil.
Called by \\[org-moor-draft-assist] with a plist and a callback.  The
plist has:

  :id           the heading's id, the same for every draft of it
  :heading      the heading's title
  :entry        the heading's text, with its subtree, as Org
  :sources      the sources at and under the heading, as plists with
                :id :title :origin and :file, a file to read, or nil
  :draft        the draft's text
  :instruction  what was asked, or \"\" for the usual
  :fresh        non-nil when asked to start anew, with \\[universal-argument]

The callback is called, in any buffer, with the text the draft should
become; the differences are set into the draft to be taken or refused."
  :type '(choice (const nil) function)
  :group 'org-moor)

;;;; Making a copy readable

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

(defun org-moor-draft-pandoc (file)
  "Return FILE turned into Markdown by pandoc, beside it in the store.
Made once, and again only when FILE is newer; it goes with the copy."
  (let ((md (concat file ".md")))
    (unless (file-newer-than-file-p md file)
      (org-moor--process-string "pandoc" "--to=gfm" "--wrap=none"
                                "-o" md file))
    md))

(defun org-moor-draft-readable (copy)
  "Return the file an assistant reads for COPY."
  (let ((fn (cdr (assoc (downcase (or (file-name-extension copy) ""))
                        org-moor-draft-converters))))
    (if fn (funcall fn copy) copy)))

(defun org-moor-draft-sources (target)
  "Return the sources at and under the heading at TARGET, as plists."
  (org-with-point-at target
    (mapcar (lambda (marker)
              (org-with-point-at marker
                (let ((source (org-moor-source)))
                  (list :id (plist-get source :id)
                        :title (org-get-heading t t t t)
                        :origin (plist-get source :origin)
                        :file (and (plist-get source :copy)
                                   (file-exists-p (plist-get source :copy))
                                   (org-moor-draft-readable
                                    (plist-get source :copy)))))))
            (org-moor--sources-at))))

(defun org-moor-draft-quotes-text ()
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

;;;; Setting a rewrite into the draft

(defun org-moor-draft--lcs (a b)
  "Return the longest common subsequence table of vectors A and B."
  (let* ((n (length a)) (m (length b))
         (table (make-vector (1+ n) nil)))
    (dotimes (i (1+ n)) (aset table i (make-vector (1+ m) 0)))
    (cl-loop for i from (1- n) downto 0 do
             (cl-loop for j from (1- m) downto 0 do
                      (aset (aref table i) j
                            (if (equal (aref a i) (aref b j))
                                (1+ (aref (aref table (1+ i)) (1+ j)))
                              (max (aref (aref table (1+ i)) j)
                                   (aref (aref table i) (1+ j)))))))
    table))

(defun org-moor-draft-conflicts (old new)
  "Return OLD with each place NEW differs from it set as a conflict.
Lines alike are kept once; lines that differ become a conflict between
OLD's and NEW's, as `smerge-mode' reads them, each line ended.  Nil
when they are alike."
  (let* ((a (vconcat (split-string old "\n")))
         (b (vconcat (split-string new "\n")))
         (table (org-moor-draft--lcs a b))
         (i 0) (j 0) mine theirs out changed)
    (cl-flet ((flush ()
                (when (or mine theirs)
                  (setq changed t)
                  (push (concat "<<<<<<< draft\n"
                                (mapconcat (lambda (l) (concat l "\n")) (nreverse mine) "")
                                "=======\n"
                                (mapconcat (lambda (l) (concat l "\n")) (nreverse theirs) "")
                                ">>>>>>> assistant")
                        out)
                  (setq mine nil theirs nil))))
      (while (or (< i (length a)) (< j (length b)))
        (cond
         ((and (< i (length a)) (< j (length b))
               (equal (aref a i) (aref b j)))
          (flush)
          (push (aref a i) out)
          (cl-incf i) (cl-incf j))
         ((and (< i (length a))
               (or (>= j (length b))
                   (>= (aref (aref table (1+ i)) j)
                       (aref (aref table i) (1+ j)))))
          (push (aref a i) mine)
          (cl-incf i))
         (t
          (push (aref b j) theirs)
          (cl-incf j))))
      (flush))
    (and changed (concat (mapconcat #'identity (nreverse out) "\n") "\n"))))

(defun org-moor-draft--conflicted-p ()
  "Return non-nil when this buffer still has a conflict in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward smerge-begin-re nil t)))

(defun org-moor-draft--receive (buffer text)
  "Set TEXT into the draft BUFFER as conflicts, to be taken or refused."
  (if (not (buffer-live-p buffer))
      (message "org-moor: the draft was closed before the reply came")
    (with-current-buffer buffer
      (let ((merged (org-moor-draft-conflicts
                     (string-trim-right
                      (buffer-substring-no-properties (point-min) (point-max)))
                     (string-trim-right text))))
        (if (not merged)
            (message "org-moor: the reply is the draft as it is")
          (erase-buffer)
          (insert merged)
          (smerge-mode 1)
          (goto-char (point-min))
          (unless (looking-at smerge-begin-re)
            (smerge-next))
          (pop-to-buffer buffer)
          (set-window-point (selected-window) (point))
          (message (substitute-command-keys
                    "org-moor: \\[smerge-keep-lower] takes, \\[smerge-keep-upper] refuses, \\[smerge-next] next")))))))

;;;; The draft

(defvar org-moor-draft-written-hook nil
  "Hook run in the heading's buffer, at the heading, once a draft is written.")

(defvar-local org-moor-draft--target nil
  "Marker of the heading this draft is written for.")

(defvar-local org-moor-draft--windows nil
  "Window configuration to restore when the draft is done.")

(defvar org-moor-draft-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-moor-draft-finish)
    (define-key map (kbd "C-c C-k") #'org-moor-draft-abort)
    (define-key map (kbd "C-c C-a") #'org-moor-draft-assist)
    map)
  "Keys of a draft buffer.")

(define-minor-mode org-moor-draft-mode
  "A draft of what a source means, to be written into its heading."
  :lighter " Draft"
  (when org-moor-draft-mode
    (setq header-line-format
          (substitute-command-keys
           (format "Draft for %s.  \\[org-moor-draft-finish] write, \\[org-moor-draft-abort] discard%s"
                   (org-with-point-at org-moor-draft--target
                     (org-get-heading t t t t))
                   (if org-moor-draft-assist-function
                       ", \\[org-moor-draft-assist] assist" ""))))))

(defun org-moor-draft--heading ()
  "Return a marker at the heading point is in, or signal."
  (unless (derived-mode-p 'org-mode)
    (user-error "org-moor: not in an Org buffer"))
  (save-excursion
    (when (org-before-first-heading-p)
      (user-error "org-moor: not under a heading"))
    (org-back-to-heading t)
    (point-marker)))

;;;###autoload (autoload 'org-moor-draft "org-moor" nil t)
(defun org-moor-draft (&optional beg end)
  "Draft what the heading at point means.
It starts from the region BEG..END when there is one, else from the
heading's quotes.  On a file in Dired, take the file first, with
`org-moor-take-file', and draft for its new heading."
  (interactive (and (use-region-p) (list (region-beginning) (region-end))))
  (let ((windows (current-window-configuration)))
    (if-let* ((file (org-moor--file-at-point)))
        (org-moor-draft--open (org-moor-take-file file t) nil windows)
      (org-moor-draft--open (org-moor-draft--heading)
                            (and beg end (buffer-substring-no-properties beg end))
                            windows))))

(defun org-moor-draft--open (target text windows &optional append)
  "Open the draft for the heading at TARGET, starting from TEXT.
Nil TEXT is the heading's quotes, or nothing.  WINDOWS is put back when
the draft is done.  When a draft for the heading is already open or
kept, APPEND adds TEXT to its end; otherwise you are asked whether to
start again."
  (let* ((text (or text
                   (org-with-point-at target (org-moor-draft-quotes-text))
                   ""))
         (id (org-with-point-at target (org-id-get-create)))
         (file (expand-file-name (concat id ".org") org-moor-draft-directory))
         (open (find-buffer-visiting file)))
    (make-directory org-moor-draft-directory t)
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
    (unless org-moor-draft-mode
      (setq org-moor-draft--target target
            org-moor-draft--windows windows)
      (org-moor-draft-mode 1))
    (goto-char (if append (point-max) (point-min)))))

(defun org-moor-draft--done ()
  "Close the draft: delete its file and buffer, put the windows back."
  (let ((file buffer-file-name)
        (windows org-moor-draft--windows))
    (set-buffer-modified-p nil)
    (kill-buffer)
    (when (and file (file-exists-p file))
      (delete-file file))
    (when windows
      (set-window-configuration windows))))

(defun org-moor-draft-fit-headings (text level)
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

(defun org-moor-draft-finish ()
  "Write the draft at the end of its heading's own text.
Its headings become children of that heading."
  (interactive)
  (when (org-moor-draft--conflicted-p)
    (user-error "org-moor: take or refuse what the assistant gave first"))
  (let ((text (string-trim (buffer-string)))
        (target org-moor-draft--target))
    (unless (string-empty-p text)
      (with-current-buffer (marker-buffer target)
        (org-with-wide-buffer
         (goto-char target)
         (let ((text (org-moor-draft-fit-headings text (org-current-level))))
           (org-moor--end-of-entry)
           (insert text "\n")))
        (org-with-point-at target
          (run-hooks 'org-moor-draft-written-hook))))
    (org-moor-draft--done)
    (message "Written into %s"
             (org-with-point-at target (org-get-heading t t t t)))))

(defun org-moor-draft-abort ()
  "Throw the draft away."
  (interactive)
  (when (or (not (buffer-modified-p))
            (y-or-n-p "Discard this draft? "))
    (org-moor-draft--done)))

(defun org-moor-draft-assist (&optional fresh)
  "Ask `org-moor-draft-assist-function' to do something with this draft.
What to do is asked: nothing is its usual help.  With FRESH
\(\\[universal-argument]), it starts anew rather than going on from before."
  (interactive "P")
  (unless org-moor-draft-assist-function
    (user-error "org-moor: no `org-moor-draft-assist-function'"))
  (when (org-moor-draft--conflicted-p)
    (user-error "org-moor: take or refuse what the assistant gave first"))
  (save-buffer)
  (let* ((instruction (string-trim (read-string "Assist (empty for the usual): ")))
         (target org-moor-draft--target)
         (buffer (current-buffer))
         (context
          (org-with-point-at target
            (list :id (org-id-get-create)
                  :heading (org-get-heading t t t t)
                  :entry (org-with-wide-buffer
                          (buffer-substring-no-properties
                           (point) (progn (org-end-of-subtree t t) (point))))
                  :sources (org-moor-draft-sources target)
                  :draft (with-current-buffer buffer
                           (buffer-substring-no-properties (point-min) (point-max)))
                  :instruction instruction
                  :fresh (and fresh t)))))
    (funcall org-moor-draft-assist-function context
             (lambda (text) (org-moor-draft--receive buffer text)))))

(provide 'org-moor-draft)
;;; org-moor-draft.el ends here
