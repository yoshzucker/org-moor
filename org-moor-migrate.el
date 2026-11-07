;;; org-moor-migrate.el --- Move records of the old form to the current one  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Records written before org-moor fetched nothing hold MOOR_ORIGIN,
;; MOOR_VIA, MOOR_FILE, MOOR_TAKEN and MOOR_SIGNIN, which it reads no
;; longer.  `org-moor-migrate-records' writes each of a sources directory
;; in the current form, once:
;;
;;   MOOR_ORIGIN  ORIGIN_URL, a path written as a file: URL
;;   MOOR_FILE    REPLICA_FILE
;;   MOOR_VIA     REPLICA_VIA
;;   MOOR_TAKEN   REPLICA_TAKEN, or REPLICA_REQUESTED with no replica there
;;   MOOR_SIGNIN  dropped: the source is awaited from the browser
;;
;; and what pandoc made of a replica beside it, REPLICA.md, as what is
;; machine readable of it, made from it.  A record holding a value of a
;; form not known is left as it is, and said.  Once every sources
;; directory is moved, this file has nothing more to do.

;;; Code:

(require 'org-moor-core)

(defconst org-moor-migrate--old-keys
  '("MOOR_ORIGIN" "MOOR_FILE" "MOOR_VIA" "MOOR_TAKEN" "MOOR_SIGNIN")
  "The properties of a record of the old form.")

(defun org-moor-migrate--url (value)
  "Return VALUE, an old origin, as an origin URL, or nil for a form not known.
A URL as it is; a path, / or C:/ or C:\\ or a shared folder's \\\\host,
by `org-moor-path-url' without being expanded here."
  (cond ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]+:[^\\]" value) value)
        ((string-match-p "\\`\\(?:[/\\]\\|[A-Za-z]:[/\\]\\)" value)
         (let ((path (replace-regexp-in-string "%" "%25" (subst-char-in-string ?\\ ?/ value))))
           (cond ((string-prefix-p "//" path) (concat "file:" path))
                 ((string-match-p "\\`[A-Za-z]:/" path) (concat "file:///" path))
                 (t (concat "file://" path)))))))

(defun org-moor-migrate--record (file)
  "Return (TEXT . NOTE) for the record FILE in the current form.
TEXT is nil for a record left as it is: of the current form already,
NOTE `current', or holding what is not known, NOTE saying what."
  (let* ((text (with-temp-buffer
                 (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
                 (buffer-string)))
         (dir (file-name-directory file))
         (lines (split-string text "\n"))
         (start (seq-position lines ":PROPERTIES:"))
         (end (and start (seq-position (nthcdr start lines) ":END:")))
         (end (and end (+ start end)))
         (props nil)
         (others nil))
    (cond
     ((not (string-search ":MOOR_" text)) (cons nil 'current))
     ((not end) (cons nil "no property drawer"))
     (t
      (dolist (line (seq-subseq lines (1+ start) end))
        (if (string-match "\\`[ \t]*:\\(MOOR_[A-Z_]+\\):[ \t]*\\(.*?\\)[ \t]*\\'" line)
            (push (cons (match-string 1 line) (match-string 2 line)) props)
          (push line others)))
      (let* ((get (lambda (key) (cdr (assoc key props))))
             (unknown (seq-remove (lambda (p) (member (car p) org-moor-migrate--old-keys)) props))
             (origin (funcall get "MOOR_ORIGIN"))
             (url (and origin (org-moor-migrate--url origin)))
             (replica (funcall get "MOOR_FILE"))
             (there (and replica (file-regular-p (expand-file-name replica dir))))
             (md (and there (concat replica ".md")))
             (made (and md (file-regular-p (expand-file-name md dir))
                        (member (org-moor-extension replica) '("docx" "pptx" "odt" "epub" "html" "htm"))))
             (new nil)
             (notes nil)
             (put (lambda (name value) (push (org-moor--property-line name value) new))))
        (cond
         (unknown (cons nil (format "properties not known: %s" (mapconcat #'car unknown ", "))))
         ((and origin (not url)) (cons nil (format "an origin of a form not known: %s" origin)))
         (t
          (when url (funcall put "ORIGIN_URL" url))
          (when replica (funcall put "REPLICA_FILE" replica))
          (when (and there (funcall get "MOOR_VIA"))
            (funcall put "REPLICA_VIA" (funcall get "MOOR_VIA")))
          (when (funcall get "MOOR_TAKEN")
            (funcall put (if there "REPLICA_TAKEN" "REPLICA_REQUESTED") (funcall get "MOOR_TAKEN")))
          (when made
            (funcall put "MACHINE_READABLE_MADE_FROM" replica)
            (funcall put "MACHINE_READABLE_MADE_BY" "pandoc")
            (funcall put "MACHINE_READABLE_FILE" md)
            (funcall put "MACHINE_READABLE_MADE"
                     (format-time-string (org-time-stamp-format t t)
                                         (file-attribute-modification-time
                                          (file-attributes (expand-file-name md dir)))))
            (push (format "machine readable %s" md) notes))
          (when (funcall get "MOOR_SIGNIN")
            (push (format "the sign-in to %s dropped" (funcall get "MOOR_SIGNIN")) notes))
          (unless there (push "its replica awaited" notes))
          (cons (string-join (append (seq-take lines (1+ start))
                                     (nreverse new) (nreverse others)
                                     (nthcdr end lines))
                             "\n")
                (string-join (nreverse notes) "; ")))))))))

(defun org-moor-migrate--plan (sources)
  "Return what moving the records in SOURCES does: (FILE TEXT NOTE) each.
TEXT is nil for one left as it is; one of the current form is not listed."
  (delq nil
        (mapcar (lambda (file)
                  (pcase-let ((`(,text . ,note) (org-moor-migrate--record file)))
                    (unless (eq note 'current) (list file text note))))
                (directory-files-recursively sources (concat "\\`" (regexp-quote org-moor-record-file) "\\'")))))

;;;###autoload (autoload 'org-moor-migrate-records "org-moor" nil t)
(defun org-moor-migrate-records (sources)
  "Write the records in the sources directory SOURCES in the current form.
What changes is listed in *org-moor-migrate*, and asked before it is
written; a record of the current form is left alone, so it may be run
again.  Interactively SOURCES is read, this buffer's sources offered --
a truss file's, beside it."
  (interactive (list (read-directory-name "Move the records in: " (org-moor-sources) nil t)))
  (let* ((sources (file-name-as-directory (expand-file-name sources)))
         (plan (org-moor-migrate--plan sources))
         (moving (seq-filter #'cadr plan)))
    (with-current-buffer (get-buffer-create "*org-moor-migrate*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Records in %s\n\n" (abbreviate-file-name sources)))
        (pcase-dolist (`(,file ,text ,note) plan)
          (insert (format "%s %s%s\n" (if text "move" "LEFT")
                          (file-relative-name file sources)
                          (if (string-empty-p note) "" (concat ": " note)))))
        (insert (format "\n%d to move, %d left as they are\n" (length moving)
                        (- (length plan) (length moving)))))
      (special-mode)
      (display-buffer (current-buffer)))
    (cond
     ((not moving) (message "org-moor: no record to move in %s" (abbreviate-file-name sources)))
     ((y-or-n-p (format "Move %d record%s, as listed? " (length moving)
                        (if (cdr moving) "s" "")))
      (pcase-dolist (`(,file ,text ,_note) moving)
        (org-moor--write-text file text))
      (message "org-moor: moved %d record%s in %s" (length moving)
               (if (cdr moving) "s" "") (abbreviate-file-name sources)))
     (t (message "org-moor: nothing moved")))))

(provide 'org-moor-migrate)
;;; org-moor-migrate.el ends here
