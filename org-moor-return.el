;;; org-moor-return.el --- Get back to what was taken  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; There is no state to return to -- nothing is ever put away, since anything
;; can be asked for again at any time.  So the way back is a list of every
;; heading something was taken to: a source, or a heading with a titled quote
;; in its own text.  Most recently taken first, because that is usually where
;; you were.
;;
;; The list is found, not kept: the files of `org-moor-files-function' are
;; read each time, so there is no index to fall out of step with them.

;;; Code:

(require 'org-moor-core)
(require 'cl-lib)

(defcustom org-moor-files-function #'org-moor-default-files
  "Function returning the Org files `org-moor-jump' looks in."
  :type 'function
  :group 'org-moor)

(defun org-moor-default-files ()
  "Return `org-agenda-files' and the Org files under `org-directory'."
  (delete-dups
   (append (org-agenda-files t)
           (and org-directory (file-directory-p org-directory)
                (directory-files-recursively org-directory "\\.org\\'")))))

(defconst org-moor--taken-re
  (concat "^[ \t]*\\(?:#\\+begin_quote[ \t]+\\(.+\\)"
          "\\|:" org-moor-property-taken ":[ \t]*\\(.+\\)\\)$")
  "A titled quote's first line, or a source's taken time.")

(defun org-moor--time-in (string)
  "Return the first timestamp in STRING as a time, or nil."
  (when (and string (string-match org-ts-regexp-both string))
    (org-time-string-to-time (match-string 0 string))))

(defun org-moor-anchors-in-buffer ()
  "Return the headings in this buffer something was taken to.
Each is a plist: :marker :path :time, the latest taking, and :count."
  (let ((table (make-hash-table)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-moor--taken-re nil t)
       (let ((title (match-string 1))
             (taken (match-string 2)))
         (unless (save-excursion (org-before-first-heading-p))
           (save-excursion
             (org-back-to-heading t)
             (let* ((pos (point))
                    (time (org-moor--time-in (or taken title)))
                    (old (gethash pos table)))
               (puthash pos
			(list :marker (or (plist-get old :marker) (point-marker))
                              :path (or (plist-get old :path)
					(org-format-outline-path
					 (org-get-outline-path t) 1000))
                              :time (if (and old (plist-get old :time)
                                             (or (null time)
						 (time-less-p time (plist-get old :time))))
					(plist-get old :time)
                                      time)
                              :count (1+ (or (plist-get old :count) 0)))
			table)))))))
    (org-moor--anchors table)))

(defun org-moor--anchors (table)
  "Return the values of TABLE as a list."
  (let (all)
    (maphash (lambda (_ v) (push v all)) table)
    all))

(defun org-moor-anchors ()
  "Return every anchor in the files of `org-moor-files-function', newest first."
  (sort (cl-loop for file in (funcall org-moor-files-function)
                 when (file-readable-p file)
                 append (with-current-buffer (find-file-noselect file)
                          (org-moor-anchors-in-buffer)))
        (lambda (a b)
          (let ((ta (plist-get a :time)) (tb (plist-get b :time)))
            (cond ((null tb) t)
                  ((null ta) nil)
                  (t (time-less-p tb ta)))))))

(defun org-moor--candidate (anchor)
  "Return the completion string for ANCHOR."
  (format "%s  %s"
          (plist-get anchor :path)
          (propertize (format "%s · %s %d"
                              (file-name-nondirectory
                               (buffer-file-name (marker-buffer (plist-get anchor :marker))))
                              (if (plist-get anchor :time)
                                  (format-time-string "%Y-%m-%d" (plist-get anchor :time))
                                "----------")
                              (plist-get anchor :count))
                      'face 'shadow)))

;;;###autoload (autoload 'org-moor-jump "org-moor" nil t)
(defun org-moor-jump ()
  "Go to a heading something was taken to, newest first."
  (interactive)
  (let* ((anchors (or (org-moor-anchors)
                      (user-error "org-moor: nothing taken yet")))
         (table (mapcar (lambda (a) (cons (org-moor--candidate a) a)) anchors))
         (choice (completing-read
                  "Return to: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        '(metadata (display-sort-function . identity))
                      (complete-with-action action table string pred)))
                  nil t))
         (marker (plist-get (cdr (assoc choice table)) :marker)))
    (pop-to-buffer-same-window (marker-buffer marker))
    (widen)
    (goto-char marker)
    (org-fold-show-context)
    (org-fold-show-entry)))

(provide 'org-moor-return)
;;; org-moor-return.el ends here
