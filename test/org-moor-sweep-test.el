;;; org-moor-sweep-test.el --- Tests for picking and sweeping  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-sweep-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(defmacro org-moor-test--answering (answer &rest body)
  "Run BODY with every `completing-read' answering ANSWER."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ,answer)))
     ,@body))

(ert-deftest org-moor-test-read-heading ()
  (org-moor-test--with-notes "* Topic\n** Sub\n"
    (let ((org-moor-inbox-file buffer-file-name)
          (org-refile-targets '((nil :maxlevel . 3)))
          (org-refile-use-outline-path nil))
      (org-moor-test--answering ""
        (should (eq (org-moor-read-heading) 'inbox)))
      (org-moor-test--answering "Sub"
        (should (equal (org-with-point-at (org-moor-read-heading)
                         (org-get-heading t t t t))
                       "Sub")))
      (org-moor-test--answering "Topic/Fresh"
        (org-with-point-at (org-moor-read-heading)
          (should (equal (org-get-heading t t t t) "Fresh"))
          (should (= (org-current-level) 2))))
      (org-moor-test--answering "Brand new"
        (org-with-point-at (org-moor-read-heading)
          (should (equal (org-get-heading t t t t) "Brand new"))
          (should (= (org-current-level) 1)))))))

(ert-deftest org-moor-test-read-directory ()
  (let* ((root (file-name-as-directory (make-temp-file "roots" t)))
         (org-moor-directory-roots (list root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "a/b" root) t)
          (should (member (abbreviate-file-name (expand-file-name "a/b/" root))
                          (mapcar #'abbreviate-file-name
                                  (org-moor--subdirectories root 2))))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (org-moor-test--answering "new/place"
              (should (equal (org-moor-read-directory)
                             (expand-file-name "new/place/" root)))
              (should (file-directory-p (expand-file-name "new/place/" root))))))
      (delete-directory root t))))

(ert-deftest org-moor-test-put-away-and-take ()
  (org-moor-test--with-notes "* Topic\n"
    (let* ((dl (file-name-as-directory (expand-file-name "dl" dir)))
           (proj (file-name-as-directory (expand-file-name "proj" dir)))
           (file (expand-file-name "minutes.docx" dl))
           (org-moor-move-directories (list dl))
           (notes (current-buffer)))
      (make-directory dl) (make-directory proj)
      (with-temp-file file (insert "m"))
      (cl-letf (((symbol-function 'read-string) (lambda (_p &optional d &rest _) d))
                ((symbol-function 'org-moor-read-heading)
                 (lambda (&rest _) (with-current-buffer notes (copy-marker (point-min))))))
        (let ((moved (org-moor-put-away (list file) proj t)))
          (should (equal moved (list (expand-file-name "minutes.docx" proj))))
          (should-not (file-exists-p file))
          (should (file-exists-p (car moved)))))
      (goto-char (point-min))
      (should (re-search-forward "^\\[\\[moor:\\([^]]+\\)\\]\\[minutes\\]\\]$" nil t))
      (let ((record (org-moor-record (match-string 1))))
        (should (plist-get record :copy))
        (should (equal (plist-get record :origin) (expand-file-name "minutes.docx" proj)))))))

(ert-deftest org-moor-test-sweep-lists-the-pile ()
  (let* ((dl (file-name-as-directory (make-temp-file "dl" t)))
         (org-moor-move-directories (list dl)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.pdf" dl))
          (make-directory (expand-file-name "sub" dl))
          (should (equal (sort (mapcar #'file-name-nondirectory (org-moor-sweep--files))
                               #'string<)
                         '("a.pdf" "sub")))
          (org-moor-sweep)
          (should (equal (buffer-name) org-moor-sweep-buffer))
          (should (derived-mode-p 'dired-mode))
          (goto-char (point-min))
          (should (search-forward "a.pdf" nil t))
          (kill-buffer))
      (delete-directory dl t))))

(provide 'org-moor-sweep-test)
;;; org-moor-sweep-test.el ends here
