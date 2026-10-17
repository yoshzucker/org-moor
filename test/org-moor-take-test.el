;;; org-moor-take-test.el --- Tests for org-moor-take  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -l test/org-moor-take-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)

(ert-deftest org-moor-test-quote-string ()
  (let ((s (org-moor-quote-string "Chat from Robin" "* not a heading\nok\n\n"
                                  (encode-time '(0 12 10 17 10 2026 nil nil 32400)))))
    (should (string-prefix-p "#+begin_quote Chat from Robin [2026-10-17" s))
    (should (string-match-p "^,\\* not a heading$" s))
    (should (string-suffix-p "ok\n#+end_quote\n" s))))

(ert-deftest org-moor-test-quote-at-point ()
  (with-temp-buffer
    (org-mode)
    (insert "* Topic\n")
    (cl-letf (((symbol-function 'org-moor--clipboard) (lambda () "hello")))
      (org-moor-quote "Mail from Alex"))
    (goto-char (point-min))
    (should (re-search-forward "^#\\+begin_quote Mail from Alex \\[" nil t))
    (should (re-search-forward "^hello\n#\\+end_quote$" nil t))))

(defmacro org-moor-test--with-notes (text &rest body)
  "Run BODY in a file-visiting Org buffer holding TEXT, with a fresh store."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "moor" t))
          (org-moor-store-directory (expand-file-name "source/" dir))
          (org-id-locations-file (expand-file-name "ids" dir))
          (buf (find-file-noselect (expand-file-name "notes.org" dir))))
     (unwind-protect
         (with-current-buffer buf
           (insert ,text)
           (goto-char (point-min))
           ,@body)
       (with-current-buffer buf (set-buffer-modified-p nil))
       (kill-buffer buf)
       (delete-directory dir t))))

(ert-deftest org-moor-test-quote-at-picked-heading ()
  (org-moor-test--with-notes "* Topic\nbody\n** Child\n"
    (cl-letf (((symbol-function 'org-moor--clipboard) (lambda () "hi"))
              ((symbol-function 'org-moor--target) (lambda (_) (copy-marker (point-min)))))
      (org-moor-quote "Chat" t))
    (should (string-match-p "\\`\\* Topic\nbody\n#\\+begin_quote Chat .*\nhi\n#\\+end_quote\n\\*\\* Child"
                            (buffer-string)))))

(ert-deftest org-moor-test-take-file ()
  (org-moor-test--with-notes "* Topic\n** Other\n"
    (let ((orig (expand-file-name "manual.pdf" (file-name-directory buffer-file-name)))
          (org-moor-move-directories nil))
      (with-temp-file orig (insert "pdf"))
      (cl-letf (((symbol-function 'read-string) (lambda (_p &optional d &rest _) d)))
        (let ((m (org-moor-take-file orig)))
          (org-with-point-at m
            (should (equal (org-get-heading t t t t) "manual"))
            (should (= (org-current-level) 2))
            (let ((s (org-moor-source)))
              (should (plist-get s :copy))
              (should (equal (plist-get s :via) "file"))
              (should (org-entry-get nil "NOTER_DOCUMENT"))))))
      (should (file-exists-p orig))
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\* Other\n\\*\\* manual$" nil t)))))

(ert-deftest org-moor-test-take-url-without-snapshot ()
  (org-moor-test--with-notes "* Topic\n"
    (let ((org-moor-snapshot-function
           (lambda (_url file done) (with-temp-file file) (funcall done t))))
      (cl-letf (((symbol-function 'read-string) (lambda (_p &optional d &rest _) d)))
        (let ((m (org-moor-take-url "https://example.com/" "Example")))
          (org-with-point-at m
            (should (equal (org-entry-get nil "MOOR_ORIGIN") "https://example.com/"))
            (should (equal (org-entry-get nil "MOOR_FILE") "page.html"))))))))

(ert-deftest org-moor-test-move-or-copy ()
  (org-moor-test--with-notes "* Topic\n"
    (let* ((root (file-name-directory buffer-file-name))
           (downloads (file-name-as-directory (expand-file-name "dl" root)))
           (piled (expand-file-name "a.txt" downloads))
           (kept (expand-file-name "b.txt" root))
           (org-moor-move-directories (list downloads)))
      (make-directory downloads)
      (with-temp-file piled (insert "a"))
      (with-temp-file kept (insert "b"))
      (should (org-moor-move-p piled))
      (should-not (org-moor-move-p kept))
      (cl-letf (((symbol-function 'read-string) (lambda (_p &optional d &rest _) d)))
        (org-moor-take-file piled)
        (org-moor-take-file kept)
        (should-not (file-exists-p piled))
        (should (file-exists-p kept))
        (with-temp-file piled (insert "a"))
        (org-moor-take-file piled nil nil t)
        (should (file-exists-p piled))))))

(provide 'org-moor-take-test)
;;; org-moor-take-test.el ends here
