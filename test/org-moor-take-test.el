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

(ert-deftest org-moor-test-inbox-is-top-level ()
  (org-moor-test--with-notes "* Old\n** Older\n"
    (let ((org-moor-inbox-file buffer-file-name))
      (org-moor-insert-source-heading 'inbox "First")
      (org-moor-insert-source-heading 'inbox "Second")
      (cl-letf (((symbol-function 'org-moor--clipboard) (lambda () "words"))
                ((symbol-function 'org-moor--target) (lambda (_) 'inbox)))
        (org-moor-quote "Chat" t))
      (should (equal (org-map-entries (lambda () (org-get-heading t t t t)))
                     '("Old" "Older" "First" "Second" "Chat")))
      (should (equal (org-map-entries #'org-current-level) '(1 2 1 1 1)))
      (should (string-match-p "^\\* Chat\n#\\+begin_quote Chat .*\nwords\n" (buffer-string))))))

(ert-deftest org-moor-test-take-file-refuses-and-undoes ()
  (org-moor-test--with-notes "* Topic\n"
    (should-error (org-moor-take-file (file-name-directory buffer-file-name))
                  :type 'user-error)
    (let ((orig (expand-file-name "x.txt" (file-name-directory buffer-file-name))))
      (with-temp-file orig (insert "x"))
      (cl-letf (((symbol-function 'read-string) (lambda (_p &optional d &rest _) d))
                ((symbol-function 'copy-file) (lambda (&rest _) (error "Disk full"))))
        (should-error (org-moor-take-file orig)))
      (should (equal (buffer-string) "* Topic\n")))))

(ert-deftest org-moor-test-url-file-name ()
  (should (equal (org-moor-url-file-name "https://x.com/a/Manual%20v3.pdf?dl=1")
                 "Manual v3.pdf"))
  (should (equal (org-moor-url-file-name "https://x.com/%E8%B3%87%E6%96%99.pptx")
                 "資料.pptx"))
  (should-not (org-moor-url-file-name "https://x.com/article"))
  (should-not (org-moor-url-file-name "https://x.com/")))

(ert-deftest org-moor-test-take-url-downloads-a-file ()
  (org-moor-test--with-notes "* Topic\n"
    (let (fetched)
      (cl-letf (((symbol-function 'read-string) (lambda (_p &optional d &rest _) d))
                ((symbol-function 'org-moor-download)
                 (lambda (url file done) (setq fetched url)
                   (with-temp-file file) (funcall done t))))
        (let ((m (org-moor-take-url "https://x.com/rules.pdf")))
          (should (equal fetched "https://x.com/rules.pdf"))
          (org-with-point-at m
            (should (equal (org-get-heading t t t t) "rules"))
            (should (equal (org-entry-get nil "MOOR_FILE") "rules.pdf"))
            (should (org-entry-get nil "NOTER_DOCUMENT"))))))))

(ert-deftest org-moor-test-protocol-path-keeps-origin ()
  (let (args)
    (cl-letf (((symbol-function 'org-moor-take-file)
               (lambda (&rest a) (setq args (cons org-moor--default-title a))))
              ((symbol-function 'select-frame-set-input-focus) #'ignore))
      (org-moor-protocol '(:path "/tmp/deck.pptx" :title "Plan"
                           :origin "https://s.example.com/deck.pptx" :via "chat"))
      (should (equal args '("Plan" "/tmp/deck.pptx" t "chat" nil
                            "https://s.example.com/deck.pptx"))))))

(provide 'org-moor-take-test)
;;; org-moor-take-test.el ends here
