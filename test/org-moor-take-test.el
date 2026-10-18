;;; org-moor-take-test.el --- Tests for org-moor-take  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-take-test.el -f ert-run-tests-batch-and-exit

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
  "Run BODY in a file-visiting Org buffer holding TEXT, with fresh sources.
`dir' is the directory the file is in; the sources and org-dock's
copies are below it.  Every buffer visiting a file there is closed after."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "moor" t)))
          (org-moor-sources-directory (expand-file-name "sources/" dir))
          (org-dock-directory (expand-file-name "dock/" dir))
          (org-id-locations-file (expand-file-name "ids" dir))
          (buf (find-file-noselect (expand-file-name "notes.org" dir))))
     (unwind-protect
         (with-current-buffer buf
           (insert ,text)
           (goto-char (point-min))
           (save-window-excursion ,@body))
       (dolist (b (buffer-list))
         (let ((name (buffer-file-name b)))
           (when (and name (file-in-directory-p name dir))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (delete-directory dir t))))

(defmacro org-moor-test--titles-as-given (&rest body)
  "Run BODY with every `read-string' answering its default."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'read-string) (lambda (_p &optional d &rest _) d)))
     ,@body))

(defun org-moor-test--id-at (marker)
  "Return the id of the link MARKER is at."
  (org-with-point-at marker
    (and (looking-at org-moor--link-re) (match-string-no-properties 1))))

(ert-deftest org-moor-test-quote-at-picked-heading ()
  (org-moor-test--with-notes "* Topic\nbody\n** Child\n"
    (cl-letf (((symbol-function 'org-moor--clipboard) (lambda () "hi"))
              ((symbol-function 'org-moor--target) (lambda (_) (copy-marker (point-min)))))
      (org-moor-quote "Chat" t))
    (should (string-match-p "\\`\\* Topic\nbody\n#\\+begin_quote Chat .*\nhi\n#\\+end_quote\n\\*\\* Child"
                            (buffer-string)))))

(ert-deftest org-moor-test-take-file ()
  "Taking a file on a heading puts a link at the end of its own text, and
the copy, with its record, in the file's sources."
  (org-moor-test--with-notes "* Topic\ntext\n** Other\n"
    (let ((orig (expand-file-name "manual.pdf" dir))
          (org-moor-move-directories nil))
      (with-temp-file orig (insert "pdf"))
      (org-moor-test--titles-as-given
        (let* ((m (org-moor-take-file orig))
               (id (org-moor-test--id-at m))
               (record (org-moor-record id)))
          (should (equal (buffer-string)
                         (format "* Topic\ntext\n[[moor:%s][manual]]\n** Other\n" id)))
          (should (equal (plist-get record :title) "manual"))
          (should (equal (plist-get record :via) "file"))
          (should (equal (plist-get record :origin) orig))
          (should (equal (plist-get record :file) "manual.pdf"))
          (should (string-match-p "\\`\\[2026\\|\\`\\[" (plist-get record :taken)))
          (should (equal (plist-get record :copy)
                         (expand-file-name "manual.pdf" (org-moor-source-directory id))))
          (should (file-in-directory-p (plist-get record :copy) org-moor-sources-directory))))
      (should (file-exists-p orig)))))

(ert-deftest org-moor-test-take-file-in-text ()
  "Taken with point in a heading's text, the link goes after that line."
  (org-moor-test--with-notes "* Topic\nfirst\nsecond\n"
    (let ((orig (expand-file-name "a.txt" dir))
          (org-moor-move-directories nil))
      (with-temp-file orig (insert "a"))
      (search-forward "fir")
      (org-moor-test--titles-as-given
        (let ((id (org-moor-test--id-at (org-moor-take-file orig))))
          (should (equal (buffer-string)
                         (format "* Topic\nfirst\n[[moor:%s][a]]\nsecond\n" id))))))))

(ert-deftest org-moor-test-take-url-without-snapshot ()
  (org-moor-test--with-notes "* Topic\n"
    (let ((org-moor-snapshot-function
           (lambda (_url file done) (with-temp-file file) (funcall done t))))
      (org-moor-test--titles-as-given
        (let* ((m (org-moor-take-url "https://example.com/" "Example"))
               (record (org-moor-record (org-moor-test--id-at m))))
          (should (equal (plist-get record :origin) "https://example.com/"))
          (should (equal (plist-get record :via) "web"))
          (should (equal (plist-get record :file) "page.html"))
          (should (plist-get record :copy))
          (should (string-match-p "\\[\\[moor:[^]]+\\]\\[Example\\]\\]" (buffer-string))))))))

(ert-deftest org-moor-test-move-or-copy ()
  (org-moor-test--with-notes "* Topic\n"
    (let* ((downloads (file-name-as-directory (expand-file-name "dl" dir)))
           (piled (expand-file-name "a.txt" downloads))
           (kept (expand-file-name "b.txt" dir))
           (org-moor-move-directories (list downloads)))
      (make-directory downloads)
      (with-temp-file piled (insert "a"))
      (with-temp-file kept (insert "b"))
      (should (org-moor-move-p piled))
      (should-not (org-moor-move-p kept))
      (org-moor-test--titles-as-given
        (org-moor-take-file piled)
        (org-moor-take-file kept)
        (should-not (file-exists-p piled))
        (should (file-exists-p kept))
        (with-temp-file piled (insert "a"))
        (org-moor-take-file piled nil nil t)
        (should (file-exists-p piled))))))

(ert-deftest org-moor-test-inbox-is-top-level ()
  "With nowhere to go, a heading is made at the inbox's top level."
  (org-moor-test--with-notes "* Old\n** Older\n"
    (let ((org-moor-inbox-file buffer-file-name)
          (orig (expand-file-name "x.txt" dir))
          (org-moor-move-directories nil))
      (with-temp-file orig (insert "x"))
      (org-moor-insert-heading 'inbox "First")
      (cl-letf (((symbol-function 'org-moor--clipboard) (lambda () "words"))
                ((symbol-function 'org-moor--target) (lambda (_) 'inbox)))
        (org-moor-quote "Chat" t)
        (org-moor-test--titles-as-given
          (org-moor-take-file orig t)))
      (should (equal (org-map-entries (lambda () (org-get-heading t t t t)))
                     '("Old" "Older" "First" "Chat" "x")))
      (should (equal (org-map-entries #'org-current-level) '(1 2 1 1 1)))
      (should (string-match-p "^\\* Chat\n#\\+begin_quote Chat .*\nwords\n" (buffer-string)))
      (should (string-match-p "^\\* x\n\\[\\[moor:[^]]+\\]\\[x\\]\\]\n\\'" (buffer-string))))))

(ert-deftest org-moor-test-take-file-refuses-and-undoes ()
  (org-moor-test--with-notes "* Topic\n"
    (should-error (org-moor-take-file dir) :type 'user-error)
    (let ((orig (expand-file-name "x.txt" dir)))
      (with-temp-file orig (insert "x"))
      (org-moor-test--titles-as-given
        (cl-letf (((symbol-function 'copy-file) (lambda (&rest _) (error "Disk full"))))
          (should-error (org-moor-take-file orig))))
      (should (equal (buffer-string) "* Topic\n"))
      (should-not (org-moor-sources-in org-moor-sources-directory)))))

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
      (cl-letf (((symbol-function 'org-moor-download)
                 (lambda (url file done) (setq fetched url)
                   (with-temp-file file) (funcall done t))))
        (org-moor-test--titles-as-given
          (let ((record (org-moor-record (org-moor-test--id-at
                                          (org-moor-take-url "https://x.com/rules.pdf")))))
            (should (equal fetched "https://x.com/rules.pdf"))
            (should (equal (plist-get record :title) "rules"))
            (should (equal (plist-get record :file) "rules.pdf"))
            (should (plist-get record :copy))))))))

(ert-deftest org-moor-test-protocol-path-keeps-origin ()
  (let (args)
    (cl-letf (((symbol-function 'org-moor-take-file)
               (lambda (&rest a) (setq args (cons org-moor--default-title a))))
              ((symbol-function 'select-frame-set-input-focus) #'ignore))
      (org-moor-protocol '(:path "/tmp/deck.pptx" :title "Plan"
                           :origin "https://s.example.com/deck.pptx" :via "chat"))
      (should (equal args '("Plan" "/tmp/deck.pptx" t "chat" nil
                            "https://s.example.com/deck.pptx"))))))

(ert-deftest org-moor-test-take-into-a-heading-out ()
  "A file taken to a heading that is out goes into its copy, and its source
into the sources of the heading's own file."
  (org-moor-test--with-notes "* Topic\ntext\n"
    (let ((orig (expand-file-name "a.txt" dir))
          (org-moor-move-directories nil)
          (notes (current-buffer)))
      (with-temp-file orig (insert "a"))
      (let ((copy (save-current-buffer (org-dock-heading (point-min) :display nil))))
        (org-moor-test--titles-as-given
          (let ((m (org-moor-take-file orig (copy-marker (point-min)))))
            (should (eq (marker-buffer m) copy))
            (should (org-moor-record (org-moor-test--id-at m) org-moor-sources-directory))))
        (should (equal (with-current-buffer notes (buffer-string)) "* Topic\ntext\n"))
        (should (string-match-p "\\`\\* Topic\ntext\n\\[\\[moor:"
                                (with-current-buffer copy (buffer-string))))))))

(provide 'org-moor-take-test)
;;; org-moor-take-test.el ends here
