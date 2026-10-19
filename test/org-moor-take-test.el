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

(defmacro org-moor-test--serving (type disposition body &rest forms)
  "Run FORMS with every fetch sending BODY as TYPE, with DISPOSITION.
`fetched' is the URL fetched last.  No snapshot is taken."
  (declare (indent 3))
  `(let ((org-moor-snapshot-function nil)
         (fetched nil))
     (cl-letf (((symbol-function 'org-moor--fetch)
                (lambda (url file done) (setq fetched url)
                  (with-temp-file file (insert ,body))
                  (funcall done t ,type ,disposition))))
       (ignore fetched)
       ,@forms)))

(defun org-moor-test--kept (url &optional title)
  "Take URL at point, titled TITLE or as offered; return its record."
  (org-moor-test--titles-as-given
    (org-moor-record (org-moor-test--id-at
                      (org-moor-take-url url (or title (org-moor-url-file-name url) "T"))))))

(ert-deftest org-moor-test-take-url-with-snapshot ()
  (org-moor-test--with-notes "* Topic\n"
    (org-moor-test--serving "text/html" nil "<html>raw</html>"
      (let ((org-moor-snapshot-function
             (lambda (_url file done) (with-temp-file file (insert "whole")) (funcall done t))))
        (org-moor-test--titles-as-given
          (let* ((m (org-moor-take-url "https://example.com/" "Example"))
                 (record (org-moor-record (org-moor-test--id-at m))))
            (should (equal (plist-get record :origin) "https://example.com/"))
            (should (equal (plist-get record :via) "web"))
            (should (equal (plist-get record :file) "page.html"))
            (should (equal (with-temp-buffer
                             (insert-file-contents (plist-get record :copy))
                             (buffer-string))
                           "whole"))
            (should (equal (directory-files (plist-get record :dir) nil "\\`[^.]")
                           '("moor.org" "page.html")))
            (should (string-match-p "\\[\\[moor:[^]]+\\]\\[Example\\]\\]" (buffer-string)))))))))

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
    (org-moor-test--serving "application/pdf" nil "%PDF-1.7"
      (let ((record (org-moor-test--kept "https://x.com/rules.pdf")))
        (should (equal fetched "https://x.com/rules.pdf"))
        (should (equal (plist-get record :title) "rules.pdf"))
        (should (equal (plist-get record :file) "rules.pdf"))
        (should (plist-get record :copy))))))

(ert-deftest org-moor-test-file-is-told-by-what-it-sends ()
  (org-moor-test--with-notes "* Topic\n"
    ;; Named by its type when its URL is spelled with none.
    (org-moor-test--serving "application/pdf; charset=binary" nil "%PDF-1.7"
      (should (equal (plist-get (org-moor-test--kept "https://arxiv.org/pdf/2401.00001") :file)
                     "2401.00001.pdf")))
    ;; Named as its Content-Disposition names it.
    (org-moor-test--serving
        "application/vnd.openxmlformats-officedocument.presentationml.presentation"
        "attachment; filename*=UTF-8''%E8%B3%87%E6%96%99.pptx" "PK"
      (should (equal (plist-get (org-moor-test--kept "https://x.com/download?id=3") :file)
                     "資料.pptx")))
    ;; Sent with no type it says, it is known by its first bytes.
    (org-moor-test--serving "application/octet-stream" nil "%PDF-1.4"
      (should (equal (plist-get (org-moor-test--kept "https://x.com/get") :file)
                     "get.pdf")))
    ;; A page is a page, however it is spelled.
    (org-moor-test--serving "text/html; charset=utf-8" nil "<html></html>"
      (should (equal (plist-get (org-moor-test--kept "https://x.com/view.php?id=1") :file)
                     "page.html")))))

(ert-deftest org-moor-test-a-sign-in-page-is-not-the-file ()
  (org-moor-test--with-notes "* Topic\n"
    (org-moor-test--serving "text/html" nil "<!DOCTYPE html><html>Sign in</html>"
      (let ((record (org-moor-test--kept "https://x.com/deck.pptx")))
        (should-not (plist-get record :file))
        (should (equal (directory-files (plist-get record :dir) nil "\\`[^.]")
                       '("moor.org")))
        (should-not (directory-files (plist-get record :dir) nil "\\`\\.fetch"))))))

(ert-deftest org-moor-test-page-keeps-its-html-without-a-snapshot ()
  (org-moor-test--with-notes "* Topic\n"
    (org-moor-test--serving "text/html" nil "<html>raw</html>"
      (let* ((org-moor-snapshot-function (lambda (_url _file done) (funcall done nil)))
             (record (org-moor-test--kept "https://x.com/article")))
        (should (equal (plist-get record :file) "page.html"))
        (with-temp-buffer
          (insert-file-contents (plist-get record :copy))
          (should (equal (buffer-string) "<html>raw</html>")))))))

(ert-deftest org-moor-test-url-unwrap ()
  (should (equal (org-moor-url-unwrap
                  "https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fx.com%2F%E8%B3%87%E6%96%99.pptx&wdOrigin=BROWSELINK")
                 "https://x.com/資料.pptx"))
  (should (equal (org-moor-url-unwrap "https://x.com/share?url=https%3A%2F%2Fy.com%2Fpost")
                 "https://x.com/share?url=https%3A%2F%2Fy.com%2Fpost")))

(ert-deftest org-moor-test-disposition-name ()
  (should (equal (org-moor--disposition-name "attachment; filename=\"a b.pdf\"") "a b.pdf"))
  (should (equal (org-moor--disposition-name "inline; filename=a.pdf; size=3") "a.pdf"))
  (should-not (org-moor--disposition-name "inline")))

(ert-deftest org-moor-test-cookies-are-sent ()
  (let* ((cookies (make-temp-file "cookies"))
         (org-moor-cookie-file cookies)
         commands)
    (cl-letf (((symbol-function 'executable-find) #'identity)
              ((symbol-function 'make-process)
               (lambda (&rest args) (push (plist-get args :command) commands) nil)))
      (org-moor--curl "https://x.com/" "/tmp/f" #'ignore)
      (org-moor-snapshot-monolith "https://x.com/" "/tmp/p" #'ignore))
    (should (equal (seq-take (cadr commands) 4) (list "curl" "-fsSL" "-b" cookies)))
    (should (equal (seq-take (car commands) 4) (list "monolith" "-s" "-C" cookies)))
    (setq org-moor-cookie-file nil commands nil)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args) (push (plist-get args :command) commands) nil)))
      (org-moor--curl "https://x.com/" "/tmp/f" #'ignore))
    (should-not (member "-b" (car commands)))
    (delete-file cookies)
    (let ((org-moor-cookie-file cookies) said)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (should-not (org-moor--cookie-args "-b")))
      (should (string-match-p "no cookie file" said)))))

(ert-deftest org-moor-test-a-copy-arriving ()
  (let* ((downloads (make-temp-file "dl" t))
         (into (make-temp-file "src" t))
         (old (expand-file-name "old.pdf" downloads))
         (org-moor-arriving-timeout 10)
         got)
    (with-temp-file old (insert "old"))
    (set-file-times old (time-subtract nil 60))
    (funcall (org-moor-arriving downloads (current-time)) "https://x" into
             (lambda (name) (setq got (or name 'none))))
    (with-temp-file (expand-file-name "deck.pptx.crdownload" downloads) (insert "half"))
    (accept-process-output nil 1.2)
    (should-not got)
    (rename-file (expand-file-name "deck.pptx.crdownload" downloads)
                 (expand-file-name "deck.pptx" downloads))
    (while (not got) (accept-process-output nil 0.1))
    (should (equal got "deck.pptx"))
    (should (file-exists-p (expand-file-name "deck.pptx" into)))
    (should (file-exists-p old))
    (delete-directory downloads t)
    (delete-directory into t)))

(ert-deftest org-moor-test-opening-the-original-says-so ()
  (let ((opened nil) (said nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq opened u)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (org-moor-open-record '(:id "x" :title "Deck" :origin "https://x.com/d"))
      (should (equal opened "https://x.com/d"))
      (should (string-match-p "opened the original" said)))))

(ert-deftest org-moor-test-protocol-file-url-is-a-file ()
  (let (args)
    (cl-letf (((symbol-function 'org-moor-take-file)
               (lambda (&rest a) (setq args a)))
              ((symbol-function 'select-frame-set-input-focus) #'ignore))
      (org-moor-protocol '(:url "file:///C:/Users/me/%E8%B3%87%E6%96%99%20v2.pdf"))
      (should (equal (car args) "C:/Users/me/資料 v2.pdf"))
      (org-moor-protocol '(:url "file:///home/me/a.pdf"))
      (should (equal (car args) "/home/me/a.pdf")))))

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
