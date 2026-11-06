;;; org-moor-take-test.el --- Tests for org-moor-take  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-take-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)

;; With native compilation, a primitive the tests redefine is given a
;; trampoline, compiled then -- for each `ert-run-tests-batch-and-exit'
;; afresh, into a cache of its own -- while a test may have redefined what
;; compiling uses, `make-process' or `executable-find', and it fails.
;; What is tested is called from Lisp, which sees the redefinition anyway.
(defvar native-comp-enable-subr-trampolines)
(setq native-comp-enable-subr-trampolines nil)

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

(defmacro org-moor-test--clipboard (text image &rest body)
  "Run BODY with TEXT on the clipboard, and an image when IMAGE."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'org-moor--clipboard-text) (lambda () ,text))
             ((symbol-function 'org-moor--clipboard-image)
              (lambda (file)
                (when ,image
                  (let ((coding-system-for-write 'no-conversion))
                    (with-temp-file file (insert "PNG")))
                  file))))
     ,@body))

(ert-deftest org-moor-test-take-clipboard-text ()
  "The clipboard's text is a source of its own, kept as a text file."
  (org-moor-test--with-notes "* Topic\nbody\n** Child\n"
    (org-moor-test--clipboard "* not a heading\nok\n\n" nil
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Chat from Robin")))
        (let* ((link (org-moor-take-clipboard nil "chat" "https://chat.example/m/1"))
               (record (org-moor-record (org-moor-test--id-at link))))
          (should (string-match-p "\\`\\* Topic\nbody\n\\[\\[moor:[^]]+\\]\\[Chat from Robin\\]\\]\n\\*\\* Child"
                                  (buffer-string)))
          (should (equal (plist-get record :title) "Chat from Robin"))
          (should (equal (plist-get record :via) "chat"))
          (should (equal (plist-get record :origin) "https://chat.example/m/1"))
          (should (equal (plist-get record :file) "Chat from Robin.txt"))
          (should (equal (with-temp-buffer
                           (insert-file-contents (plist-get record :replica))
                           (buffer-string))
                         "* not a heading\nok\n")))))))

(ert-deftest org-moor-test-take-clipboard-image ()
  "An image on the clipboard is kept as a PNG; with text too, it is asked."
  (org-moor-test--with-notes "* Topic\n"
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Board: a/b")))
      (org-moor-test--clipboard nil t
        (let ((record (org-moor-record (org-moor-test--id-at (org-moor-take-clipboard)))))
          (should (equal (plist-get record :file) "Board a b.png"))
          (should (equal (plist-get record :via) "clipboard"))
          (should-not (plist-get record :origin))
          (should (plist-get record :replica))))
      (org-moor-test--clipboard "A1\tB1" t
        (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?t)))
          (should (equal (plist-get (org-moor-record
                                     (org-moor-test--id-at (org-moor-take-clipboard)))
                                    :file)
                         "Board a b.txt")))))))

(ert-deftest org-moor-test-take-clipboard-empty ()
  (org-moor-test--with-notes "* Topic\n"
    (org-moor-test--clipboard nil nil
      (cl-letf (((symbol-function 'current-kill) (lambda (&rest _) nil)))
        (should-error (org-moor-take-clipboard) :type 'user-error)))
    (should (equal (buffer-string) "* Topic\n"))))

(ert-deftest org-moor-test-take-file ()
  "Taking a file on a heading puts a link at the end of its own text, and
the replica, with its record, in the file's sources."
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
          (should (equal (plist-get record :replica)
                         (expand-file-name "manual.pdf" (org-moor-source-directory id))))
          (should (file-in-directory-p (plist-get record :replica) org-moor-sources-directory))))
      (should (file-exists-p orig)))))

(ert-deftest org-moor-test-take-file-in-text ()
  "Taken with point in a heading's text, the link goes there, point after it."
  (org-moor-test--with-notes "* Topic\nfirst\nsecond\n"
    (let ((orig (expand-file-name "a.txt" dir))
          (org-moor-move-directories nil))
      (with-temp-file orig (insert "a"))
      (search-forward "fir")
      (org-moor-test--titles-as-given
        (let ((id (org-moor-test--id-at (org-moor-take-file orig))))
          (should (equal (buffer-string)
                         (format "* Topic\nfir[[moor:%s][a]]st\nsecond\n" id)))
          (should (looking-at-p "st\n")))))))

(defvar evil-local-mode)
(defvar evil-state)

(ert-deftest org-moor-test-take-file-in-text-evil-normal ()
  "In an Evil state with the cursor on a character -- normal, motion,
visual -- the link goes after that character."
  (org-moor-test--with-notes "* Topic\nfirst\n"
    (let ((orig (expand-file-name "a.txt" dir))
          (org-moor-move-directories nil)
          (evil-local-mode t)
          (evil-state 'motion))
      (with-temp-file orig (insert "a"))
      (search-forward "firs")
      (org-moor-test--titles-as-given
        (let ((id (org-moor-test--id-at (org-moor-take-file orig))))
          (should (equal (buffer-string)
                         (format "* Topic\nfirst[[moor:%s][a]]\n" id)))
          (should (eolp)))))))

(defmacro org-moor-test--serving (type disposition body &rest forms)
  "Run FORMS with every fetch sending BODY as TYPE, with DISPOSITION.
`fetched' is the URL fetched last.  No snapshot is taken, and no browser
is asked to download it."
  (declare (indent 3))
  `(let ((org-moor-snapshot-function nil)
         (org-moor-browser-download-function nil)
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
                             (insert-file-contents (plist-get record :replica))
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
      (cl-letf (((symbol-function 'org-moor--target) (lambda (_) 'inbox)))
        (org-moor-test--clipboard "words" nil
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Chat")))
            (org-moor-take-clipboard t)))
        (org-moor-test--titles-as-given
          (org-moor-take-file orig t)))
      (should (equal (org-map-entries (lambda () (org-get-heading t t t t)))
                     '("Old" "Older" "First" "Chat" "x")))
      (should (equal (org-map-entries #'org-current-level) '(1 2 1 1 1)))
      (should (string-match-p "^\\* Chat\n\\[\\[moor:[^]]+\\]\\[Chat\\]\\]\n" (buffer-string)))
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
        (should (plist-get record :replica))))))

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
          (insert-file-contents (plist-get record :replica))
          (should (equal (buffer-string) "<html>raw</html>")))))))

(ert-deftest org-moor-test-url-unwrap ()
  (should (equal (org-moor-url-unwrap
                  "https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fx.com%2F%E8%B3%87%E6%96%99.pptx&wdOrigin=BROWSELINK")
                 "https://x.com/資料.pptx"))
  (should (equal (org-moor-url-unwrap "https://x.com/share?url=https%3A%2F%2Fy.com%2Fpost")
                 "https://x.com/share?url=https%3A%2F%2Fy.com%2Fpost"))
  (should (equal (org-moor-url-unwrap
                  "https://x.com:8443/sites/a/Docs/Forms/AllItems.aspx?id=%2Fsites%2Fa%2FDocs%2F%E8%B3%87%E6%96%99.pdf&parent=%2Fsites%2Fa%2FDocs")
                 "https://x.com:8443/sites/a/Docs/資料.pdf"))
  (should (equal (org-moor-url-unwrap "https://x.com/login?next=%2Fhome")
                 "https://x.com/login?next=%2Fhome")))

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
      (should (equal args '("Plan" "/tmp/deck.pptx" nil "chat" nil
                            "https://s.example.com/deck.pptx"))))))

(ert-deftest org-moor-test-protocol-takes-where-emacs-is-left ()
  "From outside Emacs, at point in the Org buffer in front; a heading is
picked from any other."
  (let (where)
    (cl-letf (((symbol-function 'org-moor-take-url)
               (lambda (_url _title pick &rest _)
                 (setq where (if (and (not pick) (derived-mode-p 'org-mode))
                                 (point)
                               'picked))))
              ((symbol-function 'select-frame-set-input-focus) #'ignore))
      (save-window-excursion
        (with-temp-buffer
          (org-mode)
          (insert "* Topic\ntext\n")
          (goto-char 12)
          (set-window-buffer (selected-window) (current-buffer))
          (with-temp-buffer
            (org-moor-protocol '(:url "https://x.com/a")))
          (should (equal where 12)))
        (with-temp-buffer
          (fundamental-mode)
          (set-window-buffer (selected-window) (current-buffer))
          (org-moor-protocol '(:url "https://x.com/a"))
          (should (eq where 'picked)))))))

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

(ert-deftest org-moor-test-take-file-on-heading-line ()
  "Taken on a heading's line, the link goes there too, as it is typed."
  (org-moor-test--with-notes "* Topic\n"
    (let ((orig (expand-file-name "a.txt" dir))
          (org-moor-move-directories nil))
      (with-temp-file orig (insert "a"))
      (goto-char (point-min))
      (end-of-line)
      (insert " ")
      (org-moor-test--titles-as-given
        (let ((id (org-moor-test--id-at (org-moor-take-file orig))))
          (should (equal (buffer-string) (format "* Topic [[moor:%s][a]]\n" id)))
          (should (eolp)))))))

(ert-deftest org-moor-test-a-browser-downloads-what-it-is-told-to ()
  "The browser opens the download, and the replica is looked for where it
downloads to; with nothing told, Emacs fetches."
  (let ((opened nil) (looked nil) (fetched nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (push u opened)))
              ((symbol-function 'org-moor-arriving)
               (lambda (dir _since)
                 (push dir looked)
                 (lambda (_url _into done) (funcall done "a.pptx"))))
              ((symbol-function 'org-moor-fetch)
               (lambda (url _dir done) (push url fetched) (funcall done nil))))
      (let ((org-moor-browser-download-function
             (lambda (url done)
               (funcall done (and (string-match-p "files" url)
                                  (list "https://files.example.com/get?id=1" "/dl"))))))
        (org-moor-browser-or-fetch "https://files.example.com/view?id=1" "/s"
                                   (lambda (name) (should (equal name "a.pptx"))))
        (should (equal opened '("https://files.example.com/get?id=1")))
        (should (equal looked '("/dl")))
        (org-moor-browser-or-fetch "https://example.com/a" "/s" #'ignore)
        (should (equal fetched '("https://example.com/a"))))
      (let ((org-moor-browser-download-function nil))
        (org-moor-browser-or-fetch "https://files.example.com/view?id=2" "/s" #'ignore)
        (should (equal fetched '("https://files.example.com/view?id=2"
                                 "https://example.com/a")))
        (should (equal (length opened) 1))))))

(ert-deftest org-moor-test-a-failed-lookup-still-ends ()
  "When taking the replica fails after the lookup, DONE is still called."
  (let ((result 'unset) (org-moor--browser-queue nil) (org-moor--browser-busy nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (&rest _) (error "No browser"))))
      (let ((org-moor-browser-download-function
             (lambda (_url done) (funcall done (list "https://x/get" "/dl")))))
        (org-moor-browser-or-fetch "https://x/view" "/s" (lambda (n) (setq result n)))
        (should-not result)
        (should-not org-moor--browser-busy)))))

(ert-deftest org-moor-test-browser-downloads-one-at-a-time ()
  "The next download is opened only once the one before has come."
  (let ((opened nil) (waiting nil) (got nil)
        (org-moor--browser-queue nil) (org-moor--browser-busy nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (push u opened)))
              ((symbol-function 'org-moor-arriving)
               (lambda (_dir _since)
                 (lambda (_url _into done) (setq waiting done)))))
      (let ((org-moor-browser-download-function
             (lambda (url done) (funcall done (list (concat url "/get") "/dl")))))
        (org-moor-browser-or-fetch "https://x/1" "/s" (lambda (n) (push n got)))
        (org-moor-browser-or-fetch "https://x/2" "/s" (lambda (n) (push n got)))
        (should (equal opened '("https://x/1/get")))
        (funcall waiting "one.pdf")
        (should (equal opened '("https://x/2/get" "https://x/1/get")))
        (funcall waiting "two.pdf")
        (should (equal got '("two.pdf" "one.pdf")))
        (should-not org-moor--browser-busy)))))

(ert-deftest org-moor-test-front-download-that-cannot-start ()
  "When PowerShell cannot be started, the URL is fetched."
  (let ((got 'unset)
        (org-moor-script-directory (make-temp-file "moor" t)))
    (with-temp-file (expand-file-name "org-moor-front.ps1" org-moor-script-directory))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (signal 'file-missing '("Searching" "powershell.exe")))))
      (org-moor-front-download "https://x/view" (lambda (how) (setq got how)))
      (should (null got)))
    (delete-directory org-moor-script-directory t)))

(ert-deftest org-moor-test-front-download-asks-in-the-background ()
  "The script is run as a process; its two lines come back to DONE."
  (let ((got 'unset)
        (org-moor-script-directory (make-temp-file "moor" t)))
    (with-temp-file (expand-file-name "org-moor-front.ps1" org-moor-script-directory))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (should (member "-Resolve" (plist-get args :command)))
                 ;; A real process, done, rather than more primitives
                 ;; redefined: with native compilation, each would need a
                 ;; trampoline compiled while `make-process' is not itself.
                 (let ((proc (make-pipe-process :name "org-moor-test" :noquery t)))
                   (delete-process proc)
                   (with-current-buffer (plist-get args :buffer)
                     (insert "https://x/get\r\nC:\\dl\r\n"))
                   (funcall (plist-get args :sentinel) proc "finished\n")))))
      (org-moor-front-download "https://x/view" (lambda (how) (setq got how)))
      (should (equal got '("https://x/get" "C:\\dl"))))))

(ert-deftest org-moor-test-protocol-does-not-look-up-again ()
  "A URL from outside has been looked up there: Emacs fetches it."
  (let ((asked nil) (keep nil))
    (cl-letf (((symbol-function 'select-frame-set-input-focus) #'ignore)
              ((symbol-function 'org-moor-take-url)
               (lambda (_url _title _pick k) (setq keep k))))
      (let ((org-moor-browser-download-function (lambda (_) (setq asked t) nil)))
        (org-moor-protocol '(:url "https://x.com/a"))
        (should (eq keep #'org-moor-fetch))
        (should-not asked)))))

(ert-deftest org-moor-test-agent-take-returns-the-link-at-once ()
  (let* ((dir (make-temp-file "moor" t))
         (file (expand-file-name "a.txt" dir))
         (fetched nil))
    (with-temp-file file (insert "a"))
    (cl-letf (((symbol-function 'org-moor-fetch-replica)
               (lambda (record &rest _) (setq fetched (plist-get record :origin)))))
      (should (string-match-p "\\`\\[\\[moor:[^]]+\\]\\[Page\\]\\]\\'"
                              (org-moor-agent-take dir "https://x.com/p" "Page")))
      (should (equal fetched "https://x.com/p"))
      (should (string-match-p "\\[a\\]\\]\\'" (org-moor-agent-take dir file))))
    (should-error (org-moor-agent-take dir (expand-file-name "none" dir)))
    (delete-directory dir t)))

(ert-deftest org-moor-test-agent-take-opens-no-browser ()
  "What only a download in the browser keeps has its origin alone when an
agent takes it; a page single-file saves is saved."
  (let ((opened nil) (saved nil) (got nil)
        (org-moor--browser-queue nil) (org-moor--browser-busy nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (push u opened)))
              ((symbol-function 'org-moor-singlefile-keep)
               (lambda (url _dir done) (push url saved) (funcall done "page.html"))))
      (let ((org-moor-browser-download-function
             (lambda (url done)
               (funcall done (if (string-match-p "files" url)
                                 (list "https://files.example.com/get" "/dl")
                               'singlefile)))))
        (org-moor--unseen-keep "https://files.example.com/view" "/s" (lambda (n) (push n got)))
        (org-moor--unseen-keep "https://wiki.example.com/p" "/s" (lambda (n) (push n got)))
        (should-not opened)
        (should (equal saved '("https://wiki.example.com/p")))
        (should (equal got '("page.html" later)))))))

(ert-deftest org-moor-test-singlefile-is-told-by-the-script ()
  "An entry that has single-file save it is answered `singlefile', and the
page is saved by it, with no browser opened."
  (let ((got 'unset) (saved nil) (opened nil)
        (org-moor-script-directory (make-temp-file "moor" t)))
    (with-temp-file (expand-file-name "org-moor-front.ps1" org-moor-script-directory))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((proc (make-pipe-process :name "org-moor-test" :noquery t)))
                   (delete-process proc)
                   (with-current-buffer (plist-get args :buffer)
                     (insert "https://wiki.example.com/p\r\nsinglefile\r\n"))
                   (funcall (plist-get args :sentinel) proc "finished\n")))))
      (org-moor-front-download "https://wiki.example.com/p" (lambda (how) (setq got how))))
    (should (eq got 'singlefile))
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (push u opened)))
              ((symbol-function 'org-moor-singlefile-keep)
               (lambda (url _dir done) (push url saved) (funcall done "page.html"))))
      (let ((org-moor-browser-download-function (lambda (_url done) (funcall done 'singlefile))))
        (org-moor-browser-or-fetch "https://wiki.example.com/p" "/s" #'ignore)))
    (should (equal saved '("https://wiki.example.com/p")))
    (should-not opened)
    (delete-directory org-moor-script-directory t)))

(defun org-moor-test--fake-singlefile (dir location)
  "Write a single-file into DIR that saves a page found at LOCATION.
It writes its arguments, one a line, to DIR/args.  LOCATION nil fails."
  (let ((program (expand-file-name "single-file" dir)))
    (with-temp-file program
      (insert "#!/bin/sh\n"
              "for a in \"$@\"; do echo \"$a\"; done > " (shell-quote-argument (expand-file-name "args" dir)) "\n"
              (if location
                  (concat "for out; do :; done\n"
                          "printf '<html><head><meta name=org-moor-location content=\"%s\"></head></html>' "
                          (shell-quote-argument location) " > \"$out\"\n")
                "exit 1\n")))
    (set-file-modes program #o755)
    program))

(defun org-moor-test--keep-by-singlefile (url dir)
  "Keep URL into DIR by `org-moor-singlefile-keep'; return what DONE got."
  (let ((got 'unset))
    (org-moor-singlefile-keep url dir (lambda (n) (setq got n)))
    (with-timeout (10 (error "single-file did not end"))
      (while (eq got 'unset) (accept-process-output nil 0.05)))
    got))

(ert-deftest org-moor-test-singlefile-saves-the-page ()
  "single-file is given the browser, the profile, the script and the page;
a page that stays on its host is the replica, and one on another a sign-in."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (let* ((dir (file-name-as-directory (make-temp-file "moor" t)))
         (into (file-name-as-directory (expand-file-name "s" dir)))
         (org-moor-singlefile-browser "/opt/chrome")
         (org-moor-singlefile-browser-idle nil)
         (org-moor-singlefile-profile (expand-file-name "profile" dir))
         (org-moor-signed-out-function #'org-moor-signed-out-by-host))
    (make-directory into)
    (let ((org-moor-singlefile-program
           (org-moor-test--fake-singlefile dir "https://wiki.example.com/p?a=1&amp;b=2")))
      (should (equal (org-moor-test--keep-by-singlefile "https://wiki.example.com/p" into)
                     "page.html"))
      (should (file-exists-p (expand-file-name "page.html" into)))
      (let ((args (with-temp-buffer
                    (insert-file-contents (expand-file-name "args" dir))
                    (split-string (buffer-string) "\n" t))))
        (should (equal (butlast args)
                       (list "--browser-executable-path" "/opt/chrome"
                             "--browser-profile" org-moor-singlefile-profile
                             "--browser-arg=--disable-quic"
                             "--browser-script" org-moor--location-script
                             "--filename-conflict-action" "overwrite"
                             "--browser-wait-until" "DOMContentLoaded"
                             "--browser-wait-until-delay" "0"
                             "--browser-script" org-moor--ready-script
                             "https://wiki.example.com/p")))))
    ;; Given arguments come last, to be taken over these.
    (let ((org-moor-singlefile-arguments '("--browser-wait-until" "InteractiveTime"))
          (org-moor-singlefile-when-ready nil)
          (org-moor-singlefile-program
           (org-moor-test--fake-singlefile dir "https://wiki.example.com/p")))
      (org-moor-test--keep-by-singlefile "https://wiki.example.com/p" into)
      (should (equal (seq-subseq (with-temp-buffer
                                   (insert-file-contents (expand-file-name "args" dir))
                                   (split-string (buffer-string) "\n" t))
                                 7 12)
                     '("--filename-conflict-action" "overwrite"
                       "--browser-wait-until" "InteractiveTime" "https://wiki.example.com/p"))))
    (delete-file (expand-file-name "page.html" into))
    (let ((org-moor-singlefile-program
           (org-moor-test--fake-singlefile dir "https://login.example.com/?r=wiki")))
      (should (eq (org-moor-test--keep-by-singlefile "https://wiki.example.com/p" into)
                  'signin)))
    (let ((org-moor-singlefile-program (org-moor-test--fake-singlefile dir nil)))
      (should-not (org-moor-test--keep-by-singlefile "https://wiki.example.com/p" into)))
    (should-not (directory-files into nil "\\`[^.]"))
    (should-not (directory-files into nil "\\`\\.page"))
    (delete-directory dir t)))

(ert-deftest org-moor-test-singlefile-location ()
  "Where the page was is read back, quoted or not."
  (let ((file (make-temp-file "moor" nil ".html")))
    (with-temp-file file
      (insert "<head><meta name=org-moor-location content=https://a.example/p></head>"))
    (should (equal (org-moor-singlefile-location file) "https://a.example/p"))
    (let ((coding-system-for-write 'utf-8))
      (with-temp-file file
        (insert "<head><meta name=\"org-moor-location\" content=\"https://a.example/資料?a=1&amp;b=2\"></head>")))
    (should (equal (org-moor-singlefile-location file) "https://a.example/資料?a=1&b=2"))
    (with-temp-file file (insert "<head></head>"))
    (should-not (org-moor-singlefile-location file))
    (should-not (org-moor-signed-out-p "https://a.example/p" nil))
    (should-not (org-moor-signed-out-p "https://a.example/p" "https://a.example/login"))
    (should (org-moor-signed-out-p "https://a.example/p" "https://login.example/"))
    (delete-file file)))

(defmacro org-moor-test--with-kept-browser (&rest body)
  "Run BODY with a browser kept for single-file that is a pipe, answering.
`browser' is it; `runs' has the server each page was saved in, latest first."
  (declare (indent 0))
  `(let* ((browser (make-pipe-process :name "org-moor-test-browser" :noquery t))
          (org-moor--singlefile-server browser)
          (org-moor--singlefile-server-url "http://127.0.0.1:9222")
          (org-moor--singlefile-server-waiting nil)
          (org-moor--singlefile-server-queue nil)
          (org-moor--singlefile-server-busy nil)
          (org-moor--signing-in nil)
          (org-moor--singlefile-server-timer nil)
          (org-moor-singlefile-browser "/opt/chrome")
          (org-moor-singlefile-browser-idle 600)
          (org-moor-signed-out-function #'org-moor-signed-out-by-host)
          (runs nil))
     (unwind-protect
         (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/single-file")))
           ,@body)
       (when (timerp org-moor--singlefile-server-timer)
         (cancel-timer org-moor--singlefile-server-timer))
       (when (process-live-p browser) (delete-process browser)))))

(ert-deftest org-moor-test-singlefile-in-the-kept-browser ()
  "A page is saved in the browser kept for single-file, which is quit once
no page has been saved in it for a while."
  (org-moor-test--with-kept-browser
    (let ((got 'unset))
      (cl-letf (((symbol-function 'org-moor--singlefile-run)
                 (lambda (_url _file server done) (push server runs) (funcall done t)))
                ((symbol-function 'org-moor-singlefile-location) #'ignore))
        (org-moor-singlefile "https://wiki.example.com/p" "/s/p.html"
                             (lambda (saved) (setq got saved))))
      (should (eq got t))
      (should (equal runs '("http://127.0.0.1:9222")))
      (should (timerp org-moor--singlefile-server-timer))
      ;; While a page is saved, the browser stays when its time comes.
      (setq org-moor--singlefile-server-busy t)
      (funcall (timer--function org-moor--singlefile-server-timer))
      (should (process-live-p browser))
      (setq org-moor--singlefile-server-busy nil)
      (org-moor--singlefile-server-rest)
      (funcall (timer--function org-moor--singlefile-server-timer))
      (should-not (process-live-p browser))
      (should-not org-moor--singlefile-server)
      (should-not org-moor--singlefile-server-url))))

(ert-deftest org-moor-test-singlefile-one-page-at-a-time ()
  "Pages given at once are saved in the kept browser one after another,
in the order given; while the profile is open to sign in, they wait."
  (org-moor-test--with-kept-browser
    (let ((pending nil) (got nil))
      (cl-letf (((symbol-function 'org-moor--singlefile-run)
                 (lambda (url _file server done)
                   (push (cons url server) runs)
                   (push done pending)))
                ((symbol-function 'org-moor-singlefile-location) #'ignore))
        (dolist (u '("https://w.example/a" "https://w.example/b"))
          (org-moor-singlefile u "/s/p.html" (lambda (s) (push (cons u s) got))))
        (should (equal (mapcar #'car runs) '("https://w.example/a")))
        (should (= (length org-moor--singlefile-server-queue) 1))
        (setq org-moor--signing-in t)
        (funcall (pop pending) t)
        (should (equal got '(("https://w.example/a" . t))))
        (should (equal (mapcar #'car runs) '("https://w.example/a")))
        (org-moor--signed-in)
        (should (equal (mapcar #'car runs) '("https://w.example/b" "https://w.example/a")))
        (funcall (pop pending) t)
        (should (equal (mapcar #'car got) '("https://w.example/b" "https://w.example/a")))
        (should-not org-moor--singlefile-server-queue)
        (should-not org-moor--singlefile-server-busy)))))

(ert-deftest org-moor-test-singlefile-again-when-the-browser-went ()
  "A page not saved because the kept browser went is saved once more, in
the browser started again; one not saved while the browser stays is not
tried again."
  (org-moor-test--with-kept-browser
    (let ((got 'unset))
      (cl-letf (((symbol-function 'org-moor--singlefile-run)
                 (lambda (_url _file server done)
                   (push server runs)
                   (when (process-live-p browser) (delete-process browser))
                   (funcall done (equal server "http://127.0.0.1:9333"))))
                ((symbol-function 'org-moor--singlefile-server)
                 (lambda (then)
                   (funcall then (if runs "http://127.0.0.1:9333" "http://127.0.0.1:9222"))))
                ((symbol-function 'org-moor-singlefile-location) #'ignore))
        (org-moor-singlefile "https://wiki.example.com/p" "/s/p.html"
                             (lambda (saved) (setq got saved))))
      (should (eq got t))
      (should (equal runs '("http://127.0.0.1:9333" "http://127.0.0.1:9222")))))
  ;; Gone twice, it is not tried a third time.
  (org-moor-test--with-kept-browser
    (let ((got 'unset))
      (cl-letf (((symbol-function 'org-moor--singlefile-run)
                 (lambda (_url _file server done)
                   (push server runs)
                   (funcall done nil)))
                ((symbol-function 'org-moor--singlefile-server)
                 (lambda (then)
                   (when (process-live-p browser) (delete-process browser))
                   (funcall then "http://127.0.0.1:9222"))))
        (org-moor-singlefile "https://wiki.example.com/p" "/s/p.html"
                             (lambda (saved) (setq got saved))))
      (should-not got)
      (should (= (length runs) 2))
      (should-not org-moor--singlefile-server-queue)
      (should-not org-moor--singlefile-server-busy)))
  (org-moor-test--with-kept-browser
    (let ((got 'unset))
      (cl-letf (((symbol-function 'org-moor--singlefile-run)
                 (lambda (_url _file server done) (push server runs) (funcall done nil))))
        (org-moor-singlefile "https://wiki.example.com/p" "/s/p.html"
                             (lambda (saved) (setq got saved))))
      (should-not got)
      (should (equal runs '("http://127.0.0.1:9222"))))))

(ert-deftest org-moor-test-singlefile-kept-browser-starts ()
  "The browser is started in the profile, with no window, and single-file
is pointed at the port it writes."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (let* ((dir (file-name-as-directory (make-temp-file "moor" t)))
         (program (expand-file-name "chrome" dir))
         (org-moor-singlefile-browser program)
         (org-moor-singlefile-browser-idle 600)
         (org-moor-singlefile-profile (expand-file-name "profile" dir))
         (org-moor--singlefile-server nil)
         (org-moor--singlefile-server-url nil)
         (org-moor--singlefile-server-waiting nil)
         (got 'unset))
    (with-temp-file program
      (insert "#!/bin/sh\n"
              "for a in \"$@\"; do echo \"$a\"; done > " (shell-quote-argument (expand-file-name "args" dir)) "\n"
              "printf '4321\\n/devtools/browser/x\\n' > "
              (shell-quote-argument (expand-file-name "profile/DevToolsActivePort" dir)) "\n"
              "exec sleep 30\n"))
    (set-file-modes program #o755)
    (unwind-protect
        (progn
          (org-moor--singlefile-server (lambda (url) (setq got url)))
          (with-timeout (10 (error "the browser did not answer"))
            (while (eq got 'unset) (accept-process-output nil 0.05)))
          (should (equal got "http://127.0.0.1:4321"))
          (let ((args (with-temp-buffer
                        (insert-file-contents (expand-file-name "args" dir))
                        (split-string (buffer-string) "\n" t))))
            (should (equal (car args) (concat "--user-data-dir="
                                              (directory-file-name org-moor-singlefile-profile))))
            (should (member "--headless" args))
            (should (member "--disable-quic" args))
            (should-not (member "--no-sandbox" args))))
      (org-moor--singlefile-server-stop)
      (delete-directory dir t))
    (should-not org-moor--singlefile-server)))

(ert-deftest org-moor-test-profile-in-use ()
  "A profile is in use while the browser its SingletonLock names runs."
  (skip-unless (not (memq system-type '(windows-nt ms-dos))))
  (let* ((dir (make-temp-file "moor" t))
         (lock (expand-file-name "SingletonLock" dir)))
    (unwind-protect
        (progn
          (should-not (org-moor--profile-in-use-p dir))
          (make-symbolic-link (format "host-%d" (emacs-pid)) lock)
          (should (org-moor--profile-in-use-p dir))
          (delete-file lock)
          ;; Left behind by a browser that is gone.
          (make-symbolic-link "host-99999999" lock)
          (should-not (org-moor--profile-in-use-p dir)))
      (delete-directory dir t))))

(ert-deftest org-moor-test-singlefile-kept-browser-left-running ()
  "With the profile open in a browser left running, none is started: pages
are saved with a copy, and signing in is refused."
  (let* ((dir (file-name-as-directory (make-temp-file "moor" t)))
         (org-moor-singlefile-browser "/opt/chrome")
         (org-moor-singlefile-browser-idle 600)
         (org-moor-singlefile-profile (expand-file-name "profile" dir))
         (org-moor--singlefile-server nil)
         (org-moor--singlefile-server-url nil)
         (org-moor--singlefile-server-waiting nil)
         (org-moor--signing-in nil)
         (started nil)
         (got 'unset))
    (unwind-protect
        (cl-letf (((symbol-function 'org-moor--profile-in-use-p) (lambda (_) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest _) (setq started t) nil)))
          (org-moor--singlefile-server (lambda (url) (setq got url)))
          (should-not got)
          (should-not started)
          (should-error (org-moor-sign-in "https://wiki.example.com/p") :type 'user-error)
          (should-not started)
          (should-not org-moor--signing-in))
      (delete-directory dir t))))

(ert-deftest org-moor-test-singlefile-run-that-cannot-start ()
  "A single-file that cannot be started is a page not saved.
On Windows it is not started at all; elsewhere it is, and exits at once."
  (let ((org-moor-singlefile-program "/no/such/single-file")
        (got 'unset))
    (org-moor--singlefile-run "https://wiki.example.com/p" "/s/p.html" nil
                              (lambda (saved) (setq got saved)))
    (with-timeout (10 (error "single-file was not given up"))
      (while (eq got 'unset) (accept-process-output nil 0.05)))
    (should-not got)))

(ert-deftest org-moor-test-singlefile-run-is-stopped ()
  "single-file is stopped once it has had its time, and its page is not kept."
  (let* ((file (make-temp-file "moor" nil ".html"))
         (org-moor-singlefile-timeout 1)
         (org-moor-singlefile-program
          (expand-file-name invocation-name invocation-directory))
         (org-moor-singlefile-arguments nil)
         (org-moor-singlefile-profile nil)
         (got 'unset))
    ;; Emacs stands in for single-file: it takes the arguments and waits.
    (cl-letf (((symbol-function 'org-moor--singlefile-browser-args)
               (lambda () (list "-Q" "--batch" "--eval" "(sleep-for 30)"))))
      (let ((org-moor--location-script "--"))
        (org-moor--singlefile-run "https://wiki.example.com/p" file nil
                                  (lambda (saved) (setq got saved)))))
    (with-timeout (15 (error "single-file was not stopped"))
      (while (eq got 'unset) (accept-process-output nil 0.05)))
    (should-not got)
    (should-not (file-exists-p file))))

(ert-deftest org-moor-test-front-signed-out ()
  "A page is one of the site while its entry still has single-file save it;
the URL asked for itself is not asked about again."
  (let ((asked nil) (got nil))
    (cl-letf (((symbol-function 'org-moor-front-download)
               (lambda (url done)
                 (push url asked)
                 (funcall done (and (string-match-p "/wiki/" url) 'singlefile)))))
      (org-moor-front-signed-out "https://w.example/wiki/a" "https://w.example/wiki/a"
                                 (lambda (out) (push out got)))
      (should-not asked)
      (org-moor-front-signed-out "https://w.example/wiki/a" "https://w.example/wiki/a/"
                                 (lambda (out) (push out got)))
      (org-moor-front-signed-out "https://w.example/wiki/a" "https://w.example/login?r=a"
                                 (lambda (out) (push out got)))
      (should (equal asked '("https://w.example/login?r=a" "https://w.example/wiki/a/")))
      (should (equal got '(t nil nil))))))

(ert-deftest org-moor-test-a-sign-in-page-waits-for-a-sign-in ()
  "A source shown a sign-in page keeps its host, until its replica is kept."
  (org-moor-test--with-notes ""
    (let ((record (org-moor-new-source "Wiki" "https://wiki.example.com/p" 'web)))
      (org-moor-fetch-replica record (lambda (_url _dir done) (funcall done 'signin)))
      (should (equal (plist-get (org-moor-record (plist-get record :id)) :signin)
                     "wiki.example.com"))
      (should-not (gethash (plist-get record :id) org-moor--keeping))
      (let ((record (org-moor-record (plist-get record :id))))
        (org-moor-fetch-replica record
                                (lambda (_url dir done)
                                  (with-temp-file (expand-file-name "page.html" dir))
                                  (funcall done "page.html")))
        (let ((now (org-moor-record (plist-get record :id))))
          (should (plist-get now :replica))
          (should-not (plist-get now :signin)))))))

(ert-deftest org-moor-test-protocol-has-singlefile-keep ()
  "A page from outside that single-file saves is kept by it."
  (let ((keep nil))
    (cl-letf (((symbol-function 'select-frame-set-input-focus) #'ignore)
              ((symbol-function 'org-moor-take-url)
               (lambda (_url _title _pick k) (setq keep k))))
      (org-moor-protocol '(:url "https://wiki.example.com/p" :keep "singlefile"))
      (should (eq keep #'org-moor--singlefile-or-fetch)))))

(ert-deftest org-moor-test-singlefile-leaves-a-file-to-fetch ()
  "A URL single-file would save that is spelled as a file is fetched as one."
  (let ((fetched nil) (saved nil))
    (cl-letf (((symbol-function 'org-moor-fetch)
               (lambda (url _dir _done &optional file) (push (list url file) fetched)))
              ((symbol-function 'org-moor-singlefile-keep)
               (lambda (url _dir _done) (push url saved))))
      (org-moor--singlefile-or-fetch "https://wiki.example.com/a/b.pdf" "/s" #'ignore)
      (org-moor--singlefile-or-fetch "https://wiki.example.com/a/b.html" "/s" #'ignore)
      (org-moor--singlefile-or-fetch "https://wiki.example.com/p" "/s" #'ignore))
    (should (equal fetched '(("https://wiki.example.com/a/b.pdf" t))))
    (should (equal saved '("https://wiki.example.com/p" "https://wiki.example.com/a/b.html")))))

(ert-deftest org-moor-test-download-fetched-in-browser ()
  "With the browser kept for single-file, a download is fetched by it, for
an agent too, and no browser is opened."
  (let ((opened nil) (fetched nil) (got nil)
        (org-moor--browser-queue nil) (org-moor--browser-busy nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (push u opened)))
              ((symbol-function 'org-moor--browser-fetch-p) (lambda () t))
              ((symbol-function 'org-moor-fetch)
               (lambda (url _dir done &optional file)
                 (push (list url file) fetched) (funcall done "a.pdf"))))
      (let ((org-moor-browser-download-function
             (lambda (_url done) (funcall done (list "https://files.example.com/get?id=1" "/dl")))))
        (org-moor-browser-or-fetch "https://files.example.com/view?id=1" "/s"
                                   (lambda (n) (push n got)))
        (org-moor--unseen-keep "https://files.example.com/view?id=1" "/s"
                               (lambda (n) (push n got)))))
    (should-not opened)
    (should (equal fetched (make-list 2 '("https://files.example.com/get?id=1" t))))
    (should (equal got '("a.pdf" "a.pdf")))))

(ert-deftest org-moor-test-fetch-in-browser ()
  "What the browser fetches is kept as it says: a file under the name it was
sent with; a page sent for a file is a sign-in page; a page is saved by
single-file; a browser that cannot be started leaves it to be fetched here."
  (let* ((dir (make-temp-file "moor" t))
         (org-moor-snapshot-function nil)
         (answer nil) (saved nil) (here nil) (got nil))
    (cl-letf (((symbol-function 'org-moor--browser-fetch-p) (lambda () t))
              ((symbol-function 'executable-find) #'identity)
              ((symbol-function 'org-moor--browser-fetch)
               (lambda (_url file then)
                 (when (eq (car-safe answer) 'file)
                   (with-temp-file file (insert "%PDF-1.4")))
                 (funcall then answer)))
              ((symbol-function 'org-moor-singlefile-keep)
               (lambda (url _dir done) (push url saved) (funcall done "page.html")))
              ((symbol-function 'org-moor--fetch-here)
               (lambda (url _dir done &optional _file) (push url here) (funcall done nil))))
      (setq answer '(file "200" "application/pdf" "attachment; filename=\"r.pdf\""))
      (org-moor-fetch "https://x.com/get?id=1" dir (lambda (n) (push n got)) t)
      (setq answer '(page "https://login.x.com/"))
      (org-moor-fetch "https://x.com/get?id=1" dir (lambda (n) (push n got)) t)
      (org-moor-fetch "https://x.com/p" dir (lambda (n) (push n got)))
      (setq answer 'here)
      (org-moor-fetch "https://x.com/q" dir (lambda (n) (push n got))))
    (should (equal (reverse got) '("r.pdf" signin "page.html" nil)))
    (should (equal saved '("https://x.com/p")))
    (should (equal here '("https://x.com/q")))
    (should (equal (directory-files dir nil "\\`[^.]") '("r.pdf")))
    (delete-directory dir t)))

(ert-deftest org-moor-test-browser-fetch-waits-for-sign-in ()
  "A URL fetched while the profile is open to sign in is fetched once it is quit."
  (let ((org-moor--signing-in t) (org-moor--browser-fetch-waiting nil)
        (org-moor--singlefile-server-queue nil) (got nil))
    (cl-letf (((symbol-function 'org-moor--singlefile-server)
               (lambda (then) (funcall then nil))))
      (org-moor--browser-fetch "https://x.com/a" "/f" (lambda (how) (push how got)))
      (should-not got)
      (org-moor--signed-in)
      (should (equal got '(here)))
      (should-not org-moor--browser-fetch-waiting))))

(ert-deftest org-moor-test-sign-in ()
  "The browser is opened in the profile at the URL; once it is quit, the
sources waiting for that host are offered to be kept."
  (org-moor-test--with-notes ""
    (let* ((waiting (org-moor-new-source "Wiki" "https://wiki.example.com/p" 'web))
           (other (org-moor-new-source "Other" "https://other.example.com/p" 'web))
           (org-moor-singlefile-profile (expand-file-name "profile" dir))
           (org-moor-singlefile-browser nil)
           (org-moor--signing-in nil)
           (command nil) (kept nil))
      (org-moor-record-set waiting :signin "wiki.example.com")
      (org-moor-record-set other :signin "other.example.com")
      (make-directory (expand-file-name "Default/Cache" org-moor-singlefile-profile) t)
      (make-directory (expand-file-name "Default/Network" org-moor-singlefile-profile) t)
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/single-file"))
                ((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq command (plist-get args :command))
                   (let ((proc (make-pipe-process :name "org-moor-test" :noquery t)))
                     (delete-process proc)
                     (funcall (plist-get args :sentinel) proc "finished\n"))))
                ((symbol-function 'process-exit-status) (lambda (_) 0))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'org-moor-keep-replica)
                 (lambda (records) (setq kept (mapcar (lambda (r) (plist-get r :title)) records)))))
        (org-moor-sign-in "https://wiki.example.com/p"))
      (should (equal command (list "single-file" "--create-browser-profile"
                                   org-moor-singlefile-profile "https://wiki.example.com/p")))
      (should (equal kept '("Wiki")))
      ;; Quit, the profile is free for pages again.
      (should-not org-moor--signing-in)
      (should-not (file-exists-p (expand-file-name "Default/Cache" org-moor-singlefile-profile)))
      (should (file-exists-p (expand-file-name "Default/Network" org-moor-singlefile-profile)))
      ;; With the browser named, it is started as it is, its sandbox kept.
      (let ((org-moor-singlefile-browser "/opt/chrome"))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args) (setq command (plist-get args :command)) nil)))
          (org-moor-sign-in "https://wiki.example.com/p")))
      (should (equal command (list (expand-file-name "/opt/chrome")
                                   (concat "--user-data-dir="
                                           (directory-file-name org-moor-singlefile-profile))
                                   "--no-first-run" "--no-default-browser-check"
                                   "--disable-quic"
                                   "https://wiki.example.com/p"))))))

(ert-deftest org-moor-test-batch-command-by-the-server ()
  "While the server runs, the command names it; the agent fills in the same."
  ;; As the sources are named on this system: c\:/tmp/s on Windows.
  (let ((sources (regexp-quote (shell-quote-argument (expand-file-name "/tmp/s") t))))
    (cl-letf (((symbol-function 'org-moor--server-file) (lambda () "/run/emacs/server")))
      (should (string-match-p
               (concat "-f org-moor-batch-take --server=/run/emacs/server "
                       sources " FILE-OR-URL TITLE\\'")
               (org-moor-batch-command "/tmp/s/"))))
    (cl-letf (((symbol-function 'org-moor--server-file) #'ignore))
      (should (string-match-p (concat "-f org-moor-batch-take " sources " FILE-OR-URL TITLE\\'")
                              (org-moor-batch-command "/tmp/s/"))))))

(ert-deftest org-moor-test-batch-take-by-the-server ()
  "The server's Emacs takes it, and is asked until its replica is kept;
when it cannot be reached, it is taken here."
  (let* ((dir (make-temp-file "moor" t))
         (file (expand-file-name "a.txt" dir))
         (asked nil)
         (states (list '(pending) '(pending) '(kept "/s/ab/cd/page.html"))))
    (with-temp-file file (insert "a"))
    (cl-letf (((symbol-function 'org-moor--ask-server)
               (lambda (server form)
                 (push (list server form) asked)
                 (if (eq (car form) 'org-moor-agent-take) "[[moor:abcd][a]]" (pop states))))
              ((symbol-function 'sleep-for) #'ignore))
      (let ((command-line-args-left (list "--server=/run/s" dir "https://x.com/a")))
        (should (equal (with-output-to-string (org-moor-batch-take))
                       "[[moor:abcd][a]]\nReplica: /s/ab/cd/page.html\n"))
        (should (equal (car (last asked))
                       `("/run/s" (org-moor-agent-take ,dir "https://x.com/a" nil))))
        (should (equal (car asked) `("/run/s" (org-moor-agent-state ,dir "abcd"))))
        (should (= (length asked) 4))))
    (cl-letf (((symbol-function 'org-moor--ask-server)
               (lambda (&rest _) (signal 'file-error '("Cannot reach Emacs")))))
      (let ((command-line-args-left (list "--server=/run/s" dir file)))
        (should (string-match-p "\\`\\[\\[moor:[^]]+\\]\\[a\\]\\]\nReplica: .*a\\.txt\n\\'"
                                (with-output-to-string (org-moor-batch-take))))))
    (delete-directory dir t)))

(ert-deftest org-moor-test-batch-take-stops-waiting ()
  "A replica still being kept after `org-moor-agent-timeout' is said so."
  (let ((org-moor-agent-timeout 0))
    (cl-letf (((symbol-function 'org-moor--ask-server)
               (lambda (_server form)
                 (if (eq (car form) 'org-moor-agent-take) "[[moor:abcd][a]]" '(pending))))
              ((symbol-function 'sleep-for) #'ignore))
      (let ((command-line-args-left (list "--server=/run/s" "/s" "https://x.com/a")))
        (should (string-match-p "\nNo replica yet: "
                                (with-output-to-string (org-moor-batch-take))))))))

(ert-deftest org-moor-test-agent-state ()
  "What a source has is told an agent: its replica, a sign-in, its origin."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Wiki" "https://w.example/p" 'web))
           (id (plist-get record :id))
           (sources org-moor-sources-directory))
      (should (equal (org-moor-agent-state sources id) '(origin "https://w.example/p")))
      (should (string-prefix-p "No replica: only where"
                               (org-moor-agent-state-line (org-moor-agent-state sources id))))
      (org-moor-record-set record :signin "login.example")
      (should (equal (org-moor-agent-state sources id) '(signin "login.example")))
      (should (string-match-p "login\\.example showed a sign-in page"
                              (org-moor-agent-state-line '(signin "login.example"))))
      (puthash id t org-moor--keeping)
      (unwind-protect (should (equal (org-moor-agent-state sources id) '(pending)))
        (remhash id org-moor--keeping))
      (with-temp-file (expand-file-name "page.html" (plist-get record :dir)))
      (org-moor-record-set record :file "page.html")
      (should (equal (org-moor-agent-state sources id)
                     (list 'kept (expand-file-name "page.html" (plist-get record :dir))))))))

(ert-deftest org-moor-test-batch-take-prints-utf-8 ()
  "The link is printed in UTF-8, whatever the system's code page."
  (let ((coding 'unset))
    (cl-letf (((symbol-function 'org-moor--ask-server)
               (lambda (_server form)
                 (if (eq (car form) 'org-moor-agent-take) "[[moor:x][題]]" '(kept "/s/題.html"))))
              ((symbol-function 'princ)
               (lambda (&rest _) (setq coding coding-system-for-write))))
      (let ((command-line-args-left (list "--server=/run/s" "/s" "https://x.com/")))
        (org-moor-batch-take)))
    (should (eq coding 'utf-8))))

(ert-deftest org-moor-test-programs-given-arguments-they-read ()
  "curl and monolith are given their arguments in the system's code page,
a URL escaped to ASCII, and read in UTF-8."
  (let ((org-moor-cookie-file nil) (calls nil))
    (cl-letf (((symbol-function 'executable-find) #'identity)
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (push (cons (plist-get args :coding) (car (last (plist-get args :command))))
                       calls)
                 nil)))
      (org-moor--curl "https://x.com/資料" "/tmp/f" #'ignore)
      (org-moor-snapshot-monolith "https://x.com/資料" "/tmp/p" #'ignore))
    (dolist (call calls)
      (should (equal (car call) (cons 'utf-8 (org-moor-argument-coding))))
      (should (equal (cdr call) "https://x.com/%E8%B3%87%E6%96%99"))))
  (let ((system-type 'windows-nt) (locale-coding-system 'cp932))
    (should (eq (org-moor-argument-coding) 'cp932)))
  (let ((system-type 'darwin))
    (should (eq (org-moor-argument-coding) 'utf-8))))

(ert-deftest org-moor-test-ask-server-in-ascii ()
  "A name that is not ASCII goes and comes back whole, whatever the server decodes by."
  (require 'server)
  (cl-letf (((symbol-function 'server-eval-at)
             (lambda (_server form)
               (let ((text (format "%S" form)))
                 (should (string-match-p "\\`[[:ascii:]]*\\'" text))
                 ;; The server decodes what arrives by its coding, cp932 here.
                 (let ((value (eval (read (decode-coding-string text 'cp932)) t)))
                   (should (string-match-p "\\`[[:ascii:]]*\\'" value))
                   value)))))
    (should (equal (org-moor--ask-server "/run/s" '(concat "c:/テスト/" "sources"))
                   "c:/テスト/sources"))
    (should (equal (org-moor--ask-server "/run/s" '(list "[[moor:x][題]]" 1))
                   '("[[moor:x][題]]" 1)))))

(ert-deftest org-moor-test-take-from-outside-into-no-directory ()
  "Sources in a directory that is not there are not made: the name may
have been mangled.  Sources alone may be made, in a project's first take."
  (let* ((dir (make-temp-file "moor" t))
         (file (expand-file-name "a.txt" dir))
         (none (expand-file-name "none/sources" dir)))
    (with-temp-file file (insert "a"))
    (should-error (org-moor-agent-take none file))
    (should-error (org-moor-agent-take none "https://x.com/p" "Page"))
    (should-not (file-exists-p none))
    (delete-directory dir t)))

(ert-deftest org-moor-test-agent-take-says-nothing-over-the-user ()
  "What is said of a source an agent took goes to *Messages* alone, also
when it is said after the agent was answered; the user's own is shown."
  (org-moor-test--with-notes ""
    (let ((said nil) (later nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (cons (apply #'format format args) inhibit-message) said))))
        (let ((org-moor--agent t))
          (org-moor-fetch-replica (org-moor-new-source "Agent's" "https://x.com/a" 'web)
                                  (lambda (_url _dir done) (setq later done))))
        (funcall later nil)
        (org-moor-fetch-replica (org-moor-new-source "Mine" "https://x.com/b" 'web)
                                (lambda (_url _dir done) (funcall done nil))))
      (should (equal (mapcar #'cdr said) '(nil t)))
      (should (string-search "Agent's" (car (nth 1 said)))))))

(ert-deftest org-moor-test-singlefile-the-users-pages-first ()
  "A page the user takes is saved before the agents' pages queued, after
the user's own; an agent's after every other."
  (org-moor-test--with-kept-browser
    (let ((pending nil))
      (cl-letf (((symbol-function 'org-moor--singlefile-run)
                 (lambda (url _file _server done)
                   (push url runs)
                   (push done pending)))
                ((symbol-function 'org-moor-singlefile-location) #'ignore))
        (let ((org-moor--agent t))
          (dolist (u '("https://w.example/a1" "https://w.example/a2"))
            (org-moor-singlefile u "/s/p.html" #'ignore)))
        (dolist (u '("https://w.example/u1" "https://w.example/u2"))
          (org-moor-singlefile u "/s/p.html" #'ignore))
        (let ((org-moor--agent t))
          (org-moor-singlefile "https://w.example/a3" "/s/p.html" #'ignore))
        (while pending (funcall (pop pending) t))
        (should (equal (reverse runs)
                       '("https://w.example/a1" "https://w.example/u1" "https://w.example/u2"
                         "https://w.example/a2" "https://w.example/a3")))))))

(ert-deftest org-moor-test-agent-take-a-url-being-kept-once ()
  "A URL whose replica is being kept in the sources is that source, for
another agent or one asking again; elsewhere, or once kept, a new one."
  (let* ((dir (make-temp-file "moor" t))
         (other (make-temp-file "moor" t))
         (org-moor--keeping (make-hash-table :test #'equal))
         (done nil))
    (cl-letf (((symbol-function 'org-moor--unseen-keep)
               (lambda (_url _dir d) (push d done))))
      (let ((first (org-moor-agent-take dir "https://x.com/p" "Page")))
        (should (equal (org-moor-agent-take dir "https://x.com/p") first))
        (should-not (equal (org-moor-agent-take other "https://x.com/p") first))
        (should (= (length done) 2))
        (dolist (d done) (funcall d nil))
        (should-not (equal (org-moor-agent-take dir "https://x.com/p") first))))
    (delete-directory dir t)
    (delete-directory other t)))

(ert-deftest org-moor-test-batch-take-waits-again-by-id ()
  "moor:ID takes nothing, and waits for that source once more; still
pending, the line says how to wait again."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Wiki" "https://w.example/p" 'web))
           (id (plist-get record :id))
           (sources org-moor-sources-directory)
           (asked nil)
           (states (list '(pending) '(signin "login.example"))))
      (cl-letf (((symbol-function 'org-moor--ask-server)
                 (lambda (_server form) (push form asked) (pop states)))
                ((symbol-function 'sleep-for) #'ignore))
        (let ((command-line-args-left (list "--server=/run/s" sources (concat "moor:" id))))
          (should (equal (with-output-to-string (org-moor-batch-take))
                         (format "[[moor:%s][Wiki]]\nNo replica: login.example showed a sign-in page, which the user signs in to later; do not guess what it says.\n"
                                 id))))
        (should (equal (mapcar #'car asked) '(org-moor-agent-state org-moor-agent-state))))
      (should (string-search (concat "with moor:" id " in place")
                             (org-moor-agent-state-line '(pending) id)))
      ;; With no Emacs to ask, it is read here.
      (let ((command-line-args-left (list sources (concat "moor:" id))))
        (should (string-search "No replica: only where"
                               (with-output-to-string (org-moor-batch-take)))))
      (let ((command-line-args-left (list sources "moor:nosuch")))
        (should-error (org-moor-batch-take))))))

(ert-deftest org-moor-test-take-to-a-copy-being-written ()
  "A heading out in a copy an agent is writing takes nothing: no source
is made, and why is said."
  (org-moor-test--with-notes "* Notes\nText\n"
    (let ((org-dock--busy nil)
          (taken nil)
          (copy (org-dock-heading (point) :display nil)))
      (org-dock-set-busy (buffer-file-name copy) "Claude")
      (let ((err (should-error (org-moor--take-at (point-marker) "T"
                                                  (lambda (_) (setq taken t)))
                               :type 'user-error)))
        (should (string-search "while Claude writes it" (cadr err))))
      (should-not taken)
      (org-dock-set-busy (buffer-file-name copy) nil)
      (should (org-moor--take-at (point-marker) "T"
                                 (lambda (sources)
                                   (setq taken t)
                                   (org-moor-new-source "T" nil 'file sources))))
      (should taken))))
