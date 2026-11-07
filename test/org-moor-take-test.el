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
          (should (equal (plist-get record :replica-via) "chat"))
          (should (equal (plist-get record :origin-url) "https://chat.example/m/1"))
          (should (equal (plist-get record :replica-file) "Chat from Robin.txt"))
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
          (should (equal (plist-get record :replica-file) "Board a b.png"))
          (should (equal (plist-get record :replica-via) "clipboard"))
          (should-not (plist-get record :origin-url))
          (should (plist-get record :replica))))
      (org-moor-test--clipboard "A1\tB1" t
        (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?t)))
          (should (equal (plist-get (org-moor-record
                                     (org-moor-test--id-at (org-moor-take-clipboard)))
                                    :replica-file)
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
          (should (equal (plist-get record :replica-via) "file"))
          (should (equal (plist-get record :origin-url) (org-moor-path-url orig)))
          (should (equal (plist-get record :replica-file) "manual.pdf"))
          (should (string-prefix-p "[" (plist-get record :replica-taken)))
          (should-not (plist-get record :replica-requested))
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

(ert-deftest org-moor-test-front-download-that-cannot-start ()
  "When PowerShell cannot be started, the browser is sent to the URL itself."
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
                                   (org-moor-new-source "T" nil sources))))
      (should taken))))


;;;; The web: nothing fetched, the browser's save waited for

(defmacro org-moor-test--browser (&rest body)
  "Run BODY with `browse-url' noting what it opens in `opened', and no
lookup of where to send it; `downloads' is a directory to wait in.  The
wait is stopped after."
  (declare (indent 0))
  `(let* ((opened nil)
          (downloads (file-name-as-directory (make-temp-file "dl" t)))
          (org-moor-downloads-directory downloads)
          (org-moor-browser-url-function nil)
          (org-moor--waiting nil)
          (global-mode-string nil))
     (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (push u opened))))
       (unwind-protect (progn ,@body)
         (org-moor-stop-waiting)
         (delete-directory downloads t)))))

(defun org-moor-test--arrive (dir name &optional text)
  "Have the file NAME, holding TEXT, arrive in DIR, and be looked at twice.
Twice, since a file is whole once its size stays the same."
  (with-temp-file (expand-file-name name dir) (insert (or text "x")))
  (org-moor--wait-tick)
  (org-moor--wait-tick))

(ert-deftest org-moor-test-take-url-awaits-its-replica ()
  "A URL taken is opened in the browser and fetched by nothing: its source
has its origin alone until the file the browser saves is said to be it."
  (org-moor-test--with-notes "* Topic\n"
    (org-moor-test--browser
      (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (error "Fetched")))
                ((symbol-function 'url-copy-file) (lambda (&rest _) (error "Fetched")))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (let* ((m (org-moor-test--titles-as-given
                    (org-moor-take-url "https://example.com/a b" "Example")))
               (id (org-moor-test--id-at m))
               (record (org-moor-record id)))
          (should (equal opened '("https://example.com/a%20b")))
          (should (equal (plist-get record :origin-url) "https://example.com/a b"))
          (should-not (plist-get record :replica-file))
          (should (string-prefix-p "[" (plist-get record :replica-requested)))
          (should (memq 'org-moor--mode-line global-mode-string))
          (should (string-search "Example" (org-moor--waiting-line)))
          (org-moor-test--arrive downloads "Example.html" "<html>saved</html>")
          (let ((record (org-moor-record id)))
            (should (equal (plist-get record :replica-file) "Example.html"))
            (should (equal (plist-get record :replica-via) "web"))
            (should-not (plist-get record :replica-requested))
            (should (plist-get record :replica)))
          (should-not (file-exists-p (expand-file-name "Example.html" downloads)))
          (should-not org-moor--waiting)
          (should-not (memq 'org-moor--mode-line global-mode-string)))))))

(ert-deftest org-moor-test-waiting-asks-and-passes-over ()
  "A file still downloading is not asked about; one said not to be the
replica is passed over, and the next is asked about; one older than the
wait is never."
  (org-moor-test--with-notes ""
    (org-moor-test--browser
      (let ((record (org-moor-new-source "Deck" "https://x.com/d"))
            (asked nil)
            (answers (list nil t)))
        (with-temp-file (expand-file-name "old.pdf" downloads) (insert "old"))
        (set-file-times (expand-file-name "old.pdf" downloads) (time-subtract nil 60))
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (push prompt asked) (pop answers))))
          (org-moor-wait-for-replica record)
          (org-moor-test--arrive downloads "deck.pptx.crdownload")
          (should-not asked)
          (delete-file (expand-file-name "deck.pptx.crdownload" downloads))
          (org-moor-test--arrive downloads "other.zip")
          (should (string-search "other.zip" (car asked)))
          (should org-moor--waiting)
          (org-moor-test--arrive downloads "deck.pptx")
          (should (string-search "deck.pptx" (car asked)))
          (should (= (length asked) 2))
          (should (equal (plist-get (org-moor-record (plist-get record :id)) :replica-file)
                         "deck.pptx"))
          (should (file-exists-p (expand-file-name "other.zip" downloads)))
          (should (file-exists-p (expand-file-name "old.pdf" downloads))))))))

(ert-deftest org-moor-test-waiting-for-one-at-a-time ()
  "Another replica is waited for only once the one waited for is let go of."
  (org-moor-test--with-notes ""
    (org-moor-test--browser
      (let ((a (org-moor-new-source "A" "https://x.com/a"))
            (b (org-moor-new-source "B" "https://x.com/b")))
        (org-moor-wait-for-replica a)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (should-error (org-moor-wait-for-replica b) :type 'user-error))
        (should (equal (plist-get (plist-get org-moor--waiting :record) :id) (plist-get a :id)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (org-moor-wait-for-replica b))
        (should (equal (plist-get (plist-get org-moor--waiting :record) :id)
                       (plist-get b :id)))))))

(ert-deftest org-moor-test-browser-sent-where-it-downloads ()
  "Where `org-moor-browser-url-function' says, the browser is sent to
download, and the replica waited for where it downloads to."
  (org-moor-test--with-notes ""
    (org-moor-test--browser
      (let* ((there (file-name-as-directory (make-temp-file "there" t)))
             (org-moor-browser-url-function
              (lambda (_url done) (funcall done (list "https://files.example.com/get?id=1" there))))
             (record (org-moor-new-source "File" "https://files.example.com/view?id=1")))
        (org-moor-open-in-browser record)
        (should (equal opened '("https://files.example.com/get?id=1")))
        (should (equal (plist-get org-moor--waiting :dir) there))
        (org-moor-stop-waiting)
        (delete-directory there t)
        (should-error (org-moor-open-in-browser (org-moor-new-source "Mail" nil))
                      :type 'user-error)))))

(ert-deftest org-moor-test-protocol-waits-for-what-is-in-front ()
  "A page in front, from outside Emacs, is open already: its replica is
waited for, where the script says it arrives, and nothing is opened."
  (let (args)
    (cl-letf (((symbol-function 'org-moor-take-url) (lambda (&rest a) (setq args a)))
              ((symbol-function 'select-frame-set-input-focus) #'ignore))
      (org-moor-protocol '(:url "https://x.com/a" :title "A"))
      (should (equal args '("https://x.com/a" "A" nil t nil nil)))
      (org-moor-protocol '(:url "https://x.com/f" :arriving "C:/dl" :since "60"))
      (should (equal (nth 4 args) "C:/dl"))
      (should (equal (nth 5 args) (seconds-to-time 60))))))

(ert-deftest org-moor-test-page-in-front-awaited-already ()
  "A page in front whose replica is awaited already is not taken again:
its source is waited for, the one waited for or one awaited in this
buffer's sources."
  (org-moor-test--with-notes "* Topic\n"
    (org-moor-test--browser
      (let* ((elsewhere (file-name-as-directory (make-temp-file "else" t)))
             (waited (org-moor-new-source "Wiki" "https://x.com/wiki" elsewhere))
             (here (org-moor-new-source "Act" "https://x.com/act")))
        (org-moor-wait-for-replica waited)
        (should-not (org-moor-take-url "https://x.com/wiki" "Wiki" nil t))
        (should (equal (plist-get (plist-get org-moor--waiting :record) :id)
                       (plist-get waited :id)))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (should-not (org-moor-take-url "https://x.com/act" "Act" nil t)))
        (should (equal (plist-get (plist-get org-moor--waiting :record) :id)
                       (plist-get here :id)))
        (should (equal (buffer-string) "* Topic\n"))
        (should (= (length (org-moor-sources-in org-moor-sources-directory)) 1))
        (should (= (length (org-moor-sources-in elsewhere)) 1))
        (should-not opened)
        (delete-directory elsewhere t)))))

(ert-deftest org-moor-test-opening-without-a-replica-says-so ()
  "A source whose replica is awaited opens its origin, as it is now, and says so."
  (let ((opened nil) (said nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq opened u)))
              ((symbol-function 'kill-new) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (org-moor-open-record '(:id "x" :title "Deck" :origin-url "https://x.com/d"))
      (should (equal opened "https://x.com/d"))
      (should (string-match-p "no replica of Deck yet; opened its origin" said)))))

;;;; For an agent

(defun org-moor-test--batch (&rest args)
  "Return what `org-moor-batch-take' prints, given ARGS."
  (let ((command-line-args-left args))
    (with-output-to-string (org-moor-batch-take))))

(ert-deftest org-moor-test-batch-take-a-url-awaits-the-user ()
  "A URL an agent takes is awaited from the user, once: taken again, it is
the same source.  Nothing is fetched."
  (org-moor-test--with-notes ""
    (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) (error "Fetched")))
              ((symbol-function 'make-process) (lambda (&rest _) (error "Run"))))
      (let* ((sources org-moor-sources-directory)
             (out (org-moor-test--batch sources "https://x.com/p" "Page")))
        (should (string-match "\\`\\[\\[moor:\\([^]]+\\)\\]\\[Page\\]\\]\n" out))
        (let ((id (match-string 1 out)))
          (should (string-search "Nothing to read yet: the replica is awaited from the user" out))
          (should (string-search "Replica: awaited from the user" out))
          (should (string-search (concat "[[moor:" id "]")
                                 (org-moor-test--batch sources "https://x.com/p")))
          (should (= (length (org-moor-sources-in sources)) 1)))))))

(ert-deftest org-moor-test-batch-take-a-file-with-its-origin ()
  "A file an agent fetched itself is the replica, seen where --origin-url
says; what a machine reads better is given beside it, by the URL it came from."
  (org-moor-test--with-notes ""
    (let* ((sources org-moor-sources-directory)
           (page (expand-file-name "law.html" dir))
           (xml (expand-file-name "lawdata.xml" dir)))
      (with-temp-file page (insert "<html>law</html>"))
      (with-temp-file xml (insert "<Law/>"))
      (cl-letf (((symbol-function 'org-moor-make-machine-readable) #'identity))
        (let* ((out (org-moor-test--batch
                     "--origin-url=https://laws.example/law/1" sources page "Act"
                     (concat "--machine-readable=" xml)
                     "--machine-readable-url=https://laws.example/api/1"))
               (id (and (string-match "\\[\\[moor:\\([^]]+\\)\\]" out) (match-string 1 out)))
               (record (org-moor-record id sources)))
          (should (equal (plist-get record :origin-url) "https://laws.example/law/1"))
          (should (equal (plist-get record :replica-file) "law.html"))
          (should (equal (plist-get record :replica-via) "agent"))
          (should (equal (plist-get record :machine-readable-url) "https://laws.example/api/1"))
          (should (equal (plist-get record :machine-readable-file) "lawdata.xml"))
          (should (equal (plist-get record :machine-readable-via) "agent"))
          (should (plist-get record :machine-readable-taken))
          (should (string-search (concat "Read: " (plist-get record :machine-readable)) out))
          (should (string-search (concat "Replica: " (plist-get record :replica)) out))
          (should (file-exists-p page)))))))

(ert-deftest org-moor-test-batch-take-tells-the-user ()
  "With --server, the user's Emacs is told of a replica awaited; when it
cannot be reached, the source is taken all the same."
  (org-moor-test--with-notes ""
    (let ((asked nil))
      (cl-letf (((symbol-function 'org-moor--ask-server)
                 (lambda (server form) (push (list server form) asked) t)))
        (org-moor-test--batch "--server=/run/s" org-moor-sources-directory "https://x.com/a" "A")
        (should (equal asked '(("/run/s" (org-moor-awaited-for-agent "A"))))))
      (cl-letf (((symbol-function 'org-moor--ask-server)
                 (lambda (&rest _) (signal 'file-error '("Cannot reach Emacs")))))
        (should (string-prefix-p "[[moor:"
                                 (org-moor-test--batch "--server=/run/s"
                                                       org-moor-sources-directory
                                                       "https://x.com/b" "B")))))))

(ert-deftest org-moor-test-batch-take-again-by-id ()
  "moor:ID takes nothing, and says again what the source has."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Wiki" "https://w.example/p"))
           (id (plist-get record :id))
           (sources org-moor-sources-directory))
      (should (string-search "Replica: awaited"
                             (org-moor-test--batch sources (concat "moor:" id))))
      (with-temp-file (expand-file-name "page.html" (plist-get record :dir)) (insert "p"))
      (cl-letf (((symbol-function 'org-moor-make-machine-readable) #'identity))
        (org-moor-record-replica record "page.html" 'web))
      (should (string-search "Replica: " (org-moor-test--batch sources (concat "moor:" id))))
      (should-error (org-moor-test--batch sources "moor:nosuch")))))

(ert-deftest org-moor-test-batch-take-prints-utf-8 ()
  "The link is printed in UTF-8, whatever the system's code page."
  (org-moor-test--with-notes ""
    (let ((coding 'unset))
      (cl-letf (((symbol-function 'princ)
                 (lambda (&rest _) (setq coding coding-system-for-write))))
        (org-moor-test--batch org-moor-sources-directory "https://x.com/a" "題"))
      (should (eq coding 'utf-8)))))

(ert-deftest org-moor-test-batch-command ()
  "The command names the server while it runs, and the agent fills in the rest."
  (let ((sources (regexp-quote (shell-quote-argument (expand-file-name "/tmp/s") t))))
    (cl-letf (((symbol-function 'org-moor--server-file) (lambda () "/run/emacs/server")))
      (should (string-match-p
               (concat "-f org-moor-batch-take --server=/run/emacs/server "
                       sources " REPLICA TITLE\\'")
               (org-moor-batch-command "/tmp/s/"))))
    (cl-letf (((symbol-function 'org-moor--server-file) #'ignore))
      (should (string-match-p (concat "-f org-moor-batch-take " sources " REPLICA TITLE\\'")
                              (org-moor-batch-command "/tmp/s/"))))
    (should (string-match-p (concat "-f org-moor-batch-find " sources " moor:ID WORDS\\'")
                            (org-moor-batch-find-command "/tmp/s/")))))

(ert-deftest org-moor-test-argument-coding ()
  (let ((system-type 'windows-nt) (locale-coding-system 'cp932))
    (should (eq (org-moor-argument-coding) 'cp932)))
  (let ((system-type 'darwin))
    (should (eq (org-moor-argument-coding) 'utf-8))))

(provide 'org-moor-take-test)
;;; org-moor-take-test.el ends here
