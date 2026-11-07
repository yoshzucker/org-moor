;;; org-moor-core-test.el --- Tests for org-moor-core  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-core-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(ert-deftest org-moor-test-source-directory ()
  ;; A directory from the root is on a drive on Windows: c:/sources/.
  (let ((org-moor-sources-directory "/sources/")
        (root (expand-file-name "/")))
    (with-temp-buffer
      (should (equal (org-moor-source-directory "6f1c-aa") (concat root "sources/6f/1c-aa/")))
      (should (equal (org-moor-source-directory "6f1c-aa" "/other/")
                     (concat root "other/6f/1c-aa/")))
      (should-error (org-moor-source-directory "6")))))

(ert-deftest org-moor-test-link-parse ()
  (should (equal (org-moor-link-parse "6f1c") '("6f1c" . nil)))
  (should (equal (org-moor-link-parse "6f1c::rule two") '("6f1c" . "rule two")))
  (should (equal (org-moor-link-parse "6f1c::12") '("6f1c" . "12")))
  (should-error (org-moor-link-parse "::12")))

(ert-deftest org-moor-test-link-names-the-source ()
  (let ((record '(:id "ID" :title "Manual")))
    (should (equal (org-moor-link record) "[[moor:ID][Manual]]"))
    (should (equal (org-moor-link record "rule two") "[[moor:ID::rule two][Manual “rule two”]]"))))

(ert-deftest org-moor-test-path-url ()
  "A path is written as a file: URL, and read back as the path."
  (should (equal (org-moor-path-url "/home/me/資料 v2.pdf") "file:///home/me/資料 v2.pdf"))
  (should (equal (org-moor-path-url "https://x.com/a") "https://x.com/a"))
  (should (equal (org-moor-path-url "/a/100%.txt") "file:///a/100%25.txt"))
  (should (equal (org-moor-file-url-path "file:///home/me/資料 v2.pdf") "/home/me/資料 v2.pdf"))
  (should (equal (org-moor-file-url-path "file:///a/100%25.txt") "/a/100%.txt"))
  (should (equal (org-moor-file-url-path "file:///C:/Users/me/a.pdf") "C:/Users/me/a.pdf"))
  (should (equal (org-moor-file-url-path "file://server/share/a.pdf") "//server/share/a.pdf"))
  (should (equal (org-moor-file-url-path "file://localhost/a.pdf") "/a.pdf"))
  (should-not (org-moor-url-path "https://x.com/a")))

(ert-deftest org-moor-test-link-says-whether-a-replica-is-kept ()
  "At a link, the echo area says whether its source has a replica, and its origin."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Manual" "https://example.com/m.pdf"))
           (id (plist-get record :id)))
      (insert ":PROPERTIES:\n:SEEN: [[moor:" id "][Manual]]\n:END:\n")
      (search-backward "moor:")
      (let ((said (org-moor-eldoc)))
        (should (string-match-p "\\`moor: no replica, awaited since .*  https://example.com/m.pdf\\'" said))
        (should (eq (get-text-property 6 'face said) 'warning)))
      (org-moor-record-set record :replica-file "m.pdf")
      (should (string-match-p "\\`moor: replica m.pdf is gone" (org-moor-eldoc)))
      (with-temp-file (expand-file-name "m.pdf" (plist-get record :dir)) (insert "pdf"))
      (org-moor-record-replica record "m.pdf" 'web)
      (should (string-match-p "\\`moor: replica m.pdf (3) web " (org-moor-eldoc)))
      (goto-char (point-min))
      (should-not (org-moor-eldoc))
      (should (string-match-p "\\`moor: no source nope"
                              (org-moor-link-summary "nope::3"))))))

(ert-deftest org-moor-test-record-roundtrip ()
  "A new source has a record saying what it is; setting one part of it
keeps the rest, and what else was written there."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Manual" "https://example.com/m.pdf"))
           (id (plist-get record :id))
           (file (expand-file-name org-moor-record-file (plist-get record :dir))))
      (should (file-in-directory-p file org-moor-sources-directory))
      (should (string-match-p "\\`:PROPERTIES:\n:ORIGIN_URL: +https://example.com/m.pdf\n:REPLICA_REQUESTED: +\\["
                              (org-moor-test--file-text file)))
      (let ((read (org-moor-record id)))
        (should (equal (plist-get read :title) "Manual"))
        (should (equal (plist-get read :origin-url) "https://example.com/m.pdf"))
        (should-not (plist-get read :replica)))
      (with-temp-file file
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "Notes of mine.\n"))
      (org-moor-record-set record :replica-file "m.pdf")
      (should-not (plist-get (org-moor-record id) :replica))
      (with-temp-file (expand-file-name "m.pdf" (plist-get record :dir)))
      (should (plist-get (org-moor-record id) :replica))
      (org-moor-record-set record :title "Manual 2026")
      (should (equal (plist-get (org-moor-record id) :title) "Manual 2026"))
      (should (equal (plist-get (org-moor-record id) :origin-url) "https://example.com/m.pdf"))
      (should (string-match-p "^:REPLICA_FILE: +m\\.pdf\n:END:\n" (org-moor-test--file-text file)))
      (should (string-match-p "Notes of mine\\." (org-moor-test--file-text file))))))

(defun org-moor-test--file-text (file)
  "Return what FILE holds."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defun org-moor-test--with-replica (title name text)
  "Return a new source titled TITLE whose replica NAME holds TEXT."
  (let ((record (org-moor-new-source title "/x")))
    (with-temp-file (expand-file-name name (plist-get record :dir)) (insert text))
    (let ((org-moor-machine-readable-converters nil))
      (org-moor-record-replica record name 'file))))

(ert-deftest org-moor-test-sources-of-a-file ()
  "A file can keep its sources apart; a copy of a heading taken out of it
reads them as the file does."
  (org-moor-test--with-notes "* Topic\n"
    (let* ((own (expand-file-name "own/" dir))
           (org-moor-sources-directory-functions
            (list (lambda (file) (and (equal (file-name-nondirectory file) "notes.org") own)))))
      (should (equal (org-moor-sources) own))
      (should (equal (org-moor-sources "/elsewhere/x.org")
                     (file-name-as-directory (expand-file-name org-moor-sources-directory))))
      (with-current-buffer (save-current-buffer (org-dock-heading (point-min) :display nil))
        (should (equal (org-moor-sources) own))))))

(ert-deftest org-moor-test-follow-the-words ()
  "A link to words opens a text at them, and the replica remembers the
heading it was opened from."
  (org-moor-test--with-notes "* Topic\n"
    (let ((record (org-moor-test--with-replica "Rules" "rules.txt" "rule one\nrule\ntwo\n"))
          (heading (point-marker)))
      (org-moor-link-follow (format "%s::rule two" (plist-get record :id)) nil)
      (should (equal (file-name-nondirectory buffer-file-name) "rules.txt"))
      (should (looking-at "rule\ntwo"))
      (should (equal org-moor--read-for heading)))))

(ert-deftest org-moor-test-page-opens-in-the-browser ()
  "A page is opened in the browser, at the words where it goes to words,
and the words are put on the clipboard; so is anything else in its own
program."
  (let ((opened nil) (killed nil) (with nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq opened u)))
              ((symbol-function 'kill-new) (lambda (s &rest _) (setq killed s)))
              ((symbol-function 'org-open-file) (lambda (f &rest _) (setq with f))))
      (org-moor-open-replica-file "/s/ab/cd/page.html" "安全運転管理者")
      (should (equal opened (concat "file:///s/ab/cd/page.html#:~:text="
                                    (url-hexify-string "安全運転管理者"))))
      (should (equal killed "安全運転管理者"))
      (setq killed nil)
      (org-moor-open-replica-file "/s/ab/cd/m.docx" "the words")
      (should (equal with "/s/ab/cd/m.docx"))
      (should (equal killed "the words")))))

(ert-deftest org-moor-test-pdf-opens-at-the-page ()
  "A PDF opens at the page the words are on, found by its text."
  (cl-letf (((symbol-function 'org-moor-pdf-pages)
             (lambda (_) (list "one" "安全 運転\n管理者 を" "three"))))
    (should (= (org-moor-pdf-page-of "/x.pdf" "安全運転管理者を") 2))
    (should-not (org-moor-pdf-page-of "/x.pdf" "four"))))

(ert-deftest org-moor-test-words-in-replica ()
  "Words are found in a replica however their whitespace and width differ,
and words left out split them into parts found in turn."
  (org-moor-test--with-notes ""
    (let ((record (org-moor-test--with-replica
                   "Act" "act.txt" "第一条 自動車の使用者は、\n安全運転管理者を選任しなければならない。\n")))
      (should (eq (org-moor-words-in-replica record "使用者は、安全運転管理者を") 'found))
      (should (eq (org-moor-words-in-replica record "第１条") 'not-found))
      (should (eq (org-moor-words-in-replica record "使用者は、…選任しなければ") 'found))
      (should (eq (org-moor-words-in-replica record "選任しなければ…使用者は") 'not-found))
      (should (eq (org-moor-words-in-replica (org-moor-new-source "None" "https://x") "a")
                  'no-replica))
      (let ((command-line-args-left (list org-moor-sources-directory
                                          (concat "moor:" (plist-get record :id))
                                          "安全運転管理者")))
        (should (equal (with-output-to-string (org-moor-batch-find)) "Found\n"))))))

(defun org-moor-test--convert (file)
  "Make FILE readable as Markdown beside it, as a converter does."
  (with-temp-file (concat file ".md") (insert "md"))
  (concat file ".md"))

(ert-deftest org-moor-test-machine-readable-made-from-the-replica ()
  "A replica a converter makes readable is recorded as made from it, and
made again from the next; a fetched one is left as it is."
  (org-moor-test--with-notes ""
    (let* ((org-moor-machine-readable-converters
            (list (cons "docx" #'org-moor-test--convert)))
           (record (org-moor-new-source "Minutes" "/x")))
      (with-temp-file (expand-file-name "m.docx" (plist-get record :dir)) (insert "d"))
      (setq record (org-moor-record-replica record "m.docx" 'file))
      (should (equal (plist-get record :machine-readable-made-from) "m.docx"))
      (should (equal (plist-get record :machine-readable-file) "m.docx.md"))
      (should (equal (plist-get record :machine-readable-made-by) "test--convert"))
      (should (plist-get record :machine-readable-made))
      (should (equal (org-moor-file-for-machine record) (plist-get record :machine-readable)))
      (with-temp-file (expand-file-name "n.txt" (plist-get record :dir)) (insert "t"))
      (setq record (org-moor-record-replica record "n.txt" 'file))
      (should-not (plist-get record :machine-readable-file))
      (should-not (file-exists-p (expand-file-name "m.docx.md" (plist-get record :dir))))
      (should (equal (org-moor-file-for-machine record) (plist-get record :replica)))
      (let ((org-moor-machine-readable-converters
             (list (cons "docx" (lambda (_) (user-error "Cannot convert"))))))
        (with-temp-file (expand-file-name "o.docx" (plist-get record :dir)))
        (setq record (org-moor-record-replica record "o.docx" 'file))
        (should (equal (org-moor-file-for-machine record) (plist-get record :replica)))))))

(ert-deftest org-moor-test-handed-to-an-agent ()
  "A copy's links to sources are handed over with the files to read, and
the request says which sources they are, how to cite them by quoting, and
how to check a quote."
  (org-moor-test--with-notes "* Topic\n"
    (let ((record (org-moor-test--with-replica "Rules" "rules.txt" "r"))
          (org-dock-agent-function #'ignore))
      (end-of-line)
      (insert "\n" (org-moor-link record "r"))
      (with-current-buffer (save-current-buffer (org-dock-heading (point-min) :display nil))
        (should (equal (org-dock-links)
                       (list (cons (format "moor:%s  Rules" (plist-get record :id))
                                   (expand-file-name "rules.txt" (plist-get record :dir))))))
        (let* (got
               (org-dock-agent-function (lambda (r) (setq got r))))
          (org-dock-ask-agent)
          (let ((text (org-moor-test--file-text (plist-get got :request)))
                (sources (regexp-quote (shell-quote-argument
                                        (directory-file-name org-moor-sources-directory) t))))
            (should (string-match-p (concat "^- moor:" (regexp-quote (plist-get record :id)) "  Rules$") text))
            (should (string-search "[[moor:ID::START][TITLE “START”]]" text))
            (should (string-search "#+begin_quote" text))
            (should (string-match-p (concat "-f org-moor-batch-take " sources " REPLICA TITLE$") text))
            (should (string-match-p (concat "-f org-moor-batch-find " sources " moor:ID WORDS$") text))
            (should-not (string-match-p "l\\. 123\\|::12\\]" text))))))))

(ert-deftest org-moor-test-place-by-quote-asked-of-a-file ()
  "Where a file finds its places by the quote, the request has links without one."
  (org-moor-test--with-notes "* Topic\n"
    (let ((org-moor-place-by-quote-functions (list (lambda (_) t))))
      (should (org-moor-place-by-quote-p))
      (should (string-search "[[moor:ID][WHERE]]"
                             (org-moor-request-text (list :origin buffer-file-name)))))))

(ert-deftest org-moor-test-batch-take ()
  "An agent takes a file into a sources directory, copied, and is printed
a link to it named by its title, what to read, and the replica."
  (org-moor-test--with-notes ""
    (let* ((file (expand-file-name "minutes.txt" dir))
           (command-line-args-left (list org-moor-sources-directory file "Minutes"))
           printed)
      (with-temp-file file (insert "We agreed.\n"))
      (setq printed (with-output-to-string (org-moor-batch-take)))
      (should (string-match "\\`\\[\\[moor:\\([^]]+\\)\\]\\[Minutes\\]\\]\nRead: \\(.+\\)\nReplica: \\(.+\\)\n\\'"
                            printed))
      (let ((id (match-string 1 printed))
            (read (match-string 2 printed))
            (replica (match-string 3 printed)))
        (should (equal (plist-get (org-moor-record id) :replica) replica))
        (should (equal read replica)))
      (should (file-exists-p file)))))

(ert-deftest org-moor-test-record-set-nil-takes-the-line-out ()
  "A key set to nil is gone from the record file and from the record."
  (org-moor-test--with-notes ""
    (let ((record (org-moor-new-source "Wiki" "https://w.example/p")))
      (should (plist-get (org-moor-record (plist-get record :id)) :replica-requested))
      (org-moor-record-set record :replica-requested nil)
      (should-not (plist-get record :replica-requested))
      (should-not (plist-get (org-moor-record (plist-get record :id)) :replica-requested))
      (should (equal (plist-get (org-moor-record (plist-get record :id)) :origin-url)
                     "https://w.example/p")))))

(ert-deftest org-moor-test-link-completes-by-title ()
  "`org-insert-link' completes a `moor:' link by title, newest first,
and names it by the title, its place too."
  (org-moor-test--with-notes "* Topic\n"
    (let ((old (org-moor-new-source "Old manual" nil))
          (new (org-moor-new-source "New memo" nil))
          offered)
      (org-moor-record-set old :replica-taken "[2026-10-01 Thu 09:00]")
      (org-moor-record-set new :replica-taken "[2026-10-17 Sat 09:00]")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p table &rest _)
                   (setq offered (funcall table "" nil t))
                   (seq-find (lambda (c) (string-prefix-p "Old manual" c)) offered))))
        (should (equal (org-link-get-parameter "moor" :complete) #'org-moor-link-complete))
        (should (equal (funcall (org-link-get-parameter "moor" :complete))
                       (concat "moor:" (plist-get old :id)))))
      (should (string-prefix-p "New memo" (car offered)))
      (let ((describe (org-link-get-parameter "moor" :insert-description)))
        (should (equal (funcall describe (concat "moor:" (plist-get old :id)) nil)
                       "Old manual"))
        (should (equal (funcall describe (concat "moor:" (plist-get old :id) "::rule two") nil)
                       "Old manual “rule two”"))))))

(ert-deftest org-moor-test-fetches-nothing ()
  "org-moor fetches nothing: no function of it reaches out to a network,
nor runs a program that would."
  (let ((dir (file-name-directory (locate-library "org-moor"))))
    (dolist (file (directory-files dir t "\\`org-moor.*\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (should-not (re-search-forward
                     (concat "(\\(url-retrieve\\|url-retrieve-synchronously\\|url-copy-file"
                             "\\|url-insert-file-contents\\|open-network-stream"
                             "\\|make-network-process\\|eww\\|eww-open-file\\)[ )]"
                             "\\|\"\\(curl\\|wget\\|monolith\\|single-file\\|node\\)\"")
                     nil t))))))

(provide 'org-moor-core-test)
;;; org-moor-core-test.el ends here
