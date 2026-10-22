;;; org-moor-core-test.el --- Tests for org-moor-core  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-core-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(ert-deftest org-moor-test-source-directory ()
  (let ((org-moor-sources-directory "/sources/"))
    (with-temp-buffer
      (should (equal (org-moor-source-directory "6f1c-aa") "/sources/6f/1c-aa/"))
      (should (equal (org-moor-source-directory "6f1c-aa" "/other/") "/other/6f/1c-aa/"))
      (should-error (org-moor-source-directory "6")))))

(ert-deftest org-moor-test-link-parse ()
  (should (equal (org-moor-link-parse "6f1c") '("6f1c" . nil)))
  (should (equal (org-moor-link-parse "6f1c::12") '("6f1c" . 12)))
  (should (equal (org-moor-link-parse "6f1c::rule two") '("6f1c" . "rule two")))
  (should-error (org-moor-link-parse "::12")))

(ert-deftest org-moor-test-link-names-the-source ()
  (let ((record '(:id "ID" :title "Manual")))
    (should (equal (org-moor-link record) "[[moor:ID][Manual]]"))
    (should (equal (org-moor-link record 12 'page) "[[moor:ID::12][Manual p. 12]]"))
    (should (equal (org-moor-link record 123 'line) "[[moor:ID::123][Manual l. 123]]"))))

(ert-deftest org-moor-test-link-says-whether-a-copy-is-kept ()
  "At a link, the echo area says whether its source has a copy, and its origin."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Manual" "https://example.com/m.pdf" 'web))
           (id (plist-get record :id)))
      (insert ":PROPERTIES:\n:SEEN: [[moor:" id "][Manual]]\n:END:\n")
      (search-backward "moor:")
      (let ((said (org-moor-eldoc)))
        (should (string-match-p "\\`moor: no copy  web .*  https://example.com/m.pdf\\'" said))
        (should (eq (get-text-property 6 'face said) 'warning)))
      (org-moor-record-set record :file "m.pdf")
      (should (string-match-p "\\`moor: copy m.pdf is gone" (org-moor-eldoc)))
      (with-temp-file (expand-file-name "m.pdf" (plist-get record :dir)) (insert "pdf"))
      (should (string-match-p "\\`moor: copy m.pdf (3)  web " (org-moor-eldoc)))
      (goto-char (point-min))
      (should-not (org-moor-eldoc))
      (should (string-match-p "\\`moor: no source nope"
                              (org-moor-link-summary "nope::3"))))))

(ert-deftest org-moor-test-record-roundtrip ()
  "A new source has a record saying what it is; setting one part of it
keeps the rest, and what else was written there."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Manual" "https://example.com/m.pdf" 'chat))
           (id (plist-get record :id))
           (file (expand-file-name org-moor-record-file (plist-get record :dir))))
      (should (file-in-directory-p file org-moor-sources-directory))
      (should (string-prefix-p ":PROPERTIES:\n:MOOR_ORIGIN: https://example.com/m.pdf\n:MOOR_VIA:    chat\n"
                               (org-moor-test--file-text file)))
      (let ((read (org-moor-record id)))
        (should (equal (plist-get read :title) "Manual"))
        (should (equal (plist-get read :via) "chat"))
        (should-not (plist-get read :copy)))
      (with-temp-file file
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "Notes of mine.\n"))
      (org-moor-record-set record :file "m.pdf")
      (should-not (plist-get (org-moor-record id) :copy))
      (with-temp-file (expand-file-name "m.pdf" (plist-get record :dir)))
      (should (plist-get (org-moor-record id) :copy))
      (org-moor-record-set record :title "Manual 2026")
      (should (equal (plist-get (org-moor-record id) :title) "Manual 2026"))
      (should (equal (plist-get (org-moor-record id) :origin) "https://example.com/m.pdf"))
      (should (string-match-p "^:MOOR_FILE: +m\\.pdf\n:END:\n" (org-moor-test--file-text file)))
      (should (string-match-p "Notes of mine\\." (org-moor-test--file-text file))))))

(defun org-moor-test--file-text (file)
  "Return what FILE holds."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

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

(ert-deftest org-moor-test-follow-a-line ()
  "A link to a line opens the copy there, and the copy remembers the
heading it was opened from."
  (org-moor-test--with-notes "* Topic\n"
    (let ((record (org-moor-new-source "Rules" "/x" 'file))
          (heading (point-marker)))
      (with-temp-file (expand-file-name "rules.txt" (plist-get record :dir))
        (insert "rule one\nrule two\n"))
      (org-moor-record-set record :file "rules.txt")
      (org-moor-link-follow (format "%s::2" (plist-get record :id)) nil)
      (should (equal (file-name-nondirectory buffer-file-name) "rules.txt"))
      (should (looking-at "rule two"))
      (should (equal org-moor--read-for heading)))))

(ert-deftest org-moor-test-handed-to-an-agent ()
  "A copy's links to sources are handed over with the files to read, and
the request says which sources they are and how to cite them."
  (org-moor-test--with-notes "* Topic\n"
    (let ((record (org-moor-new-source "Rules" "/x" 'file))
          (org-dock-agent-function #'ignore))
      (with-temp-file (expand-file-name "rules.txt" (plist-get record :dir)) (insert "r"))
      (org-moor-record-set record :file "rules.txt")
      (end-of-line)
      (insert "\n" (org-moor-link record 1 'line))
      (with-current-buffer (save-current-buffer (org-dock-heading (point-min) :display nil))
        (should (equal (org-dock-links)
                       (list (cons (format "moor:%s  Rules" (plist-get record :id))
                                   (expand-file-name "rules.txt" (plist-get record :dir))))))
        (let* (got
               (org-dock-agent-function (lambda (r) (setq got r))))
          (org-dock-ask-agent)
          (let ((text (org-moor-test--file-text (plist-get got :request))))
            (should (string-match-p (concat "^- moor:" (regexp-quote (plist-get record :id)) "  Rules$") text))
            (should (string-match-p (regexp-quote "[[moor:ID::123][TITLE l. 123]]") text))
            (should (string-match-p (concat "-f org-moor-batch-take "
                                            (regexp-quote (directory-file-name org-moor-sources-directory))
                                            " FILE-OR-URL TITLE$")
                                    text))))))))

(ert-deftest org-moor-test-unreadable-copy-handed-over ()
  "A copy that cannot be made readable is handed to an agent as it is."
  (org-moor-test--with-notes "* Topic\n"
    (let ((record (org-moor-new-source "Minutes" "/x" 'file))
          (org-moor-readable-converters
           (list (cons "docx" (lambda (_) (user-error "Cannot convert"))))))
      (with-temp-file (expand-file-name "m.docx" (plist-get record :dir)))
      (org-moor-record-set record :file "m.docx")
      (should (equal (car (org-moor-link-file "moor" (plist-get record :id)))
                     (expand-file-name "m.docx" (plist-get record :dir)))))))

(ert-deftest org-moor-test-batch-take ()
  "An agent takes a file into a sources directory, copied, and is printed
a link to it named by its title."
  (org-moor-test--with-notes ""
    (let* ((file (expand-file-name "minutes.txt" dir))
           (command-line-args-left (list org-moor-sources-directory file "Minutes"))
           printed)
      (with-temp-file file (insert "We agreed.\n"))
      (setq printed (with-output-to-string (org-moor-batch-take)))
      (should (string-match "\\`\\[\\[moor:\\([^]]+\\)\\]\\[Minutes\\]\\]\n\\'" printed))
      (should (plist-get (org-moor-record (match-string 1 printed)) :copy))
      (should (file-exists-p file)))))

(ert-deftest org-moor-test-link-completes-by-title ()
  "`org-insert-link' completes a `moor:' link by title, newest first,
and names it by the title, its place too."
  (org-moor-test--with-notes "* Topic\n"
    (let ((old (org-moor-new-source "Old manual" nil 'file))
          (new (org-moor-new-source "New memo" nil 'chat))
          offered)
      (org-moor-record-set old :taken "[2026-10-01 Thu 09:00]")
      (org-moor-record-set new :taken "[2026-10-17 Sat 09:00]")
      (with-temp-file (expand-file-name "m.pdf" (plist-get old :dir)) (insert "%PDF"))
      (org-moor-record-set old :file "m.pdf")
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
        (should (equal (funcall describe (concat "moor:" (plist-get old :id) "::12") nil)
                       "Old manual p. 12"))
        (should (equal (funcall describe (concat "moor:" (plist-get new :id) "::3") nil)
                       "New memo l. 3"))))))

(provide 'org-moor-core-test)
;;; org-moor-core-test.el ends here
