;;; org-moor-migrate-test.el --- Tests for org-moor-migrate  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-migrate-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-core-test)

(defun org-moor-test--old-record (sources id text &rest files)
  "Write a record of the old form holding TEXT, and FILES beside it, as source ID."
  (let ((dir (org-moor-source-directory id sources)))
    (make-directory dir t)
    (with-temp-file (expand-file-name org-moor-record-file dir) (insert text))
    (dolist (f files) (with-temp-file (expand-file-name f dir) (insert "x")))
    dir))

(ert-deftest org-moor-test-migrate-records ()
  "Records of the old form are written in the current one, after asking,
and read as such; a record holding what is not known is left; one of the
current form is left alone."
  (org-moor-test--with-notes ""
    (let ((sources org-moor-sources-directory))
      (org-moor-test--old-record
       sources "aa11" ":PROPERTIES:\n:MOOR_ORIGIN: C:\\Users\\me\\資料 100%.docx\n:MOOR_VIA:    file\n:MOOR_FILE:   m.docx\n:MOOR_TAKEN:  [2026-10-01 Thu 09:00]\n:END:\n#+title: Minutes\nNotes of mine.\n"
       "m.docx" "m.docx.md")
      (org-moor-test--old-record
       sources "bb22" ":PROPERTIES:\n:MOOR_ORIGIN: https://w.example/p\n:MOOR_VIA:    web\n:MOOR_TAKEN:  [2026-10-02 Fri 09:00]\n:MOOR_SIGNIN: login.example\n:END:\n#+title: Wiki\n")
      (org-moor-test--old-record
       sources "cc33" ":PROPERTIES:\n:MOOR_ORIGIN: relative/a.pdf\n:END:\n#+title: Odd\n")
      (org-moor-new-source "New" "https://x.com/n")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (org-moor-migrate-records sources))
      (should (string-search "MOOR_" (org-moor-test--file-text
                                      (expand-file-name org-moor-record-file
                                                        (org-moor-source-directory "aa11" sources)))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (org-moor-migrate-records sources))
      (let ((minutes (org-moor-record "aa11" sources))
            (wiki (org-moor-record "bb22" sources)))
        (should (equal (plist-get minutes :origin-url) "file:///C:/Users/me/資料 100%25.docx"))
        (should (equal (org-moor-url-path (plist-get minutes :origin-url)) "C:/Users/me/資料 100%.docx"))
        (should (equal (plist-get minutes :replica-via) "file"))
        (should (equal (plist-get minutes :replica-taken) "[2026-10-01 Thu 09:00]"))
        (should (plist-get minutes :replica))
        (should (equal (plist-get minutes :machine-readable-made-from) "m.docx"))
        (should (equal (plist-get minutes :machine-readable-file) "m.docx.md"))
        (should (string-search "Notes of mine." (org-moor-test--file-text
                                                 (expand-file-name org-moor-record-file
                                                                   (plist-get minutes :dir)))))
        (should (equal (plist-get wiki :origin-url) "https://w.example/p"))
        (should (equal (plist-get wiki :replica-requested) "[2026-10-02 Fri 09:00]"))
        (should-not (plist-get wiki :replica-via))
        (should-not (string-search "MOOR_" (org-moor-test--file-text
                                            (expand-file-name org-moor-record-file (plist-get wiki :dir))))))
      (should (string-search "MOOR_ORIGIN" (org-moor-test--file-text
                                            (expand-file-name org-moor-record-file
                                                              (org-moor-source-directory "cc33" sources)))))
      (should (string-search "LEFT cc/33/moor.org: an origin of a form not known"
                             (with-current-buffer "*org-moor-migrate*" (buffer-string))))
      (should-not (seq-filter #'cadr (org-moor-migrate--plan sources))))))

(provide 'org-moor-migrate-test)
;;; org-moor-migrate-test.el ends here
