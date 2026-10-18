;;; org-moor-return-test.el --- Tests for org-moor-return  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-return-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(ert-deftest org-moor-test-anchors ()
  "A heading something was taken to has a titled quote or a link to a
source in its own text; a source is counted once, and its time is when it
was taken."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Manual" "https://example.com/m.pdf" 'web))
           (id (plist-get record :id)))
      (org-moor-record-set record :taken "[2026-10-17 Sat 09:00]")
      (insert "#+begin_quote before any heading [2026-10-01 Thu 09:00]\n#+end_quote\n"
              "* Topic A\n"
              "#+begin_quote Chat from Robin [2026-10-15 Thu 09:00]\nx\n#+end_quote\n"
              "#+begin_quote Mail from Alex [2026-10-16 Fri 09:00]\ny\n#+end_quote\n"
              "** Manual\n"
              (format "[[moor:%s][Manual]]\nSaid on [[moor:%s::12][Manual p. 12]].\n" id id)
              "* Topic B\nnothing taken\n")
      (let* ((anchors (org-moor-anchors-in-buffer))
             (path (lambda (p) (seq-find (lambda (a) (equal (plist-get a :path) p)) anchors))))
        (should (= (length anchors) 2))
        (should-not (funcall path "Topic B"))
        (should (= (plist-get (funcall path "Topic A") :count) 2))
        (should (equal (format-time-string "%F" (plist-get (funcall path "Topic A") :time))
                       "2026-10-16"))
        (should (= (plist-get (funcall path "Topic A/Manual") :count) 1))
        (should (equal (format-time-string "%F" (plist-get (funcall path "Topic A/Manual") :time))
                       "2026-10-17"))))))

(ert-deftest org-moor-test-default-files-leave-the-sources ()
  "A source's record is an Org file, and no note."
  (org-moor-test--with-notes "* Topic\n"
    (save-buffer)
    (org-moor-new-source "Manual" "/x" 'file)
    (let ((org-directory dir)
          (org-agenda-files nil))
      (should (equal (mapcar #'file-name-nondirectory (org-moor-default-files))
                     '("notes.org"))))))

(provide 'org-moor-return-test)
;;; org-moor-return-test.el ends here
