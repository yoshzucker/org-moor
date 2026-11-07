;;; org-moor-return-test.el --- Tests for org-moor-return  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-return-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(ert-deftest org-moor-test-anchors ()
  "A heading something was taken to has a link to a source in its own
text; a source is counted once, and its time is when it was taken."
  (org-moor-test--with-notes ""
    (let* ((record (org-moor-new-source "Manual" "https://example.com/m.pdf"))
           (id (plist-get record :id))
           (chat (org-moor-new-source "Chat from Robin" nil))
           (mail (org-moor-new-source "Mail from Alex" nil)))
      (org-moor-record-set record :replica-taken "[2026-10-17 Sat 09:00]")
      (org-moor-record-set chat :replica-taken "[2026-10-15 Thu 09:00]")
      (org-moor-record-set mail :replica-taken "[2026-10-16 Fri 09:00]")
      (insert (format "[[moor:%s][before any heading]]\n" id)
              "* Topic A\n"
              (format "[[moor:%s][Chat from Robin]]\n" (plist-get chat :id))
              (format "[[moor:%s][Mail from Alex]]\n" (plist-get mail :id))
              "** Manual\n"
              (format "[[moor:%s][Manual]]\nSaid on [[moor:%s::rule two][Manual “rule two”]].\n" id id)
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
    (org-moor-new-source "Manual" "/x")
    (let ((org-directory dir)
          (org-agenda-files nil))
      (should (equal (mapcar #'file-name-nondirectory (org-moor-default-files))
                     '("notes.org"))))))

(provide 'org-moor-return-test)
;;; org-moor-return-test.el ends here
