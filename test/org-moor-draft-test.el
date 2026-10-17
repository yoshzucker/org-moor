;;; org-moor-draft-test.el --- Tests for org-moor-draft  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L test -l test/org-moor-draft-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(ert-deftest org-moor-test-draft-quotes-text ()
  (with-temp-buffer
    (org-mode)
    (insert "* T\n#+begin_quote A [2026-10-17 Sat]\none\n#+end_quote\n"
            "#+begin_quote B\ntwo\n#+end_quote\n** Child\n#+begin_quote C\nno\n#+end_quote\n")
    (goto-char (point-min))
    (should (equal (org-moor-draft-quotes-text) "one\n\ntwo"))))

(ert-deftest org-moor-test-draft-write-and-abort ()
  (org-moor-test--with-notes "* Topic\n#+begin_quote Chat\nraw words\n#+end_quote\n** Child\n"
    (let ((org-moor-draft-directory (expand-file-name "drafts/" dir))
          (notes (current-buffer)))
      (org-moor-draft)
      (should org-moor-draft-mode)
      (should (equal (buffer-string) "raw words"))
      (let ((draft buffer-file-name))
        (erase-buffer)
        (insert "Clear sentence.")
        (org-moor-draft-finish)
        (should-not (file-exists-p draft)))
      (with-current-buffer notes
        (should (string-match-p "#\\+end_quote\nClear sentence\\.\n\\*\\* Child"
                                (buffer-string)))
        (goto-char (point-min))
        (org-moor-draft)
        (set-buffer-modified-p nil)
        (org-moor-draft-abort))
      (should (eq (current-buffer) notes)))))

(ert-deftest org-moor-test-draft-fit-headings ()
  (should (equal (org-moor-draft-fit-headings "no headings" 2) "no headings"))
  (should (equal (org-moor-draft-fit-headings "* A\ntext\n** B\n* C\n" 2)
                 "*** A\ntext\n**** B\n*** C\n"))
  (should (equal (org-moor-draft-fit-headings "*** A\n**** B\n" 1)
                 "** A\n*** B\n")))

(ert-deftest org-moor-test-draft-readable ()
  (let ((org-moor-draft-converters '(("docx" . org-moor-draft-pandoc)))
        made)
    (cl-letf (((symbol-function 'org-moor--process-string)
               (lambda (&rest args) (push args made) "")))
      (should (equal (org-moor-draft-readable "/s/a.pdf") "/s/a.pdf"))
      (should (equal (org-moor-draft-readable "/s/a.txt") "/s/a.txt"))
      (should (equal (org-moor-draft-readable "/s/a.docx") "/s/a.docx.md"))
      (should (equal (car made)
                     '("pandoc" "--to=gfm" "--wrap=none" "-o" "/s/a.docx.md" "/s/a.docx"))))))

(ert-deftest org-moor-test-draft-conflicts ()
  (should-not (org-moor-draft-conflicts "a\nb" "a\nb"))
  (should (equal (org-moor-draft-conflicts "a\nb\nc" "a\nB\nc")
                 "a\n<<<<<<< draft\nb\n=======\nB\n>>>>>>> assistant\nc\n"))
  ;; Only added: the draft's side is empty, and refusing keeps it as it was.
  (should (equal (org-moor-draft-conflicts "a" "a\nanswer")
                 "a\n<<<<<<< draft\n=======\nanswer\n>>>>>>> assistant\n")))

(ert-deftest org-moor-test-draft-assist ()
  (org-moor-test--with-notes "* Topic\n#+begin_quote Chat\nb\n#+end_quote\n"
    (let* ((org-moor-draft-directory (expand-file-name "drafts/" dir))
           given
           (org-moor-draft-assist-function
            (lambda (context reply)
              (setq given context)
              (funcall reply "B\nadded\n"))))
      (org-moor-draft)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "do it")))
        (org-moor-draft-assist))
      (should (equal (plist-get given :draft) "b"))
      (should (equal (plist-get given :instruction) "do it"))
      (should (equal (plist-get given :heading) "Topic"))
      (should smerge-mode)
      (should-error (org-moor-draft-finish) :type 'user-error)
      (smerge-keep-lower)
      (should (equal (buffer-string) "B\nadded\n"))
      (org-moor-draft-finish))))

(provide 'org-moor-draft-test)
;;; org-moor-draft-test.el ends here
