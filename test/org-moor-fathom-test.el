;;; org-moor-fathom-test.el --- Tests for org-moor-fathom  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L test -l test/org-moor-fathom-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(ert-deftest org-moor-test-fathom-quotes-text ()
  (with-temp-buffer
    (org-mode)
    (insert "* T\n#+begin_quote A [2026-10-17 Sat]\none\n#+end_quote\n"
            "#+begin_quote B\ntwo\n#+end_quote\n** Child\n#+begin_quote C\nno\n#+end_quote\n")
    (goto-char (point-min))
    (should (equal (org-moor-fathom-quotes-text) "one\n\ntwo"))))

(ert-deftest org-moor-test-fathom-pdf-pages ()
  (cl-letf (((symbol-function 'org-moor--process-string)
             (lambda (&rest _) "first\f\fthird\f")))
    (should (equal (org-moor-fathom-pdftotext "m.pdf" "ID")
                   "[[moor:ID::1][p. 1]]\nfirst\n\n[[moor:ID::3][p. 3]]\nthird"))))

(ert-deftest org-moor-test-fathom-write-and-abort ()
  (org-moor-test--with-notes "* Topic\n#+begin_quote Chat\nraw words\n#+end_quote\n** Child\n"
    (let ((org-moor-fathom-directory (expand-file-name "drafts/" dir))
          (notes (current-buffer)))
      (org-moor-fathom)
      (should org-moor-fathom-mode)
      (should (equal (buffer-string) "raw words"))
      (let ((draft buffer-file-name))
        (erase-buffer)
        (insert "Clear sentence.")
        (org-moor-fathom-finish)
        (should-not (file-exists-p draft)))
      (with-current-buffer notes
        (should (string-match-p "#\\+end_quote\nClear sentence\\.\n\\*\\* Child"
                                (buffer-string)))
        (goto-char (point-min))
        (org-moor-fathom)
        (set-buffer-modified-p nil)
        (org-moor-fathom-abort))
      (should (eq (current-buffer) notes)))))

(ert-deftest org-moor-test-fathom-fit-headings ()
  (should (equal (org-moor-fathom-fit-headings "no headings" 2) "no headings"))
  (should (equal (org-moor-fathom-fit-headings "* A\ntext\n** B\n* C\n" 2)
                 "*** A\ntext\n**** B\n*** C\n"))
  (should (equal (org-moor-fathom-fit-headings "*** A\n**** B\n" 1)
                 "** A\n*** B\n")))

(ert-deftest org-moor-test-fathom-pandoc-drops-anchors ()
  (cl-letf (((symbol-function 'org-moor--process-string)
             (lambda (&rest _) "* H\n:PROPERTIES:\n:CUSTOM_ID: h\n:END:\nbody\n")))
    (should (equal (org-moor-fathom-pandoc "a.html" "X") "* H\nbody\n"))))

(provide 'org-moor-fathom-test)
;;; org-moor-fathom-test.el ends here
