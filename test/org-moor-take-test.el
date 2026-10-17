;;; org-moor-take-test.el --- Tests for org-moor-take  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -l test/org-moor-take-test.el -f ert-run-tests-batch-and-exit

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

(provide 'org-moor-take-test)
;;; org-moor-take-test.el ends here
