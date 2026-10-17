;;; org-moor-place-test.el --- Tests for places and pruning  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L test -l test/org-moor-place-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(defmacro org-moor-test--with-source (&rest body)
  "Run BODY in notes holding one source with a text copy.
`copy' is the copy's path and `id' the source's id."
  (declare (indent 0))
  `(org-moor-test--with-notes "* Topic\n"
     (let* ((org-moor-fathom-directory (expand-file-name "drafts/" dir))
            (orig (expand-file-name "rules.txt" dir))
            (org-moor-move-directories nil)
            (org-moor-files-function (lambda () (list buffer-file-name)))
            (notes (current-buffer)))
       (with-temp-file orig (insert "rule one\nrule two\n"))
       (cl-letf (((symbol-function 'read-string) (lambda (_p &optional d &rest _) d)))
         (let* ((marker (org-moor-take-file orig))
                (id (org-with-point-at marker (org-id-get)))
                (copy (org-moor-copy-path id "rules.txt")))
           (save-buffer)
           (org-id-add-location id buffer-file-name)
           ,@body)))))

(ert-deftest org-moor-test-copy-id ()
  (let ((org-moor-store-directory (make-temp-file "store" t)))
    (unwind-protect
        (let ((f (org-moor-copy-path "6f1c-aa" "m.pdf")))
          (make-directory (file-name-directory f) t)
          (with-temp-file f)
          (should (equal (org-moor-copy-id f) "6f1c-aa"))
          (should-not (org-moor-copy-id (expand-file-name "loose.pdf" org-moor-store-directory))))
      (delete-directory org-moor-store-directory t))))

(ert-deftest org-moor-test-place-string ()
  (should (equal (org-moor-place-string "ID" 12 nil) "[[moor:ID::12][p. 12]]\n"))
  (should (equal (org-moor-place-string "ID" nil " * said \n")
                 "[[moor:ID][source]]\n#+begin_quote\n,* said\n#+end_quote\n")))

(ert-deftest org-moor-test-note-here-collects ()
  (org-moor-test--with-source
    (set-buffer (find-file-noselect copy))
    (goto-char (point-min))
    (set-mark (point)) (end-of-line) (activate-mark)
    (org-moor-note-here)
    (should org-moor-fathom-mode)
    (set-buffer (find-file-noselect copy))
    (deactivate-mark)
    (org-moor-note-here)
    (should org-moor-fathom-mode)
    (should (equal (buffer-string)
                   (format "[[moor:%s][source]]\n#+begin_quote\nrule one\n#+end_quote\n\n[[moor:%s][source]]\n"
                           id id)))
    (org-moor-fathom-finish)
    (with-current-buffer notes
      (should (string-match-p (regexp-quote "#+begin_quote\nrule one") (buffer-string))))))

(ert-deftest org-moor-test-open-source ()
  (org-moor-test--with-source
    (let (opened)
      (let ((org-moor-open-function (lambda (f) (setq opened f))))
        (goto-char (point-min))
        (org-moor-open-source)
        (should (equal opened copy))
        (org-moor-open-source t)
        (should (equal opened orig))))))

(ert-deftest org-moor-test-prune ()
  (org-moor-test--with-source
    (let ((copies (org-moor-copies)))
      (should (= (length copies) 1))
      (should (= (plist-get (car copies) :size) (length "rule one\nrule two\n")))
      (org-moor-let-go (plist-get (car copies) :marker))
      (should-not (file-exists-p copy))
      (should-not (org-moor-copies))
      (goto-char (point-min))
      (should (search-forward "MOOR_ORIGIN" nil t))
      (should-not (save-excursion (goto-char (point-min)) (search-forward "MOOR_FILE" nil t))))))

(provide 'org-moor-place-test)
;;; org-moor-place-test.el ends here
