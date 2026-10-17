;;; org-moor-core-test.el --- Tests for org-moor-core  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -l test/org-moor-core-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor-core)

(ert-deftest org-moor-test-store-path ()
  (let ((org-moor-store-directory "/store/"))
    (should (equal (org-moor-store-path "6f1c-aa") "/store/6f/1c-aa/"))
    (should (equal (org-moor-copy-path "6f1c-aa" "m.pdf") "/store/6f/1c-aa/m.pdf"))
    (should-error (org-moor-store-path "6"))))

(ert-deftest org-moor-test-link-parse ()
  (should (equal (org-moor-link-parse "6f1c") '("6f1c" . nil)))
  (should (equal (org-moor-link-parse "6f1c::12") '("6f1c" . 12)))
  (should-error (org-moor-link-parse "6f1c::p12")))

(ert-deftest org-moor-test-source-roundtrip ()
  (let* ((org-moor-store-directory (make-temp-file "moor" t))
         (org-id-locations-file (make-temp-file "moor-ids"))
         (file (expand-file-name "n.org" org-moor-store-directory)))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (insert "* Manual\n")
          (goto-char (point-min))
          (should-not (org-moor-source-p))
          (let ((id (org-moor-set-source "https://example.com/m.pdf" 'chat "m.pdf")))
            (should (org-moor-source-p))
            (let ((s (org-moor-source)))
              (should (equal (plist-get s :id) id))
              (should (equal (plist-get s :via) "chat"))
              (should-not (plist-get s :copy))
              (make-directory (org-moor-store-path id) t)
              (with-temp-file (org-moor-copy-path id "m.pdf"))
              (should (plist-get (org-moor-source) :copy))
              (set-buffer-modified-p nil)
              (kill-buffer))))
      (delete-directory org-moor-store-directory t)
      (delete-file org-id-locations-file))))

(provide 'org-moor-core-test)
;;; org-moor-core-test.el ends here
