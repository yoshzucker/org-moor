;;; org-moor-return-test.el --- Tests for org-moor-return  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -l test/org-moor-return-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)

(defconst org-moor-test--anchors-text "\
#+begin_quote before any heading [2026-10-01 Thu 09:00]
#+end_quote
* Topic A
#+begin_quote Chat from Robin [2026-10-15 Thu 09:00]
x
#+end_quote
#+begin_quote Mail from Alex [2026-10-16 Fri 09:00]
y
#+end_quote
** Manual
:PROPERTIES:
:MOOR_ORIGIN: https://example.com/m.pdf
:MOOR_TAKEN: [2026-10-17 Sat 09:00]
:END:
* Topic B
nothing taken
")

(ert-deftest org-moor-test-anchors ()
  (with-temp-buffer
    (org-mode)
    (insert org-moor-test--anchors-text)
    (let* ((anchors (org-moor-anchors-in-buffer))
           (paths (mapcar (lambda (a) (plist-get a :path)) anchors)))
      (should (= (length anchors) 2))
      (should (member "Topic A" paths))
      (should (member "Topic A/Manual" paths))
      (should-not (member "Topic B" paths))
      (let ((a (seq-find (lambda (a) (equal (plist-get a :path) "Topic A")) anchors)))
        (should (= (plist-get a :count) 2))
        (should (equal (format-time-string "%F" (plist-get a :time)) "2026-10-16"))))))

(provide 'org-moor-return-test)
;;; org-moor-return-test.el ends here
