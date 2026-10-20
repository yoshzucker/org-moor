;;; org-moor-place-test.el --- Tests for places, reading and pruning  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-place-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(defmacro org-moor-test--with-source (&rest body)
  "Run BODY in notes whose heading Topic links to a source with a text copy.
`copy' is the copy's path, `record' the source, `orig' the file it was
taken from, and `notes' the notes' buffer, saved."
  (declare (indent 0))
  `(org-moor-test--with-notes "* Topic\n"
     (let* ((orig (expand-file-name "rules.txt" dir))
            (org-moor-move-directories nil)
            (org-moor-files-function (lambda () (list buffer-file-name)))
            (notes (current-buffer)))
       (with-temp-file orig (insert "rule one\nrule two\n"))
       (org-moor-test--titles-as-given
         (let* ((record (org-moor-record (org-moor-test--id-at (org-moor-take-file orig))))
                (copy (plist-get record :copy)))
           (save-buffer)
           (goto-char (point-min))
           ,@body)))))

(ert-deftest org-moor-test-copy-record ()
  "A copy, and what it was made readable as, are known as their source's
by where they lie."
  (org-moor-test--with-source
    (should (equal (plist-get (org-moor-copy-record copy) :id) (plist-get record :id)))
    (let ((md (concat copy ".md")))
      (with-temp-file md)
      (should (equal (plist-get (org-moor-copy-record md) :id) (plist-get record :id))))
    (should-not (org-moor-copy-record orig))))

(ert-deftest org-moor-test-place-string ()
  (let ((record '(:id "ID" :title "Rules")))
    (should (equal (org-moor-place-string record 12 'page) "[[moor:ID::12][Rules p. 12]]\n"))
    (should (equal (org-moor-place-string record 3 'line " * said \n")
                   "[[moor:ID::3][Rules l. 3]]\n#+begin_quote\n,* said\n#+end_quote\n"))))

(ert-deftest org-moor-test-insert-place-where-opened-from ()
  "A place noted in a copy opened from a heading goes into that heading,
taken out to be written in, after the line its point is on."
  (org-moor-test--with-source
    (org-moor-link-follow (format "%s::2" (plist-get record :id)) nil)
    (should (equal buffer-file-name copy))
    (set-mark (point)) (end-of-line) (activate-mark)
    (org-moor-insert-place)
    (let ((dock (window-buffer (next-window))))
      (should (org-dock-origin-file dock))
      (should (equal (with-current-buffer dock (buffer-string))
                     (format "* Topic\n[[moor:%s][rules]]\n[[moor:%s::2][rules l. 2]]\n#+begin_quote\nrule two\n#+end_quote\n"
                             (plist-get record :id) (plist-get record :id))))
      (forward-line -1)
      (org-moor-insert-place)
      (should (string-suffix-p (format "#+end_quote\n[[moor:%s::1][rules l. 1]]\n"
                                       (plist-get record :id))
                               (with-current-buffer dock (buffer-string))))
      (with-current-buffer dock (org-dock-finish)))
    (should (string-match-p "rules l\\. 1" (with-current-buffer notes (buffer-string))))))

(ert-deftest org-moor-test-noter-text ()
  "Reading beside the heading: the copy read-only on the left, the heading
taken out on the right, `i' noting there, `q' writing it back."
  (org-moor-test--with-source
    (goto-char (point-max))
    (insert "* Next\n")
    (save-buffer)
    (goto-char (point-min))
    (org-moor-noter)
    (should (equal buffer-file-name copy))
    (should org-moor-noter-mode)
    (should buffer-read-only)
    (should (eq (key-binding "i") #'org-moor-insert-place))
    (should (= (length (window-list)) 2))
    (let ((dock (window-buffer (next-window))))
      (should (org-dock-origin-file dock))
      (forward-line 1)
      (org-moor-insert-place)
      (should (string-match-p "rules l\\. 2" (with-current-buffer dock (buffer-string))))
      (with-current-buffer dock
        (goto-char (point-max))
        (insert "Second rule.\n"))
      (org-moor-noter-quit)
      (should-not (buffer-live-p dock)))
    (should (= (length (window-list)) 1))
    (with-current-buffer notes
      (should (string-match-p "rules l\\. 2\\]\\]\nSecond rule\\.\n\\* Next" (buffer-string)))
      (should-not (buffer-modified-p)))
    (with-current-buffer (find-file-noselect copy)
      (should-not org-moor-noter-mode)
      (should-not buffer-read-only))))

(ert-deftest org-moor-test-open-source ()
  (org-moor-test--with-source
    (let (opened)
      (let ((org-moor-open-function (lambda (f) (setq opened f)))
            (org-moor-text-extensions nil))
        (org-moor-open-source)
        (should (equal opened copy))
        (org-moor-open-source t)
        (should (equal opened orig))))))

(ert-deftest org-moor-test-set-title ()
  "A source given another title has it in its record, and in the links here
named by the old one, their places kept."
  (org-moor-test--with-source
    (let ((id (plist-get record :id)))
      (goto-char (point-max))
      (insert (format "As [[moor:%s::2][rules l. 2]] says, and [[moor:%s::1][my words]].\n" id id))
      (search-backward "rules l. 2")
      (org-moor-set-title (org-moor--record-at-point) "House rules")
      (should (equal (plist-get (org-moor-record id) :title) "House rules"))
      (should (string-match-p (regexp-quote (format "[[moor:%s][House rules]]" id)) (buffer-string)))
      (should (string-match-p (regexp-quote (format "[[moor:%s::2][House rules l. 2]]" id)) (buffer-string)))
      (should (string-match-p (regexp-quote "[my words]]") (buffer-string))))))

(ert-deftest org-moor-test-set-origin ()
  "The sources linked in a region are given one origin together."
  (org-moor-test--with-source
    (let ((id (plist-get record :id)))
      (org-moor-set-origin (mapcar #'org-moor-record
                                   (org-moor-ids-in (point-min) (point-max)))
                           " https://example.com/shared/rules.zip ")
      (should (equal (plist-get (org-moor-record id) :origin)
                     "https://example.com/shared/rules.zip"))
      (should (equal (plist-get (org-moor-record id) :title)
                     (plist-get record :title))))))

(ert-deftest org-moor-test-prune ()
  "The copies are listed from the sources, with whether anything links to
them; letting one go keeps its record."
  (org-moor-test--with-source
    (let ((copies (org-moor-copies)))
      (should (= (length copies) 1))
      (should (= (plist-get (car copies) :size) (length "rule one\nrule two\n")))
      (should (plist-get (car copies) :linked))
      (org-moor-let-go (car copies))
      (should-not (file-exists-p copy))
      (should-not (org-moor-copies))
      (should (equal (plist-get (org-moor-record (plist-get record :id)) :origin) orig)))))

(provide 'org-moor-place-test)
;;; org-moor-place-test.el ends here
