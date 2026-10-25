;;; org-moor-place-test.el --- Tests for places, reading and pruning  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-place-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)
(require 'eww)

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
                   "[[moor:ID::3][Rules l. 3]]\n#+begin_quote\n,* said\n#+end_quote\n"))
    (should (equal (org-moor-place-string record 12 'page nil t) "[[moor:ID::12][p. 12]]\n"))
    (should (equal (org-moor-place-string record "rule two" 'text nil t)
                   "[[moor:ID::rule two][“rule two”]]\n"))
    (should (equal (org-moor-place-string record nil nil nil t) "[[moor:ID][Rules]]\n"))))

(ert-deftest org-moor-test-place-alone-in-a-file-so-said ()
  "A file `org-moor-place-alone-functions' says so of names a place by
itself, and tells an agent to; any other, by the title and the place."
  (let ((org-moor-place-alone-functions
         (list (lambda (file) (equal (file-name-nondirectory file) "truss.org")))))
    (should (org-moor-place-alone-p "/x/truss.org"))
    (should-not (org-moor-place-alone-p "/x/notes.org"))
    (let ((text (org-moor-request-text '(:origin "/x/truss.org"))))
      (should (string-search "[[moor:ID::12][p. 12]]" text))
      (should (string-search "named once" text)))
    (let ((text (org-moor-request-text '(:origin "/x/notes.org"))))
      (should (string-search "[[moor:ID::12][TITLE p. 12]]" text))
      (should-not (string-search "named once" text)))))

(ert-deftest org-moor-test-text-place ()
  "A text place is the start of what was selected, on one line, with no
brackets, and is named by it."
  (let ((org-moor-text-place-length 12)
        (record '(:id "ID" :title "Page")))
    (should (equal (org-moor-text-place "  rule\n [two] of three ") "rule two of"))
    (should-not (org-moor-text-place " [ ] "))
    (should (equal (org-moor-place-string record "rule two" 'text)
                   "[[moor:ID::rule two][Page \u201crule two\u201d]]\n"))))

(ert-deftest org-moor-test-page-in-eww ()
  "A page is read in EWW, found at a text place, and a place noted there
is the text selected."
  (org-moor-test--with-notes "* Topic\n"
    (let* ((orig (expand-file-name "page.html" dir))
           ;; A batch Emacs's frame is narrow enough to break a line.
           (shr-width 70)
           (shr-use-fonts nil)
           (org-moor-move-directories nil)
           (org-moor-files-function (lambda () (list buffer-file-name))))
      (with-temp-file orig
        (insert "<html><body><p>rule one</p><p>rule two</p></body></html>"))
      (org-moor-test--titles-as-given
        (let* ((record (org-moor-record (org-moor-test--id-at (org-moor-take-file orig))))
               (copy (plist-get record :copy)))
          (save-buffer)
          (goto-char (point-min))
          (org-moor-link-follow (format "%s::rule two" (plist-get record :id)) nil)
          (with-timeout (5 (ert-fail "EWW drew nothing"))
            (while (not (and (derived-mode-p 'eww-mode) org-moor--read-for))
              (accept-process-output nil 0.05)))
          (should (equal (org-moor-shown-file) copy))
          (should (looking-at "rule two"))
          (set-mark (point)) (end-of-line) (activate-mark)
          (org-moor-insert-place)
          (should (string-search
                   (format "[[moor:%s::rule two][page \u201crule two\u201d]]\n#+begin_quote\nrule two\n"
                           (plist-get record :id))
                   (with-current-buffer (window-buffer (next-window)) (buffer-string)))))))))

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
  "A copy is opened by `org-open-file', given the prefix as a `file:' link
is; the origin, by its own command."
  (org-moor-test--with-source
    (let (opened)
      (cl-letf (((symbol-function 'org-open-file)
                 (lambda (f &optional how &rest _) (setq opened (list f how)))))
        (org-moor-open-source)
        (should (equal opened (list copy nil)))
        (org-moor-open-source '(16))
        (should (equal opened (list copy '(16))))
        (org-moor-link-follow (format "%s::2" (plist-get record :id)) '(16))
        (should (equal opened (list copy '(16))))
        (org-moor-open-origin)
        (should (equal opened (list orig nil)))))))

(ert-deftest org-moor-test-open-origin-url-encoded ()
  "A URL origin reaches the browser percent-encoded, once."
  (let (opened org-moor-office-url-schemes)
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq opened u))))
      (org-moor--open-origin "https://example.com/Shared Documents/予算.xlsx")
      (should (equal opened "https://example.com/Shared%20Documents/%E4%BA%88%E7%AE%97.xlsx"))
      (org-moor--open-origin opened)
      (should (equal opened "https://example.com/Shared%20Documents/%E4%BA%88%E7%AE%97.xlsx")))))

(ert-deftest org-moor-test-open-origin-office ()
  "An Office document's URL origin opens in its application, not the browser."
  (let (browsed opened)
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq browsed u)))
              ((symbol-function 'org-moor--open-externally) (lambda (u) (setq opened u))))
      (org-moor--open-origin "https://example.com/Shared Documents/予算.XLSX?web=0")
      (should (equal opened "ms-excel:ofe|u|https://example.com/Shared%20Documents/%E4%BA%88%E7%AE%97.XLSX?web=0"))
      (should-not browsed)
      (org-moor--open-origin "https://example.com/view.aspx?file=a.docx")
      (should (equal browsed "https://example.com/view.aspx?file=a.docx")))))

(ert-deftest org-moor-test-noter-office ()
  "A Word document is read beside its heading as what pandoc makes it,
and its lines are noted."
  (skip-unless (executable-find "pandoc"))
  (org-moor-test--with-notes "* Topic\n"
    (let* ((md (expand-file-name "made.md" dir))
           (orig (expand-file-name "rules.docx" dir))
           (org-moor-move-directories nil)
           (org-moor-files-function (lambda () (list buffer-file-name))))
      (with-temp-file md (insert "rule one\n\nrule two\n"))
      (call-process "pandoc" nil nil nil "-o" orig md)
      (org-moor-test--titles-as-given
        (let* ((record (org-moor-record (org-moor-test--id-at (org-moor-take-file orig))))
               (copy (plist-get record :copy)))
          (save-buffer)
          (goto-char (point-min))
          (org-moor-noter)
          (should (equal buffer-file-name (concat copy ".md")))
          (should org-moor-noter-mode)
          (should (re-search-forward "rule two" nil t))
          (org-moor-insert-place)
          (should (string-search
                   (format "[[moor:%s::%d][rules l. %d]]" (plist-get record :id)
                           (line-number-at-pos) (line-number-at-pos))
                   (with-current-buffer (window-buffer (next-window)) (buffer-string))))
          (org-moor-noter-quit))))))

(ert-deftest org-moor-test-set-copy ()
  "A file given to a source becomes its copy: moved from where things pile
up, the old copy and what it was made readable as let go."
  (org-moor-test--with-source
    (let* ((pile (expand-file-name "pile/" dir))
           (org-moor-move-directories (list pile))
           (new (expand-file-name "rules v2.txt" pile))
           (md (concat copy ".md")))
      (make-directory pile)
      (with-temp-file new (insert "rule three\n"))
      (with-temp-file md)
      (cl-letf (((symbol-function 'y-or-n-p) #'always))
        (org-moor-set-copy record new))
      (let ((now (org-moor-record (plist-get record :id))))
        (should (equal (file-name-nondirectory (plist-get now :copy)) "rules v2.txt"))
        (should (file-exists-p (plist-get now :copy))))
      (should-not (file-exists-p new))
      (should-not (file-exists-p copy))
      (should-not (file-exists-p md)))))

(ert-deftest org-moor-test-keep-copy ()
  "Sources linked under a heading with only their origins are given copies
from them: a URL fetched, a path copied; one with a copy is left alone."
  (org-moor-test--with-source
    (let* ((sources (org-moor-sources))
           (page (org-moor-new-source "Wiki" "https://example.com/wiki" 'web sources))
           (file (org-moor-new-source "Rules again" orig 'file sources)))
      (goto-char (point-max))
      (insert (org-moor-link page) "\n" (org-moor-link file) "\n")
      (goto-char (point-min))
      (org-moor-test--serving "text/html" nil "<html>wiki</html>"
        (org-moor-keep-copy (org-moor--records-to-keep))
        (should (equal fetched "https://example.com/wiki")))
      (should (equal (plist-get (org-moor-record (plist-get page :id)) :file) "page.html"))
      (let ((now (org-moor-record (plist-get file :id))))
        (should (equal (file-name-nondirectory (plist-get now :copy)) "rules.txt"))
        (should (file-exists-p orig)))
      (should (equal (plist-get (org-moor-record (plist-get record :id)) :copy) copy)))))

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
