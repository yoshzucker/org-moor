;;; org-moor-place-test.el --- Tests for places, reading and pruning  -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs --batch -Q -L . -L ../org-dock -L test -l test/org-moor-place-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-moor)
(require 'org-moor-take-test)

(defmacro org-moor-test--with-source (&rest body)
  "Run BODY in notes whose heading Topic links to a source with a text replica.
`replica' is the replica's path, `record' the source, `orig' the file it was
taken from, and `notes' the notes' buffer, saved."
  (declare (indent 0))
  `(org-moor-test--with-notes "* Topic\n"
     (let* ((orig (expand-file-name "rules.txt" dir))
            (org-moor-move-directories nil)
            (org-moor-files-function (lambda () (list buffer-file-name)))
            (notes (current-buffer)))
       ;; Its lines end in LF alone, as they would not on Windows by default.
       (let ((coding-system-for-write 'utf-8-unix))
         (with-temp-file orig (insert "rule one\nrule two\n")))
       (org-moor-test--titles-as-given
         (let* ((record (org-moor-record (org-moor-test--id-at (org-moor-take-file orig))))
                (replica (plist-get record :replica)))
           (save-buffer)
           (goto-char (point-min))
           ,@body)))))

(ert-deftest org-moor-test-replica-record ()
  "A replica, and what it was made readable as, are known as their source's
by where they lie."
  (org-moor-test--with-source
    (should (equal (plist-get (org-moor-replica-record replica) :id) (plist-get record :id)))
    (let ((md (concat replica ".md")))
      (with-temp-file md)
      (should (equal (plist-get (org-moor-replica-record md) :id) (plist-get record :id))))
    (should-not (org-moor-replica-record orig))))

(ert-deftest org-moor-test-place-string ()
  "Words quoted are linked to by their start, and quoted under the link;
where the quote is the place, the link has none."
  (let ((record '(:id "ID" :title "Rules"))
        (org-moor-text-place-length 12))
    (should (equal (org-moor-place-string record " * said [two] of three \n")
                   "[[moor:ID::* said two o][Rules “* said two o”]]\n#+begin_quote\n,* said [two] of three\n#+end_quote\n"))
    (should (equal (org-moor-place-string record "rule two" t)
                   "[[moor:ID][Rules]]\n#+begin_quote\nrule two\n#+end_quote\n"))))

(ert-deftest org-moor-test-text-place ()
  "A place is the start of what was selected, on one line, with no brackets."
  (let ((org-moor-text-place-length 12))
    (should (equal (org-moor-text-place "  rule\n [two] of three ") "rule two of"))
    (should-not (org-moor-text-place " [ ] "))))

(ert-deftest org-moor-test-insert-quote-from-the-clipboard ()
  "Words copied from a replica read in its own program are quoted here,
linked to the source the heading links to."
  (org-moor-test--with-source
    (forward-line 1)
    (end-of-line)
    (cl-letf (((symbol-function 'org-moor--clipboard) (lambda () "rule two")))
      (call-interactively #'org-moor-insert-quote))
    (should (equal (buffer-string)
                   (format "* Topic\n[[moor:%s][rules]]\n[[moor:%s::rule two][rules “rule two”]]\n#+begin_quote\nrule two\n#+end_quote\n"
                           (plist-get record :id) (plist-get record :id))))
    (let ((org-moor-place-by-quote-functions (list (lambda (_) t))))
      (goto-char (point-max))
      (org-moor-insert-quote record "rule one")
      (should (string-suffix-p (format "[[moor:%s][rules]]\n#+begin_quote\nrule one\n#+end_quote\n"
                                       (plist-get record :id))
                               (buffer-string))))
    (should-error (org-moor-insert-quote record " ") :type 'user-error)))

(ert-deftest org-moor-test-insert-place-where-opened-from ()
  "Words selected in a replica opened from a heading are quoted in that
heading, taken out to be written in, after the line its point is on;
nothing selected, nothing is noted."
  (org-moor-test--with-source
    (org-moor-link-follow (format "%s::rule two" (plist-get record :id)) nil)
    (should (equal buffer-file-name replica))
    (should-error (org-moor-insert-place) :type 'user-error)
    (set-mark (point)) (end-of-line) (activate-mark)
    (org-moor-insert-place)
    (let ((dock (window-buffer (next-window))))
      (should (org-dock-origin-file dock))
      (should (equal (with-current-buffer dock (buffer-string))
                     (format "* Topic\n[[moor:%s][rules]]\n[[moor:%s::rule two][rules “rule two”]]\n#+begin_quote\nrule two\n#+end_quote\n"
                             (plist-get record :id) (plist-get record :id))))
      (with-current-buffer dock (org-dock-finish)))
    (should (string-match-p "rules “rule two”" (with-current-buffer notes (buffer-string))))))

(ert-deftest org-moor-test-noter-text ()
  "Reading beside the heading: the replica read-only on the left, the heading
taken out on the right, `i' noting there, `q' writing it back."
  (org-moor-test--with-source
    (goto-char (point-max))
    (insert "* Next\n")
    (save-buffer)
    (goto-char (point-min))
    (org-moor-noter)
    (should (equal buffer-file-name replica))
    (should org-moor-noter-mode)
    (should buffer-read-only)
    (should (eq (key-binding "i") #'org-moor-insert-place))
    (should (= (length (window-list)) 2))
    (let ((dock (window-buffer (next-window))))
      (should (org-dock-origin-file dock))
      (forward-line 1)
      (set-mark (point)) (end-of-line) (activate-mark)
      (org-moor-insert-place)
      (should (string-match-p "rules “rule two”" (with-current-buffer dock (buffer-string))))
      (with-current-buffer dock
        (goto-char (point-max))
        (insert "Second rule.\n"))
      (org-moor-noter-quit)
      (should-not (buffer-live-p dock)))
    (should (= (length (window-list)) 1))
    (with-current-buffer notes
      (should (string-match-p "#\\+end_quote\nSecond rule\\.\n\\* Next" (buffer-string)))
      (should-not (buffer-modified-p)))
    (with-current-buffer (find-file-noselect replica)
      (should-not org-moor-noter-mode)
      (should-not buffer-read-only))))

(ert-deftest org-moor-test-open-replica ()
  "A text replica is opened in Emacs, as it is seen; given two prefixes,
by the program the system opens it with.  The origin, a file: URL, is
opened as the file."
  (org-moor-test--with-source
    (let (opened)
      (cl-letf (((symbol-function 'org-open-file)
                 (lambda (f &optional how &rest _) (setq opened (list f how)))))
        (save-window-excursion
          (org-moor-open-replica)
          (should (equal buffer-file-name replica)))
        (should-not opened)
        (org-moor-open-replica '(16))
        (should (equal opened (list replica '(16))))
        (cl-letf (((symbol-function 'kill-new) #'ignore))
          (org-moor-link-follow (format "%s::rule two" (plist-get record :id)) '(16)))
        (should (equal opened (list replica '(16))))
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
  "A Word document is not read beside its heading as what pandoc makes of
it: it is opened in its own program, to be quoted from the clipboard."
  (org-moor-test--with-notes "* Topic\n"
    (let* ((orig (expand-file-name "rules.docx" dir))
           (org-moor-move-directories nil)
           (org-moor-machine-readable-converters nil)
           (opened nil))
      (with-temp-file orig (insert "d"))
      (org-moor-test--titles-as-given
        (let* ((record (org-moor-record (org-moor-test--id-at (org-moor-take-file orig)))))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'org-open-file) (lambda (f &rest _) (setq opened f))))
            (org-moor-noter))
          (should (equal opened (plist-get record :replica)))
          (should (= (length (window-list)) 1)))))))

(ert-deftest org-moor-test-set-replica ()
  "A file given to a source becomes its replica: moved from where things pile
up, the old replica let go."
  (org-moor-test--with-source
    (let* ((pile (expand-file-name "pile/" dir))
           (org-moor-move-directories (list pile))
           (new (expand-file-name "rules v2.txt" pile)))
      (make-directory pile)
      (with-temp-file new (insert "rule three\n"))
      (cl-letf (((symbol-function 'y-or-n-p) #'always))
        (org-moor-set-replica record new))
      (let ((now (org-moor-record (plist-get record :id))))
        (should (equal (file-name-nondirectory (plist-get now :replica)) "rules v2.txt"))
        (should (file-exists-p (plist-get now :replica)))
        (should (equal (plist-get now :replica-via) "file")))
      (should-not (file-exists-p new))
      (should-not (file-exists-p replica)))))

(ert-deftest org-moor-test-keep-replica ()
  "Sources linked under a heading with only their origins are given replicas:
a file copied from where it is; those on the web listed, to have the
browser save each.  One with a replica is left alone."
  (org-moor-test--with-source
    (let* ((sources (org-moor-sources))
           (page (org-moor-new-source "Wiki" "https://example.com/wiki" sources))
           (file (org-moor-new-source "Rules again" orig sources))
           (listed nil))
      (goto-char (point-max))
      (insert (org-moor-link page) "\n" (org-moor-link file) "\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-moor-awaiting) (lambda (records) (setq listed records))))
        (org-moor-keep-replica (org-moor--records-to-keep)))
      (should (equal (mapcar (lambda (r) (plist-get r :id)) listed) (list (plist-get page :id))))
      (let ((now (org-moor-record (plist-get file :id))))
        (should (equal (file-name-nondirectory (plist-get now :replica)) "rules.txt"))
        (should (file-exists-p orig)))
      (should (equal (plist-get (org-moor-record (plist-get record :id)) :replica) replica)))))

(ert-deftest org-moor-test-awaiting ()
  "The sources awaited are listed, and RET opens one in the browser; one
whose replica has come is listed no more."
  (org-moor-test--with-notes ""
    (let* ((wiki (org-moor-new-source "Wiki" "https://example.com/wiki"))
           (opened nil))
      (org-moor-awaiting (list wiki))
      (unwind-protect
          (with-current-buffer "*org-moor awaiting*"
            (should (derived-mode-p 'org-moor-awaiting-mode))
            (goto-char (point-min))
            (should (search-forward "Wiki" nil t))
            (should (search-forward "https://example.com/wiki" nil t))
            (cl-letf (((symbol-function 'org-moor-open-in-browser)
                       (lambda (r) (setq opened (plist-get r :id)))))
              (org-moor-awaiting-open))
            (should (equal opened (plist-get wiki :id)))
            (with-temp-file (expand-file-name "page.html" (plist-get wiki :dir)))
            (org-moor-record-set wiki :replica-file "page.html")
            (revert-buffer)
            (goto-char (point-min))
            (should-not (search-forward "Wiki" nil t)))
        (kill-buffer "*org-moor awaiting*")))))

(ert-deftest org-moor-test-set-title ()
  "A source given another title has it in its record, and in the links here
named by the old one, their places kept."
  (org-moor-test--with-source
    (let ((id (plist-get record :id)))
      (goto-char (point-max))
      (insert (format "As [[moor:%s::rule two][rules “rule two”]] says, and [[moor:%s::rule one][my words]].\n" id id))
      (search-backward "rules “rule two”")
      (org-moor-set-title (org-moor--record-at-point) "House rules")
      (should (equal (plist-get (org-moor-record id) :title) "House rules"))
      (should (string-match-p (regexp-quote (format "[[moor:%s][House rules]]" id)) (buffer-string)))
      (should (string-match-p (regexp-quote (format "[[moor:%s::rule two][House rules “rule two”]]" id)) (buffer-string)))
      (should (string-match-p (regexp-quote "[my words]]") (buffer-string))))))

(ert-deftest org-moor-test-set-origin ()
  "The sources linked in a region are given one origin together."
  (org-moor-test--with-source
    (let ((id (plist-get record :id)))
      (org-moor-set-origin (mapcar #'org-moor-record
                                   (org-moor-ids-in (point-min) (point-max)))
                           " https://example.com/shared/rules.zip ")
      (should (equal (plist-get (org-moor-record id) :origin-url)
                     "https://example.com/shared/rules.zip"))
      (should (equal (plist-get (org-moor-record id) :title)
                     (plist-get record :title))))))

(ert-deftest org-moor-test-prune ()
  "The replicas are listed from the sources, with whether anything links to
them; letting one go keeps its record."
  (org-moor-test--with-source
    (let ((replicas (org-moor-replicas)))
      (should (= (length replicas) 1))
      (should (= (plist-get (car replicas) :size) (length "rule one\nrule two\n")))
      (should (plist-get (car replicas) :linked))
      (org-moor-let-go (car replicas))
      (should-not (file-exists-p replica))
      (should-not (org-moor-replicas))
      (should (equal (plist-get (org-moor-record (plist-get record :id)) :origin-url)
                     (org-moor-path-url orig))))))

(provide 'org-moor-place-test)
;;; org-moor-place-test.el ends here
