;;; org-moor.el --- Bring what drifts past into your files, and tie it to a heading  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines
;; URL: https://github.com/yoshzucker/org-moor

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Take a thing in when you meet it, write what it means, tie what you wrote
;; to the source and the place inside it, and get back to all of it in
;; seconds.
;;
;; `org-moor-quote'   quote the clipboard, titled, at point or a chosen heading
;; `org-moor-jump'    go back to any heading something was taken to
;; `org-moor-take-file' take a file: a heading for it, its copy in the store
;; `org-moor-take-url'  take a web page: its URL, and a copy if monolith is there
;; `org-moor-fathom'    draft what a heading's source means, and write it in
;; `org-moor-sweep'     what piled up in downloads and the like, newest first
;; `org-moor-put-away'  move a file into a directory of yours, and take it too
;;
;; See README.org.

;;; Code:

(require 'org-moor-core)
(require 'org-moor-take)
(require 'org-moor-return)
(require 'org-moor-fathom)
(require 'org-moor-sweep)

(provide 'org-moor)
;;; org-moor.el ends here
