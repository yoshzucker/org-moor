;;; org-moor.el --- Bring what drifts past into your files, and tie it to a heading  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yoshzucker

;; Author: yoshzucker
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (org "9.6") (org-dock "0.1"))
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
;; `org-moor-jump'       go back to any heading something was taken to
;; `org-moor-take-file'  take a file: its copy in the sources, a link to it here
;; `org-moor-take-url'   take a page or a file on the web, kept as what it sends
;; `org-moor-take-clipboard' take the clipboard's text or image, as a source
;; `org-moor-sweep'      what piled up in downloads and the like, newest first
;; `org-moor-put-away'   move a file into a directory of yours, and take it too
;; `org-moor-noter'      read a source beside its heading, taken out to note in
;; `org-moor-insert-place' note the place being read, in its heading
;; `org-moor-open-source' the copy of a source the heading links to, C-u its origin
;; `org-moor-retitle'    give a source another title
;; `org-moor-prune'      choose which copies to let go, largest first
;;
;; See README.org.

;;; Code:

;; Every command's autoload names this file, not the one defining it, so
;; that whichever command comes first loads all of the package -- and
;; `with-eval-after-load' on `org-moor' always runs.

(require 'org-moor-core)
(require 'org-moor-take)
(require 'org-moor-return)
(require 'org-moor-sweep)
(require 'org-moor-place)
(require 'org-moor-prune)

(provide 'org-moor)
;;; org-moor.el ends here
