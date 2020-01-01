;;; mj-projects.el --- Majordomo project utilities   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Oleg Pykhalov

;; Author: Oleg Pykhalov <go.wigust@gmail.com>
;; Keywords: tools, internal

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

;; This Emacs library provides utilities to control Majodromo Git
;; projects.

;;; Code:

(require 'bui)
(require 'ivy) ;used in `majordomo-ivy-find-project'

(defvar mj-projects-direcotory "~/majordomo")

(defun mj-project-candidates ()
  (delete-dups
   (seq-filter #'file-directory-p
               (apply #'append
                      (mapcar (lambda (dir)
                                (cddr ;skip "." and ".."
                                 (directory-files dir t)))
                              (seq-filter #'file-directory-p
                                          (cddr ;skip "." and ".."
                                           (directory-files (expand-file-name mj-projects-direcotory) t))))))))

(defun mj-project-ivy ()
  "Find Majordomo project."
  (interactive)
  (find-file
   (ivy-completing-read "Directory: " (mj-project-candidates))))

(defun mj-project->entry (project)
  `((id   . ,project)
    (name . ,project)))

(defun mj-project-get-entries ()
  (mapcar 'mj-project->entry (mj-project-candidates)))

(bui-define-interface mj-project list
  :buffer-name "*Majordomo Project*"
  :get-entries-function 'mj-project-get-entries
  :describe-function 'mj-project-list-describe
  :hint 'mj-project-list-hint
  :format '((name nil 30 t))
  :sort-key '(name))

(defun mj-project ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'mj-project 'list))

(provide 'mj-projects)
;;; mj-projects.el ends here
