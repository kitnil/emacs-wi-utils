;;; wi-projects.el --- wi project utilities   -*- lexical-binding: t; -*-

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
(require 'ivy) ;used in `wi-ivy-find-project'

(defvar wi-groups-direcotory "~/majordomo")

(defvar wi-projects-directories '("~/src" "~/archive/src"))

(defun wi-project-candidates-groups-direcotory ()
  (seq-filter #'file-directory-p
              (apply #'append
                     (mapcar (lambda (dir)
                               (cddr ;skip "." and ".."
                                (directory-files dir t)))
                             (seq-filter #'file-directory-p
                                         (cddr ;skip "." and ".."
                                          (directory-files (expand-file-name wi-groups-direcotory)
                                                           t)))))))

(defun wi-project-candidates ()
  (delete-dups
   (append (wi-project-candidates-groups-direcotory)
           (apply #'append
                  (mapcar (lambda (directory)
                            (seq-filter #'file-directory-p
                                        (cddr ;skip "." and ".."
                                         (directory-files (expand-file-name directory) t))))
                          wi-projects-directories)))))

(defun wi-project-ivy ()
  "Find wi project."
  (interactive)
  (find-file
   (ivy-completing-read "Directory: " (wi-project-candidates))))

(defun wi-project-browse-at-remote ()
  "Call `browse-at-remote' with project."
  (interactive)
  (let ((default-directory (ivy-completing-read "Directory: " (wi-project-candidates))))
    (find-file ".")
    (browse-at-remote)))

(defun wi-project-browse-at-remote ()
  "Call `browse-at-remote' with project."
  (interactive)
  (let ((default-directory (ivy-completing-read "Directory: " (wi-project-candidates))))
    (find-file ".")
    (browse-at-remote)))

(defun wi-project->entry (project)
  `((id   . ,project)
    (name . ,project)))

(defun wi-project-get-entries ()
  (mapcar 'wi-project->entry (wi-project-candidates)))

(defun wi-project-list-describe (project)
  (interactive)
  (magit-status project))

(bui-define-interface wi-project list
  :buffer-name "*wi Project*"
  :get-entries-function 'wi-project-get-entries
  :describe-function 'wi-project-list-describe
  :hint 'wi-project-list-hint
  :format '((name nil 30 t))
  :sort-key '(name))

(defun wi-project ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'wi-project 'list))

(provide 'wi-projects)
;;; wi-projects.el ends here
