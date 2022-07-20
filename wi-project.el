;;; wi-projects.el --- wi project utilities   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Oleg Pykhalov

;; Author: Oleg Pykhalov <go.wigust@gmail.com>
;; Keywords: tools

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

;; This Emacs library provides utilities to control Git projects.

;;; Code:

(require 'bui)
(require 'ivy) ;used in `wi-ivy-find-project'
(require 'seq)
(require 'browse-at-remote)
(require 'magit)

(defgroup wi-project nil
  "Settings for `wi-project'."
  :prefix "wi-project-"
  :group  'wi-project)

(defcustom wi-groups-direcotories-git
  nil "Filter projects by .git directory."
  :type 'boolean
  :group 'wi-project)

(defcustom wi-groups-direcotories
  nil "Directories with group based project layout."
  :type 'string
  :group 'wi-project)

(defcustom wi-projects-directories
  nil "Directories with subdirectories containing Git repositories."
  :type '(list string)
  :group 'wi-project)

(defun wi-project-candidates-groups-direcotory ()
  "Return a list of projects with group based layout."
  (seq-reduce #'append
              (mapcar (lambda (wi-groups-direcotory)
                        (seq-filter (if wi-groups-direcotories-git
                                        #'(lambda (directory)
                                            (and (file-directory-p directory)
                                                 (file-exists-p (concat (expand-file-name directory) "/.git"))))
                                      #'file-directory-p)
                                    (apply #'append
                                           (mapcar (lambda (dir)
                                                     (cddr ;skip "." and ".."
                                                      (directory-files dir t)))
                                                   (seq-filter #'file-directory-p
                                                               (cddr ;skip "." and ".."
                                                                (directory-files (expand-file-name wi-groups-direcotory)
                                                                                 t)))))))
                      wi-groups-direcotories)
              '()))

(defun wi-project-candidates ()
  "Return a list of projects."
  (let ((project-dirs (apply #'append
                             (mapcar (lambda (directory)
                                       (seq-filter #'file-directory-p
                                                   (cddr ;skip "." and ".."
                                                    (directory-files (expand-file-name directory) t))))
                                     wi-projects-directories))))
    (if wi-groups-direcotories
        (delete-dups
         (append (wi-project-candidates-groups-direcotory)
                 project-dirs)))))

;;;###autoload
(defun wi-project-ivy ()
  "Find wi project."
  (interactive)
  (find-file
   (ivy-completing-read "Directory: " (wi-project-candidates))))

;;;###autoload
(defun wi-project-browse-at-remote ()
  "Call `browse-at-remote' with project."
  (interactive)
  (let ((default-directory (ivy-completing-read "Directory: " (wi-project-candidates))))
    (find-file ".")
    (browse-at-remote)))

(defun wi-project->entry (project)
  "Return a project's entry for PROJECT."
  `((id   . ,project)
    (name . ,project)))

(defun wi-project-get-entries ()
  "Return a list of project entries."
  (mapcar 'wi-project->entry (wi-project-candidates)))

;;;###autoload
(defun wi-project-list-describe ()
  "Open `magit-status' for project."
  (interactive)
  (magit-status-internal
   (ivy-completing-read "Directory: " (wi-project-candidates))))

(bui-define-interface wi-project list
  :buffer-name "*wi Project*"
  :get-entries-function 'wi-project-get-entries
  :describe-function 'wi-project-list-describe
  :hint 'wi-project-list-hint
  :format '((name nil 30 t))
  :sort-key '(name))

;;;###autoload
(defun wi-project ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'wi-project 'list))

(provide 'wi-project)

;;; wi-project.el ends here
