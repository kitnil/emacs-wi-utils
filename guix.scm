;;; guix.scm --- Guix package for emacs-wi-project

;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>

;; This file is part of Emacs-Guix.

;; Emacs-Guix is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Emacs-Guix is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs-Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains Guix package for development version of
;; Emacs-Guix.  To build or install, run:
;;
;;   guix build --file=guix.scm
;;   guix package --install-from-file=guix.scm

;;; Code:

(use-modules
 (ice-9 popen)
 (ice-9 rdelim)
 (guix build utils)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (gnu packages autotools)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (guix build-system emacs)
 ((guix licenses) #:prefix license:))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Execute 'git ARGS ...' command and return its output without trailing
newspace."
  (with-directory-excursion %source-dir
    (let* ((port   (apply open-pipe* OPEN_READ "git" args))
           (output (read-string port)))
      (close-pipe port)
      (string-trim-right output #\newline))))

(define (current-commit)
  (git-output "log" "-n" "1" "--pretty=format:%H"))

(let ((commit (current-commit)))
  (package
    (name "emacs-wi-utils")
    (version (string-append "0.0.1" "-" (string-take commit 7)))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system emacs-build-system)
    (inputs
     `(("emacs-bui" ,emacs-bui)
       ("emacs-browse-at-remote" ,emacs-browse-at-remote)
       ("emacs-magit" ,emacs-magit)
       ("emacs-ivy" ,emacs-ivy)
       ("emacs-s" ,emacs-s)))
    (synopsis "Ivy procedures for Git project management")
    (description "This package provides several Emacs's Ivy procedures to work
with Git-based projects.")
    (home-page "https://github.com/kitnil/emacs-wi-utils")
    (license license:gpl3+)))

;;; guix.scm ends here
