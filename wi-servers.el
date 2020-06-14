;;; wi-servers.el --- wi servers utilities   -*- lexical-binding: t; -*-

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

;; This Emacs library provides Bui interface to control remote
;; GNU/Linux servers.

;;; Code:

(require 'bui)
(require 's)
(require 'seq)
(require 'subr-x)

(defcustom wi-known-hosts
  "~/.ssh/known_hosts" "SSH known hosts file."
  :type 'string
  :group 'wi-servers)

(defcustom wi-servers-domain
  ".intr" "Domain for `wi-hosts--candidates'."
  :type 'string
  :group 'wi-servers)

(defun wi-server->entry (server)
  "Return alist id and server name for SERVER."
  `((id   . ,server)
    (name . ,server)))

(defun wi-hosts--candidates ()
  "List host candidates with a domain."
  (seq-uniq
   (seq-filter (lambda (str)
                 (string-suffix-p wi-servers-domain str))
               (mapcar (lambda (str)
                         (car (split-string str " ")))
                       (split-string (with-temp-buffer
                                       (insert-file-contents (expand-file-name wi-known-hosts))
                                       (buffer-string))
                                     "\n")))))

(defun wi-servers-get-entries ()
  "Get entries for `wi-hosts--candidates'."
  (mapcar 'wi-server->entry (wi-hosts--candidates)))

(defvar wi-servers-list-default-hint
  '(("\\[wi-servers-list-tramp]") " open TRAMP session;\n"
    ("\\[wi-servers-list-open-terminal]") " open XTerm with SSH session; "
    ("\\[wi-servers-list-xpanes-open-terminal]") " open XTerm with xpanes and SSH sessions; "
    ("\\[wi-servers-list-xpanes-open-top]") " open XTerm with SSH session and top;\n"
    ("\\[wi-servers-list-xpanes-open-tail-taskexecutor]") " open XTerm and tail taskexecutor logs; "))

(defun wi-servers-list-hint ()
  "Return Bui hints."
  (bui-format-hints
   wi-servers-list-default-hint
   (bui-default-hint)))

(bui-define-interface wi-servers list
  :buffer-name "*Majordomo Servers*"
  :get-entries-function 'wi-servers-get-entries
  :describe-function 'wi-servers-list-describe
  :hint 'wi-servers-list-hint
  :format '((name nil 30 t))
  :sort-key '(name))

;;;###autoload
(defun wi-servers-list-tramp ()
  "Open connection to selected servers via Emacs's TRAMP."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (find-file (concat "/ssh:" host ":"))))
          (bui-list-get-marked-args 'general)))

;;;###autoload
(defun wi-servers-list-tramp-sudo ()
  "Open connection as root user to selected servers via Emacs's TRAMP."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (find-file (format "/ssh:%s|sudo:%s:"
                                 host host))))
          (bui-list-get-marked-args 'general)))

;;;###autoload
(defun wi-servers-list-ping ()
  "Ping selected servers."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (ping host)))
          (bui-list-get-marked-args 'general)))

(defvar wi-servers-list-xterm-command
  '("xterm" "-bg" "white" "-fg" "black" "+sb" "-title" "xpanes"))

(defun wi-servers-list-xpanes-terminal (hosts command)
  "Helper function to open terminal with `xpanes' with selected HOSTS and run a COMMAND."
  (apply #'start-process "xterm" nil
         `(,@wi-servers-list-xterm-command
           ,@(if (> (length hosts) 4)
                 '("-fa" "Monospace" "-fs" "6")
               '())
           "-e" ,(mapconcat 'identity `(,(format "%s" (format "xpanes -c '%s'" command)) ,@hosts) " "))))

;;;###autoload
(defun wi-servers-list-xpanes-open-terminal ()
  "Open `xpanes' interactive SSH session with selected servers."
  (interactive)
  (wi-servers-list-xpanes-terminal (mapcar #'car (bui-list-get-marked-args 'general))
                                             "ssh {}"))

;;;###autoload
(defun wi-servers-list-xpanes-open-top ()
  "Open `xpanes' with `top' for selected servers."
  (interactive)
  (wi-servers-list-xpanes-terminal (mapcar #'car (bui-list-get-marked-args 'general))
                                             "ssh -t {} -- top"))

(defun wi-servers-list-ansible-console (hosts)
  "Helper function to open `ansible-console' with selected HOSTS."
  (apply #'start-process "xterm" nil
         `(,@wi-servers-list-xterm-command
           "-bg" "black" "-fg" "white"
           "-e" ,(mapconcat 'identity `(,(format "%s" (format "ansible-console --limit=%s" (string-join hosts ","))) ) " "))))

;;;###autoload
(defun wi-servers-list-open-ansible-console ()
  "Open `xterm' with `ansible-console' for selected servers."
  (interactive)
  (wi-servers-list-ansible-console (mapcar #'car (bui-list-get-marked-args 'general))))

(let ((map wi-servers-list-mode-map))
  (define-key map (kbd "A") 'wi-servers-list-open-ansible-console)
  (define-key map (kbd "f") 'wi-servers-list-tramp)
  (define-key map (kbd "F") 'wi-servers-list-tramp-sudo)
  (define-key map (kbd "S") 'wi-servers-list-xpanes-open-terminal)
  (define-key map (kbd "t") 'wi-servers-list-xpanes-open-top))

;;;###autoload
(defun wi-installed-servers ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'wi-servers 'list))

(provide 'wi-servers)

;;; wi-servers.el ends here
