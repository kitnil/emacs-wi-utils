(require 'bui)
(require 's)
(require 'cl)

(defun mj-server->entry (server)
  `((id   . ,server)
    (name . ,server)))

(defun mj-hosts--candidates ()
  (seq-filter (lambda (str)
                (string-suffix-p ".intr" str))
              (mapcar (lambda (str)
                        (car (split-string str " ")))
                      (split-string (with-temp-buffer
                                      (insert-file-contents (expand-file-name "~/.ssh/known_hosts"))
                                      (buffer-string))
                                    "\n"))))

(defun mj-servers-get-entries ()
  (mapcar 'mj-server->entry (mj-hosts--candidates)))

(defvar mj-servers-list-default-hint
  '(("\\[mj-servers-list-tramp]") " open TRAMP session;\n"
    ("\\[mj-servers-list-open-terminal]") " open XTerm with SSH session; "
    ("\\[mj-servers-list-xpanes-open-terminal]") " open XTerm with xpanes and SSH sessions; "
    ("\\[mj-servers-list-xpanes-open-top]") " open XTerm with SSH session and top;\n"
    ("\\[mj-servers-list-xpanes-open-tail-taskexecutor]") " open XTerm and tail taskexecutor logs; "))

(defun mj-servers-list-hint ()
  (bui-format-hints
   mj-servers-list-default-hint
   (bui-default-hint)))

(bui-define-interface mj-servers list
  :buffer-name "*Majordomo Servers*"
  :get-entries-function 'mj-servers-get-entries
  :describe-function 'mj-servers-list-describe
  :hint 'mj-servers-list-hint
  :format '((name nil 30 t))
  :sort-key '(name))

(defun mj-servers-list-tramp ()
  "Display packages placed in the location at point."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (find-file (concat "/ssh:" host ":"))))
          (bui-list-get-marked-args 'general)))

(defun mj-servers-list-ping ()
  "Display packages placed in the location at point."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (ping host)))
          (bui-list-get-marked-args 'general)))

(defun mj-servers-list-open-terminal ()
  "Display packages placed in the location at point."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (terminal-here (concat "/ssh:" host ":"))))
          (bui-list-get-marked-args 'general)))

(defvar mj-servers-list-xterm-command
  '("xterm" "-bg" "white" "-fg" "black" "+sb" "-title" "xpanes" "-e"))

(defun mj-servers-list-xpanes-terminal (hosts command)
  (apply #'start-process "xterm" nil
         `(,@mj-servers-list-xterm-command
           ,(mapconcat 'identity `(,(format "%s" (format "xpanes -c '%s'" command)) ,@hosts) " "))))

(defun mj-servers-list-xpanes-open-terminal ()
  "Display packages placed in the location at point."
  (interactive)
  (mj-servers-list-xpanes-terminal (mapcar #'car (bui-list-get-marked-args 'general))
                                             "ssh {}"))

(defun mj-servers-list-xpanes-open-top ()
  "Display packages placed in the location at point."
  (interactive)
  (mj-servers-list-xpanes-terminal (mapcar #'car (bui-list-get-marked-args 'general))
                                             "ssh -t {} -- top"))

(defun mj-servers-list-xpanes-open-tail-taskexecutor ()
  "Display packages placed in the location at point."
  (interactive)
  (mj-servers-list-xpanes-terminal (mapcar #'car (bui-list-get-marked-args 'general))
                                             "ssh -t {} -- sudo tail -f /var/log/taskexecutor.log"))

(let ((map mj-servers-list-mode-map))
  (define-key map (kbd "f") 'mj-servers-list-tramp)
  (define-key map (kbd "s") 'mj-servers-list-open-terminal)
  (define-key map (kbd "S") 'mj-servers-list-xpanes-open-terminal)
  (define-key map (kbd "t") 'mj-servers-list-xpanes-open-top)
  (define-key map (kbd "T") 'mj-servers-list-xpanes-open-tail-taskexecutor))

(defun mj-installed-servers ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'mj-servers 'list))
