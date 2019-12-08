(require 'bui)

(defun mj-server->entry (server)
  `((id   . ,server)
    (name . ,server)))

;; Code from counsel-tramp.
(defun mj-hosts--candidates (&optional file)
  "Collect candidates for counsel-tramp from FILE."
  (let ((source (split-string
                 (with-temp-buffer
                   (insert-file-contents (or file "~/.ssh/config"))
                   (buffer-string))
                 "\n"))
        (hosts (if file '() counsel-tramp-custom-connections)))
    (dolist (host source)
      (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
	(setq host (match-string 1 host))
	(if (string-match "[ \t\n\r]+\\'" host)
	    (replace-match "" t t host))
	(if (string-match "\\`[ \t\n\r]+" host)
	    (replace-match "" t t host))
        (unless (string= host "*")
	  (if (string-match "[ ]+" host)
	      (let ((result (split-string host " ")))
		(while result
		  (push
		   (concat "/" tramp-default-method ":" (car result) ":")
		   hosts)
		  (push
		   (concat "/ssh:" (car result) "|sudo:root@" (car result) ":/")
		   hosts)
		  (pop result)))
	    (push
	     (concat host)
	     hosts))))
      (when (string-match "Include +\\(.+\\)$" host)
        (setq include-file (match-string 1 host))
        (when (not (file-name-absolute-p include-file))
          (setq include-file (concat (file-name-as-directory "~/.ssh") include-file)))
        (when (file-exists-p include-file)
          (setq hosts (append hosts (counsel-tramp--candidates include-file))))))
    (reverse hosts)))

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
