(require 'bui)

(defun mj-server->entry (server)
  `((id   . ,server)
    (name . ,server)))

(defun mj-servers-get-entries ()
  (mapcar 'mj-server->entry
          '("web15.intr" "web16.intr" "web17.intr" "web18.intr" "web19.intr" "web20.intr"
            "web21.intr" "web22.intr" "web23.intr" "web25.intr" "web26.intr" "web27.intr" "web28.intr" "web29.intr" "web30.intr"
            "web31.intr" "web32.intr" "web33.intr" "web34.intr" "web35.intr" "web36.intr" "web37.intr"

            "kvm1.intr" "kvm10.intr" "kvm11.intr" "kvm12.intr"
            "kvm13.intr" "kvm14.intr" "kvm15.intr" "kvm16.intr" "kvm17.intr"
            "kvm19.intr" "kvm2.intr" "kvm20.intr" "kvm21.intr" "kvm22.intr"
            "kvm23.intr" "kvm24.intr" "kvm25.intr" "kvm26.intr" "kvm27.intr"
            "kvm28.intr" "kvm29.intr" "kvm30.intr" "kvm31.intr" "kvm32.intr"
            "kvm33.intr" "kvm34.intr" "kvm35.intr" "kvm36.intr" "kvm37.intr"
            "kvm5.intr" "kvm6.intr" "kvm9.intr")))

(bui-define-interface mj-servers list
  :buffer-name "*Majordomo Servers*"
  :get-entries-function 'mj-servers-get-entries
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
