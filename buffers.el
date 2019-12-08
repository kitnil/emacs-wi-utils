(require 'bui)

(defun buffers-buffer->entry (buffer)
  `((id   . ,buffer)
    (name . ,buffer)))

(defun mj-installed-servers-get-entries ()
  (mapcar 'buffers-buffer->entry
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

(bui-define-interface mj-installed-servers list
  :buffer-name "*Majordomo Servers*"
  :get-entries-function 'mj-installed-servers-get-entries
  :format '((name nil 30 t))
  :sort-key '(name))

(defun mj-installed-servers-list-tramp ()
  "Display packages placed in the location at point."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (find-file (concat "/ssh:" host ":"))))
          (bui-list-get-marked-args 'general)))

(defun mj-installed-servers-list-open-terminal ()
  "Display packages placed in the location at point."
  (interactive)
  (mapcar (lambda (entry)
            (let ((host (car entry)))
              (terminal-here (concat "/ssh:" host ":"))))
          (bui-list-get-marked-args 'general)))

(defun mj-installed-servers-list-xpanes-terminal (hosts)
  (start-process "xterm" nil "xterm" "-bg" "white" "-fg" "black" "+sb" "-title" "xpanes" "-e"
                 (mapconcat 'identity `(,(format "%s" "xpanes -c 'ssh {}'") ,@hosts) " ")))

(defun mj-installed-servers-list-xpanes-open-terminal ()
  "Display packages placed in the location at point."
  (interactive)
  (mj-installed-servers-list-xpanes-terminal (mapcar #'car (bui-list-get-marked-args 'general))))

(let ((map mj-installed-servers-list-mode-map))
  (define-key map (kbd "f") 'mj-installed-servers-list-tramp)
  (define-key map (kbd "s") 'mj-installed-servers-list-open-terminal)
  (define-key map (kbd "S") 'mj-installed-servers-list-xpanes-open-terminal))

(defun buffers ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'mj-installed-servers 'list))
