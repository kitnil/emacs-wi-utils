(require 'bui)

(defun buffers-buffer->entry (buffer)
  (with-current-buffer buffer
    `((id   . ,buffer)
      (name . ,(buffer-name))
      (mode . ,major-mode)
      (size . ,(buffer-size))
      (file-name . ,buffer-file-name))))

(defun buffers-get-entries ()
  (mapcar 'buffers-buffer->entry (buffer-list)))

(bui-define-interface buffers list
  :buffer-name "*Buffers*"
  :get-entries-function 'buffers-get-entries
  :format '((name nil 30 t)
            (mode nil 25 t)
            (size nil 8 bui-list-sort-numerically-2 :right-align t)
            (file-name bui-list-get-file-name 30 t))
  :sort-key '(name))

(defun buffers ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'buffers 'list))
