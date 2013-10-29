;;; fd-compilation.el
;; Stuff related to compilation.

(require 'notifications)
(defun compilation-end-defun (compilation-buffer result)
  (with-current-buffer compilation-buffer
    (if (string= (buffer-name) "*compilation*")
	(notifications-notify
	 :title (format "Compilation: %s" result)
	 :body (format "Cmd: %s" compile-command))
      (notifications-notify
       :title (format "Finished: %s" (buffer-name))))))

(setq compilation-finish-function 'compilation-end-defun)

(require 'compilation-setup)
(global-set-key (kbd "C-c r") 'cs-recompile-wrapper)
(global-set-key (kbd "C-c c s") 'cs-compilation-setup-save)

(provide 'fd-compilation)
