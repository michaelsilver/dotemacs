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

(defvar compilation-setups nil
  "alist with compilation setups.")


(defun fd-recompile-wrapper ()
  (interactive)
  (let* ((lu-name (or (buffer-file-name) (buffer-name)))
	(item (when lu-name (assoc lu-name compilation-setups))))
    (if item
	(let ((compile-command (caar item))
	      (compilation-directory (cdar item)))
	  (call-interactively 'recompile))
      (call-interactively 'recompile))))

(defun fd-compilation-setup-save ()
  (interactive)
  (add-to-list 'compilation-setups
	       (cons (or (buffer-file-name) (buffer-name))
		     (cons compile-command compilation-directory))))

(global-set-key (kbd "C-c r") 'fd-recompile-wrapper)
(global-set-key (kbd "C-c c s") 'fd-compilation-setup-save)

(provide 'fd-compilation)
