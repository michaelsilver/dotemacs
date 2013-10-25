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

(defun verbose-recompile ()
  (message "Compiling with: cmd: '%s', dir: '%s'"
	     compile-command compilation-directory)
  (call-interactively 'recompile))

(defun fd-recompile-wrapper ()
  (interactive)
  (let* ((lu-name (or (buffer-file-name) (buffer-name)))
	 (item (when lu-name (cdr (assoc lu-name compilation-setups)))))
    (if item
	(let ((compile-command (car item))
	      (compilation-directory (cdr item)))
	  (verbose-recompile))
      (verbose-recompile))))

(defun fd-compilation-setup-save ()
  (interactive)
  (let ((key (or (buffer-file-name) (buffer-name)))
	(val (cons compile-command compilation-directory)))
    (if (assoc key compilation-setups)
	(setf (cdr (assoc key compilation-setups)) val)
      (add-to-list 'compilation-setups (cons key val)))
    (message "Pinned compilation: cmd: '%s', dir: '%s' to '%s'"
	     compile-command compilation-directory key)))

(global-set-key (kbd "C-c r") 'fd-recompile-wrapper)
(global-set-key (kbd "C-c c s") 'fd-compilation-setup-save)

(provide 'fd-compilation)
