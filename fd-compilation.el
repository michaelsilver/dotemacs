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
(global-set-key (kbd "C-c c c") 'cs-compile-wrapper)
(global-set-key (kbd "M-g l") 'cs-last-error)
(global-set-key (kbd "M-g t") 'error-last-of-trace)

(defcustom error-trace-max-distance 2
  "Maximum distance between errors of a trace.")

(defun next-error-point (&optional pt reverse)
  "The point of the next error from point. Nil if no error after
this."
  (save-excursion
    (condition-case e
	(progn
	  (compilation-next-error (if reverse 1 -1) nil (or pt (point)))
	  (point))
      ('error nil))))

(defun error-end-of-trace (&optional reverse)
  "Jump to the last error of the next trace. If reverse jump to
the top."
  (interactive)
  (pop-to-buffer (next-error-find-buffer))
  ;; Enter the next trace. Should raise error if we are at the end
  (compilation-next-error 1 nil (or compilation-current-error (point-min)))

  ;; Move to it's end
  (let ((le (internal-last-of-trace (point) reverse)))
    (goto-char le)
    (recenter)
    (compile-goto-error)))

(defun internal-end-of-trace (pt reverse)
  "Find the last error o a trace that we are in."
  (let ((nep (next-error-point pt reverse)))
    ;; There is an error and it is close.
    (if (and nep (<= (count-lines pt nep)
		     error-trace-max-distance))
	(internal-last-of-trace nep reverse)
      pt)))

(provide 'fd-compilation)
