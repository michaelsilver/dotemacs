
;; CLIPBOARD
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value
      x-select-enable-primary t)

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (with-temp-buffer
	(insert filename)
	(clipboard-kill-region (point-min) (point-max)))
      (message (format "Copied '%s'" filename)))))

(global-set-key (kbd "C-x y") 'my-put-file-name-on-clipboard)


(provide 'fd-clipboard)
