;; Miscelaneous settings
;;
;; Do not include anything that requires anything that isnt packaged
;; with emacs here.

;; Ensure that personal.el exists
(cond ((not (file-readable-p (my-expand-path "personal.el")))
       (progn
	 (copy-file (my-expand-path "dummy-personal.el") (my-expand-path "personal.el"))
	 (message "Copied dummy personal preferences to personal.el"))))

(load-file (my-expand-path "personal.el"))

;; Server configuration
(require 'server)
(if (server-running-p)
    (message "Skipping server creation, one already exists")
  (server-start))

;; Configurations
(delete-selection-mode t)
(setq backup-directory-alist '(("." . (my-expand-path "backup/"))))
(set-input-method 'greek)
(toggle-input-method)
(setq scroll-step 1)
(global-set-key "\C-Z" 'revert-buffer)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(provide 'fd-misc)
