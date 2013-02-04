;; Miscelaneous settings
;;
;; Do not include anything that requires anything that isnt packaged
;; with emacs here.

;; Ensure that personal.el exists
(cond ((not (file-readable-p "~/.emacs.d/personal.el"))
       (progn
	 (copy-file "~/.emacs.d/dummy-personal.el" "~/.emacs.d/personal.el")
	 (message "Copied dummy personal preferences to personal.el"))))

(load-file   "~/.emacs.d/personal.el")

;; Server configuration
(require 'server)
(if (server-running-p)
    (message "Skipping server creation, one already exists")
  (server-start))

;; Configurations
(delete-selection-mode t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(set-input-method 'greek)
(toggle-input-method)
(setq scroll-step 1)
(global-set-key "\C-Z" 'revert-buffer)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'fd-misc)
