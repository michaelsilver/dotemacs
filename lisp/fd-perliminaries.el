;;; fd-perliminaries.el

;; Require this in every module to make sure it can run independently.

(setq dotemacs-dir "~/.emacs.d/")
(defun my-expand-path (f) (concat dotemacs-dir f))
(add-to-list 'load-path (my-expand-path "lisp"))
(add-to-list 'load-path (my-expand-path "el-get/el-get"))
;; (eval-when-compile
;;   (load-file (my-expand-path "fd-el-get.el")))

(defvar fd-secretary-enabled nil
  "Secretary mode loads configurations for gnus and automatically
  opens erc and other stuff. You may also have a slightly
  different face so you can tell them apart. Enable this with the
  `-secreatry' cli option.")

(add-to-list 'command-switch-alist
	     '("secretary" . fd-enable-secreatry))

(defun fd-enable-secreatry (switch)
  "Enable the secretary stuff"
  (setq fd-secretary-enabled t)
  (message "Secretary mode!!"))



;; PATH in emacs
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (car (reverse (split-string (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
