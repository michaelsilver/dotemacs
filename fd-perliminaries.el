;;; fd-perliminaries.el

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

(provide 'fd-perliminaries)
