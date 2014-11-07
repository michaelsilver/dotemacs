;; GGtags

(require 'ggtags)
(defun fd-tagging-hook ()
  (setq-local eldoc-documentation-function 'ggtags-el)
  (setq-local imenu-create-index-function 'ggtags-build-imenu-index)
  (ggtags-mode 1))

(dolist (m '(c-mode-common-hook
	     python-mode
	     js2-mode))
  (add-hook m 'fd-tagging-hook))

(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "M-<") 'beginning-of-buffer)

(provide 'fd-tags)
