;; GGtags

(require 'ggtags)
(defun fd-tagging-hook ()
  (setq-local eldoc-documentation-function 'ggtags-eldoc-function)
  (setq-local imenu-create-index-function 'ggtags-build-imenu-index)
  (ggtags-mode 1)
  (setq ggtags-mode-map-alist nil))

(dolist (m '(c-mode-common-hook
	     python-mode
	     js2-mode))
  (add-hook m 'fd-tagging-hook))

(provide 'fd-tags)
