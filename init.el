(require 'cl)
(add-to-list 'load-path "~/.emacs.d/")

(require 'fd-el-get)
(require 'fd-misc)
(require 'fd-misc-programming)
(require 'fd-visual)
(require 'fd-clipboard)
(require 'fd-ido)
(require 'fd-yasnippet)
(require 'fd-autocomplete)
(require 'fd-python)
(require 'fd-undotree)
(require 'fd-recentfiles)
(require 'fd-erc)
(require 'fd-bookmarks)
(require 'fd-tags)
(require 'fd-expand-region)
(require 'fd-ffip)
(require 'fd-lisp)
(require 'fd-desktop)
(require 'fd-cc-mode)
(require 'fd-prolog)
(require 'fd-vimperator)
(require 'fd-org)
(require 'fd-midnight)

;; Customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((gtags-rootdir . "~/Projects/linux-3.7-rc8/") (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t)))))
