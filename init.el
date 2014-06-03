(require 'cl)

(load-file "~/.emacs.d/fd-essential.el")

(require 'fd-perliminaries)
(require 'fd-el-get)
(require 'fd-misc)
(require 'fd-misc-programming)
(require 'fd-automode)
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
(require 'fd-lisp)
(require 'fd-cc-mode)
(require 'fd-prolog)
(require 'fd-vimperator)
(require 'fd-org)
(require 'fd-midnight)
(require 'fd-dired)
(require 'fd-term)
(require 'fd-compilation)
(require 'fd-imenu)

(if fd-secretary-enabled
    (require 'fd-mail)
  (require 'fd-desktop))

(setq enable-local-variables :all)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(message "Welcome to emacs!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
