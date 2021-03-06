(require 'cl)

(load-file "~/.emacs.d/lisp/fd-preliminaries.el")
(require 'ms-personal)
(when (eq system-type 'darwin) ;; mac specific settings
  (require 'ms-macosx))
(require 'fd-el-get)
(require 'fd-misc)
(require 'fd-misc-programming)
(require 'ms-tramp)                     ; if not Michael Silver, delete this line
(require 'fd-automode)
(require 'fd-javascript)
(require 'fd-visual)
(require 'fd-clipboard)
(require 'fd-ido)
(require 'fd-yasnippet)
(require 'fd-autocomplete)
(require 'fd-python)
;; (require 'fd-desktop)
(require 'fd-bookmarks)
(require 'fd-tags)
(require 'fd-lisp)
(require 'fd-cc-mode)
(require 'fd-prolog)
(require 'fd-org)
(require 'fd-dired)
(require 'fd-term)
(require 'fd-compilation)
(require 'fd-imenu)
(require 'fd-sql)
(require 'fd-agenda)
(require 'fd-notify)
(require 'fd-projects)
(require 'fd-jstest)

(setq enable-local-variables :all)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(message "Welcome to emacs!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
