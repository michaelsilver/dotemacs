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

(if fd-secretary-enabled
    (require 'fd-mail)
  (require 'fd-desktop))

;; Customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Sandbox/TODO.org" "~/Ubuntu One/OrgMode/health.org" "/home/fakedrake/.orgmode/agenda/notes.org")))
 '(safe-local-variable-values (quote ((fd-setup-test-cmd . "test") (fd-setup-test-cmd . "nosetests") (cs-key . wikipediabase) (cs-key . install) (cscope-id . "linux") (eval progn (c-set-offset (quote innamespace) (quote 0)) (c-set-offset (quote inline-open) (quote 0))) (gtags-path-style quote root) (gtags-rootdir . "/homes/cperivol/Projects/Nema/ZYNQ/linux-zynq/") (gtags-rootdir . "~/Projects/linux-3.7-rc8/") (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(message "Welcome to emacs!")
