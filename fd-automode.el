;; Extra automodes
(add-to-list 'auto-mode-alist '("[.]zcml" . nxml-mode))
(add-to-list 'auto-mode-alist '("[.]list" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("[.]pt" . html-mode))
(add-to-list 'auto-mode-alist '("[.]sim" . asm-mode))
(add-to-list 'auto-mode-alist '("shrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("[mM]akefile.*$" . makefile-mode))
(add-to-list 'auto-mode-alist '("[.]mk$" . makefile-mode))
(add-to-list 'auto-mode-alist '("[.]zsh.*$" . sh-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

(provide 'fd-automode)
