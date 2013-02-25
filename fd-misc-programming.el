;; Programming realted miscelaneous
(which-function-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline 'query)
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1))) ;; fix tabcompletion

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Extra automodes
(add-to-list 'auto-mode-alist '("[.]zcml" . nxml-mode))
(add-to-list 'auto-mode-alist '("[.]pt" . html-mode))
(add-to-list 'auto-mode-alist '("[.]sim" . asm-mode))

;; Indent buffer
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))
(global-set-key "\C-x\\" 'indent-buffer)

;; PATH in emacs
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (car (reverse (split-string (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(if window-system (set-exec-path-from-shell-PATH))

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'fd-misc-programming)
