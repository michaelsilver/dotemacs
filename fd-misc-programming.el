;; Programming realted miscelaneous
(which-function-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline 'query)
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1))) ;; fix tabcompletion

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers


(add-hook 'term-mode-hook
	  #'(lambda ()
	      (setq autopair-dont-activate t) ;; for emacsen < 24
	      (autopair-mode -1))             ;; for emacsen >= 24
	  )

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

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally)

(defalias '>> 'rsh)
(defalias '<< 'lsh)

(defun github-clone (repo)
  (interactive "MProvide the repository line <username>/<repo>: ")
  (async-shell-command (format "git clone git@github.com:%s" repo)))

(global-set-key (kbd "C-j") 'default-indent-new-line)
(global-set-key (kbd "C-M-l") 'add-dir-local-variable)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)


(provide 'fd-misc-programming)
