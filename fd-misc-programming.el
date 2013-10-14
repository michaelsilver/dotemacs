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

;; Extra automodes
(add-to-list 'auto-mode-alist '("[.]zcml" . nxml-mode))
(add-to-list 'auto-mode-alist '("[.]pt" . html-mode))
(add-to-list 'auto-mode-alist '("[.]sim" . asm-mode))
(add-to-list 'auto-mode-alist '("shrc$" . sh-mode))

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

(global-set-key (kbd "C-c r") 'recompile)
(define-key git-global-map "p" (lambda () (interactive) (git-cmd "push")))


(require 'notifications)
(defun compilation-end-defun (compilation-buffer result)
  (with-current-buffer compilation-buffer
    (notifications-notify
     :title (format "%s(%d): %s"
		    (if (= result 0) "Success" "Fail")
		    result
		    compile-command))))

(setq compilation-finish-function 'compilation-end-defun)

(provide 'fd-misc-programming)
