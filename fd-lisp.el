;; Lisp
;; Elisp
(define-key emacs-lisp-mode-map "\C-c\C-e" 'eval-buffer)

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

(defun fd-elisp-hooks ()
  (define-key emacs-lisp-mode-map (kbd "C-M-d") 'edebug-defun))

(add-hook 'emacs-lisp-mode-hook 'fd-elisp-hooks)

;; Slime
(require 'clojure-mode)
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))


(provide 'fd-lisp)
