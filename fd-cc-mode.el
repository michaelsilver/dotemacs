;; CC-MODE
;; Requires: c-eldoc

(defun my-cc-newline-and-indent ()
  "Append a newline first if the cursor is between { and }."
  (interactive)
  (when (and (not (nth 8 (syntax-ppss)))
	     (looking-back "{\s*")
	     (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (newline-and-indent))

(defun fakedrake-cc-mode-init ()
  "Just some initializations I need for C"
  (define-key c-mode-base-map (kbd "C-M-n") 'c-end-of-defun)
  (define-key c-mode-base-map (kbd "C-M-p") 'c-beginning-of-defun)
  (define-key c-mode-base-map (kbd "M-n") 'c-end-of-statement)
  (define-key c-mode-base-map (kbd "M-p") 'c-beginning-of-statement)
  (define-key c-mode-base-map (kbd "C-j") 'my-cc-newline-and-indent)
  (define-key c-mode-base-map (kbd "C-x <SPC>") 'gud-break)
  (setq c-default-style "linux" c-basic-offset 4))

(setq compilation-scroll-output t)
(add-hook 'c-mode-common-hook 'fakedrake-cc-mode-init)

(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-to-list 'ido-ignore-buffers ".*-preprocessed\*")

(provide 'fd-cc-mode)
