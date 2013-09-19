;; Terminals
;;
;; TODO: Have an org mode style buffer switcher.

(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/zsh") ;; or use zsh...

(defun fd-term-beginning-of-line ()
  (interactive)
  (term-send-raw-string (kbd "C-a")))

;; only needed if you use autopair
(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)
      (define-key term-raw-map (kbd "M-m") 'fd-term-beginning-of-line)))

(defun ido-term-buffer()
  (interactive)
  (ido-for-mode "Term buffer: " 'term-mode))

(global-set-key (kbd "C-c t") 'ido-term-buffer)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

(add-to-list 'ido-ignore-buffers
	     (format "\*%s<[0-9]*>\*" multi-term-buffer-name))

(defadvice term-send-raw (after update-current-directory)
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd (file-truename (format "/proc/%d/cwd" pid))))
    (cd cwd)))

(ad-activate 'term-send-raw)

(provide 'fd-term)
