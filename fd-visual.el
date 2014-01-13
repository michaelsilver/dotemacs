;; Visual Settings
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                                          ("%b - Dir:  " default-directory)))))))

;; Extra keywords
(setq my:elisp-extra-keywords '("and" "interactive" "or" "cons" "list" "setq-default" "setq" "setf" "set"))

(font-lock-add-keywords 'emacs-lisp-mode (list (cons (regexp-opt my:elisp-extra-keywords 'symbols) font-lock-keyword-face)))

(if window-system
    (let ((comment "IndianRed2"))
      (global-hl-line-mode t)
      (require 'naquadah-theme)
      (load-theme 'naquadah t)
      (custom-theme-set-faces
       'naquadah
       `(default ((t (:family "DeJavu Sans Mono"))))
       `(mode-line ((t (:height 1.1 :background "gray30"))))
       `(minibuffer-prompt ((t (:foreground "orange1"))))
       `(region ((t (:background "gray35"))))
       `(hl-line ((t (:background "gray25"))))
       `(ido-only-match ((t (:foreground "dark green" :bold nil))))

       ;; Development
       `(font-lock-comment-face ((t (:foreground ,comment))))
       `(font-lock-function-name-face ((t (:foreground "orange1" :bold t))))
       `(font-lock-doc-face ((t (:foreground ,comment))))
       `(font-lock-doc-string-face ((t (:foreground ,comment))))
       `(link ((t (:foreground  "#729fcf" :underline t))))

       ;; ERC
       `(erc-prompt-face ((t (:background "#f57900" :bold t :foreground "gray10")))))))

(line-number-mode 1)	; have line numbers and
(column-number-mode 1)	; column numbers in the mode line
(mouse-avoidance-mode 'banish)
(tool-bar-mode -1)	; no tool bar with icons
(menu-bar-mode -1)
(scroll-bar-mode -1)	; no scroll bars
(add-hook 'find-file-hook (lambda () (setq show-trailing-whitespace t)))
(global-linum-mode 1)	; add line numbers on the left
(show-paren-mode t)

(add-hook 'term-mode-hook '(lambda() (set (make-local-variable 'global-hl-line-mode) nil)))

;; full screen
(defun fullscreen ()
  "Set the fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(set-face-attribute 'default nil :height 93)

;; Zoom
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height
		      (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 5)))
  (message (format "Font size: %d" (face-attribute 'default :height))))

(global-set-key (kbd "M-+")      #'(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "M--")      #'(lambda nil (interactive) (djcb-zoom -1)))


(provide 'fd-visual)
