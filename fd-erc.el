
;; ERC
;; check channels
(require 'erc)

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; joining && autojoing

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs")
	(".*\\.freenode.net" "#codebender.cc")
	(".*\\.freenode.net" "#p-space")))

(defun fakedrake-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  ;; (select-frame (make-frame '((name . "Emacs IRC")
  ;; 			      (minibuffer . t))))
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
      (erc :server "irc.freenode.net" :port 6667 :nick my-freenode-nick :full-name my-freenode-fullname :password my-freenode-password)))

(defun my-destroy-erc ()
  "Kill all erc buffers!!"
  (interactive)
  (message "Killing all ERC buffers!")
  (save-excursion
    (dolist (i (buffer-list))
      (with-current-buffer i
	(cond
	 ((eq major-mode 'erc-mode) (kill-buffer (current-buffer))))))))

(defun fd-bury-erc ()
  "Kill all erc buffers!!"
  (interactive)
  (message "Burying ERC buffers...")
  (dolist (i (buffer-list))
    (with-current-buffer i
      (when (eq major-mode 'erc-mode)
	  (bury-buffer)))))

;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e s") 'fakedrake-erc-start-or-switch) ;; ERC
(global-set-key (kbd "C-c e b") 'fd-bury-erc)
(global-set-key (kbd "C-c e k") 'my-destroy-erc)

(add-hook 'erc-mode-hook '(lambda() (set (make-local-variable 'global-hl-line-mode) nil)))

(erc-notifications-mode t)

(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)
(setq erc-image-inline-rescale 5)

(provide 'fd-erc)
