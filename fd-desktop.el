
;; Desktop
(require 'desktop)
(desktop-save-mode t)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq make-backup-files nil)

(global-set-key "\C-ckb" (lambda nil (interactive) (when (y-or-n-p "Really kill all buffers?") (desktop-clear))))



(provide 'fd-desktop)
