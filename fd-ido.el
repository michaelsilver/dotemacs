
;; Ido mode
(require 'ido)
(require 'ido-speed-hack)
(require 'ido-better-flex)
(require 'ido-ubiquitous)
(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-save-directory-list-file (my-expand-path ".ido.last"))
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
;; (ido-everywhere t)
;; This is mainly for just swapped letters. It sometimes doesnt catch
;; entire words
(ido-better-flex/enable)
;; (setq ido-file-extensions-order '(".c" ".cpp" ".h" ".py" ".txt" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
(setq org-completion-use-ido t)

;; TODO: if you actually are at the buffer at point, rotate.
(defun ido-for-mode(prompt the-mode)
  "Switch to buffer of the-mode using prompt"
  (switch-to-buffer
   (ido-completing-read prompt (fd-mode-buffers the-mode))))

(defun fd-mode-buffers (the-mode)
  "List of buffers of mode THE-MODE."
  (save-excursion
    (delq
     nil
     (mapcar (lambda (buf)
	       (when (and (buffer-live-p buf) (not (eq (current-buffer) buf)))
		 (with-current-buffer buf
		   (and (eq major-mode the-mode)
			(buffer-name buf)))))
	     (buffer-list)))))

(provide 'fd-ido)
