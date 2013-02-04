;; Python

;; Fix docstring paragraph filling
(add-hook 'python-mode-hook (lambda ()
			      (setq paragraph-start (concat paragraph-start "\\|\\s-*\"\"\".*$"))))

(defun py-my-indent-region (&optional min max)
  "Stupidly clamp indentation to the closest multiple of 4 spaces."
  (interactive)
  (save-excursion
    (let ((top (or min (point-min)))
	  (bottom (or max (point-max)))
	  (line-move-visual nil))
      (goto-char top)
      (while (<= (point) bottom)
	(indent-line-to
	 (* 4 (round (/ (float (current-indentation)) 4))))
	(next-line) (end-of-line)))))


(setq py-shell-name "ipython")
(setq py-mode-map python-mode-map)
;; (setq ipython-command "/usr/bin/ipython2")
;; (setq ipython-completion-command-string "print ';'.join(__IP.Completer.all_completions('%s'))\n")
;; (require 'ipython2)

(setq pylookup-dir "~/.emacs.d/el-get/pylookup/")
(require 'pylookup)
;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(require 'python-mode)
(define-key python-mode-map "\C-cp" '(lambda () (interactive) (insert "import ipdb; ipdb.set_trace()")))
(define-key python-mode-map "\C-ch" 'pylookup-lookup)
(define-key python-mode-map "\C-x\\" 'py-my-indent-region)

(setq py-split-windows-on-execute-p nil)

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p major-mode 'python-mode)
    ;; set COMINT argument to `t'.
    (ad-set-arg 1 t)))

(provide 'fd-python)
