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

(defvar projects-alist '(("wikipedia-base" . "/home/fakedrake/Projects/CSAIL/Python/WikipediaBase/PLAN.org")
			 ("overlay-parse" . "/home/fakedrake/Projects/CSAIL/Python/py/src/overlay-parse/overlay_parse/dates.py")
			 ("dotemacs" . "~/.emacs.d/fd-misc-programming.el")
			 ("csail-plan" . "~/Projects/CSAIL/PLAN.org")
			 ("wikipedia-mirror-futuna" . "/ssh:cperivol@futuna.csail.mit.edu:/scratch/cperivol/wikipedia-mirror/")
			 ("wikipedia-mirror-ashmore" . "/ssh:cperivol@ashmore.csail.mit.edu:/local/cperivol/wikipedia-mirror/")
			 ("wikipedia-mirror-tuvalu" . "/ssh:cperivol@tuvalu.csail.mit.edu:/local/cperivol/wikipedia-mirror/")
			 ("xilinx-zynq-bootstrap-purple" . "/ssh:cperivol@purple:/homes/cperivol/Projects/xilinx-zynq-bootstrap")
			 ("wikipedia-base-futuna" . "/ssh:cperivol@futuna.csail.mit.edu:/scratch/cperivol/wikipediabase/"))
  "An alist of projects and the most relatd file.")

(defun read-project-name ()
  "An abstraction to implement ways of deciding which should be
the default project"
  (ido-completing-read "Jump to project: " projects-alist))

(defun jump-to-project (project-name)
  "Jump to a characteristic file in a project found in
PROJECTS-ALIST."
  (interactive (list (read-project-name)))
  (find-file  (cdr (assoc project-name projects-alist))))

(global-set-key (kbd "C-x j p") 'jump-to-project)

(setq git--commit-args "--cleanup=whitespace")

(provide 'fd-misc-programming)
