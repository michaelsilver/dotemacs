;; ORG mode
(require 'org)
(load-library "org-compat") 		;XXX this is bad but i see no better way.

;; Set up org-mode capture system
(if (and (file-exists-p my-orgmode-dir)
	 (eq t (car (file-attributes my-orgmode-dir)))) ; It is actually a directory
    (setq org-default-notes-file (concat my-orgmode-dir my-notes-file))
  (message (format "Ormode directory is not valid: %s" my-orgmode-dir)))


(defvar org-journal-file "~/.orgmode/journal.org"
  "Path to OrgMode journal file.")
(defvar org-journal-date-format "%Y-%m-%d"
  "Date format string for journal headings.")

(defun org-heading-search (string &optional begin-point)
  "Look for a heading with head in it's name."
  (let ((p (or begin-point (point))))
    (if (search-forward string nil t)
	;; If you are at head stop or keep looking
	(if (org-at-heading-p)
	    t
	  (org-heading-search string p))
      (goto-char p)
      nil)))

(defun org-heading-search-path (hlist)
  "Return the portion of the list that was not found and place
point at the last found one."
  (if hlist
      (save-restriction
	;; If the head is found, decapitate
	(if (org-heading-search (car hlist))
	    (progn
	      (org-narrow-to-subtree)
	      (org-heading-search-path (cdr hlist)))
	  hlist))
    nil))

(defun org-end-of-element ()
  "Jump to the end of the text under the current or previous
  heading. If the point is before the first element it will be
  set exactly before it."
  (end-of-line)
  (if (search-forward-regexp "^\*" nil t)
      (previous-line)
    (end-of-buffer))
  (search-backward-regexp "[^[:space:]]" nil t)
  (end-of-line))

(defun org-insert-first-child (name)
  "Insert a heading as the first child of the current
heading. Non nil no-root means do not create a root node."
  (org-end-of-element)

  (insert "\n")
  (let  ((demote (/= (org-outline-level) 0)))
    (org-insert-heading)
    (when demote
      (org-demote)))
  (insert name))

(defun org-heading-create-path (hlist)
  "Create a path of headings under the current. Non-nil no-root
means the first element is definitely not a root node."
  (when hlist
    (org-insert-first-child (car hlist))
    (org-heading-create-path (cdr hlist))))

(defun org-search-or-insert-path (hlist)
  "Search for path or insert it."
  (show-all)
  (org-heading-create-path
   (org-heading-search-path hlist)))

(defun fd-org-journal-space (newlines-sep)
  "Makse sure there is correct spacing from the current line."
  (let ((nls (1+ (or newlines-sep 0))))
    (if (looking-at (format "\n\\{%d\\} *\n" nls))
	(progn (forward-char nls) (end-of-line))
      (dotimes (i nls) (insert "\n"))))
  (org-indent-line))

(defun org-journal-entry (heading &optional newlines-sep)
  "Create a new diary entry for today or append to an existing one."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)

  ;; Insert a heading for today if there is none
  (let ((today (format-time-string org-journal-date-format)))
    (beginning-of-buffer)
    (org-search-or-insert-path (list heading today))

    (org-end-of-element)
    (fd-org-journal-space newlines-sep)

    (when (not (looking-at "\n[:space:]*\n"))
      (insert "\n")
      (backward-char))))

;; Org mode key bindings
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cj" (lambda () (interactive) (org-journal-entry "Journal" 1)))
(global-set-key (kbd "C-c w") (lambda () (interactive) (org-journal-entry "Workout")))

;; Bindings
(add-hook 'org-mode-hook
	  '(lambda ()
	     (define-key org-mode-map "\M-j" 'org-meta-return)
	     (define-key org-mode-map "\M-n" 'org-forward-element)
	     (define-key org-mode-map "\M-p" 'org-backward-element)))

;; FlySpell
;; For this you would need word lists. For arch install:
;; pacman -S aspell-en
;;
;; Usage:
;; F8 - Run ispell to the whole document
;; M-$ - Current word
(add-hook 'org-mode-hook
	  #'(lambda ()
	      (define-key org-mode-map [(tab)] nil)
	      (flyspell-mode t)))

;; AutoPairs
;; (add-hook 'org-mode-hook
;;           #'(lambda () (add-to-list 'autopair-extra-pairs '(:everywhere ("=" . "=")))))

;; Aspell
(setq ispell-program-name "aspell")


(require 'org-latex)
(setq org-latex-default-figure-position "H")

;; XXX: ensure the fonts are all there.
;; For arch that would be:
;; pacman -S ttf-dejavu ttf-freefont.
;;
;; Usage: on top of the .org doc put these.
;; #+LaTeX_CLASS: fakedrake-org-article
;; #+LaTeX_HEADER: <some extra headings>
(add-to-list 'org-export-latex-classes
	     '("fakedrake-org-article"
	       "\\documentclass[11pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{float}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setmainfont{DejaVu Sans}
\\setmonofont[Scale=0.8]{FreeMono}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage[bookmarks, colorlinks, breaklinks]{hyperref}
\\hypersetup{linkcolor=black, citecolor=blue,filecolor=black,urlcolor=MidnightBlue}
\\pagestyle{empty}
\\usepackage{amsmath}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; XXX: Ensure pdflatex is available
;; for arch that would be:
;; pacman -S texlive-most texlive-lang
;; XXX: Ensure xetexlatex is available
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
	"xelatex -interaction nonstopmode %f")) ;; for multiple passes

(require 'fd-octopress)
(octopress-setup)

(provide 'fd-org)
