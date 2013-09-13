;; ORG mode
(require 'org)

;; Set up org-mode capture system
(if (and (file-exists-p my-orgmode-dir)
	 (eq t (car (file-attributes my-orgmode-dir)))) ; It is actually a directory
    (setq org-default-notes-file (concat my-orgmode-dir my-notes-file))
  (message (format "Ormode directory is not valid: %s" my-orgmode-dir)))

(add-to-list 'org-modules 'org-habit)
(setq org-log-repeat "time")

(setq org-agenda-custom-commands
      '(("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))))

(setq org-directory my-orgmode-dir)
(setq org-mobile-directory my-orgmode-dir)
(setq org-agenda-files (mapcar (lambda (x) (concat org-directory x)) my-agenda-files))

;; Org mode key bindings
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

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
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode %f"
	"xelatex -interaction nonstopmode %f")) ;; for multiple passes


(provide 'fd-org)
