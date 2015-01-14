(require 'js2-mode)

(defun my-js-newline-and-indent ()
  "Append a newline first if the cursor is between { and }."
  (interactive)
  (when (and (not (nth 8 (syntax-ppss)))
	     (looking-back "{\s*")
	     (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (newline-and-indent))

(defun fd-js-mode-hook ()
  "Hooks for all clojure."
  (setq js2-basic-offset 2)
  (define-key js2-mode-map (kbd "C-j") 'my-js-newline-and-indent)
  (setq-local untabify-on-save t))

(defvar js-compilation-error-regex-alist
  ;; REGEX FILE-GROUP LINE-GROUP COLUMN-GROUP ERROR-TYPE LINK-GROUP
  '(
    ;; When selenium runs pipe it through `sed
    ;; 's_http://localhost:8080_$(CURDIR)_g'`
    (my-selenium
     "^BrowserLog: \\(\\(.*\.js\\) \\([0-9]*\\):\\([0-9]*\\)\\) "
     2 3 4 '(1) 1)

    (nodejs
     "^[ \\t]*at.* (?\\(file://\\)?\\(\\(.*?\\):\\([0-9]*\\)\\(:\\([0-9]*\\)\\)?\\))?$"
     3 4 6 nil 2)
    (jasmine
     "^[ \\t]*\\(file://\\(.*\\):\\([0-9]*\\)\\)$"
     2 3 nil nil  1)))

(setq compilation-error-regexp-alist
      (append (mapcar 'car js-compilation-error-regex-alist)
	      compilation-error-regexp-alist))

(setq compilation-error-regexp-alist-alist
      (append js-compilation-error-regex-alist
	      compilation-error-regexp-alist-alist))

(add-hook 'js2-mode-hook 'fd-js-mode-hook)

(defvar html-beatify-cmd "js-beautify --type html -s 2 -a -p -f -")
(defun html-beautify-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end)
			   html-beatify-cmd nil t))

(defvar js2-beatify-cmd "js-beautify -s 2 -a -p -f -")
(defun js2-beautify-region ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end)
			   js2-beatify-cmd nil t))

(provide 'fd-javascript)
