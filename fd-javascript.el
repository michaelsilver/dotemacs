(require 'js2-mode)
(require 'slime-js)
(setq slime-js-swank-command
      (concat (replace-in-string
	       (shell-command-to-string "/usr/sbin/npm bin -g 2> /dev/null")
	       "\n" "")
	      "/swank-js")
      slime-js-swank-args nil)

(defvar fd-js-etags-gen-command (replace-in-string
				 "find . \! -name '.*' -regex '.*\.jsm?' |
xargs etags --language=none
--regex='/.*?\\([a-zA-Z0-9_]*\\)[ \\n\\t]*=[ \\t]*function[ \\t]*\\([a-zA-Z0-9_]*\\)?[ \\n\\t]*(/\\1/'
--regex='/[ \\t]*\\([a-zA-Z0-9_]*\\)[ \\n\\t]*:/\\1/'
--regex='/[ \\t,;]*function[ \\t\\n]*\\([a-zA-Z0-9_]*\\)[ \\t\\n]*(/\\1/'
--regex='/^var[ \\t\\n]*\\([a-zA-Z0-9_]*\\)/\\1/'
" "\n" " ")
  "Find assigned functions, defined functions, object/dictionary
  properties that are preceeded by spaces and top level
  variables")

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


(defun js-jack-in-browser ()
  (interactive)
  (unless (get-process "swank-js")
    (slime-js-run-swank))

  (slime-connect "localhost" 4005 nil t)
  (slime-repl))

(defun fd-js-mode-hook ()
  "Hooks for all clojure."
  (slime-js-minor-mode 1)
  (setq tab-width 2
	js2-basic-offset 2)
  (setq-local etags-table-create-table-command-string
	      fd-js-etags-gen-command)
  (define-key js2-mode-map (kbd "C-j") 'my-js-newline-and-indent))

(defvar js-compilation-error-regex-alist
  ;; REGEX FILE-GROUP LINE-GROUP COLUMN-GROUP ERROR-TYPE LINK-GROUP
  '((nodejs
     "^[ \\t]*at .*?file://\\(\\(.*?\\):\\([0-9]*\\)\\(:\\([0-9]*\\)\\)?\\))?$"
     2 3 5 nil 1)
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

(provide 'fd-javascript)
