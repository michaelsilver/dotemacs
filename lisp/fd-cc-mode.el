;; CC-MODE
;; Requires: c-eldoc

(defun my-cc-newline-and-indent ()
  "Append a newline first if the cursor is between { and }."
  (interactive)
  (when (and (not (nth 8 (syntax-ppss)))
	     (looking-back "{\s*")
	     (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (newline-and-indent))

(defun c++-to-headers-mode ()
  "Change the mode of a c++ header file to c++-mode if there is
at least one .cpp file in the same directory."
  (when (and (s-ends-with? ".h" (buffer-file-name))
             (eq major-mode 'c-mode)
             (delete-if-not
              (lambda (f) (or (s-ends-with? ".cc" f) (s-ends-with? ".cpp" f)))
              (directory-files default-directory)))
    (c++-mode)))

(setq google-like-c-style
  '("google"
    (c-offsets-alist
      (access-label . [1]))))


(defun fakedrake-cc-mode-init ()
  "Just some initializations I need for C"
  (c++-to-headers-mode)

  (define-key c-mode-base-map (kbd "C-M-n") 'c-end-of-defun)
  (define-key c-mode-base-map (kbd "C-M-p") 'c-beginning-of-defun)
  (define-key c-mode-base-map (kbd "M-n") 'c-end-of-statement)
  (define-key c-mode-base-map (kbd "M-p") 'c-beginning-of-statement)
  (define-key c-mode-base-map (kbd "C-j") 'my-cc-newline-and-indent)
  (define-key c-mode-base-map (kbd "C-x <SPC>") 'gud-break)
  (c-add-style "google-like" google-like-c-style)
  (setq c-default-style "google-like" c-basic-offset 4))

(setq compilation-scroll-output t)
(add-hook 'c-mode-common-hook 'fakedrake-cc-mode-init)

(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-to-list 'ido-ignore-buffers ".*-preprocessed\*")

;; ;; Sometimes I dont want emacs to indent comments at all.
;; (setq fd-c-disable-comments-lineup nil)
;; (defadvice c-lineup-C-comments (around c-lineup-C-comments-handle-doxygen activate)
;;   (let ((looking-at (if fd-c-disable-comments-lineup (lambda (s) nil) (symbol-function 'looking-at))))
;;     ad-do-it))
(setq c-macro-prompt-flag t)

(defun c-comment-includes ()
  (save-excursion
    (goto-char (point-min))
    ;; preserve the char count
    (replace-string "#include" "// clude")))

(add-hook 'c-macro-expansion-pre-hook 'c-comment-includes)
(defadvice c-macro-expansion (around c-macro-expansion-hooks activate)
  (let ((contents (buffer-string)))
    (with-temp-buffer
      (insert contents)
      (run-hooks 'c-macro-expansion-pre-hook)
      ad-do-it)))
(provide 'fd-cc-mode)
