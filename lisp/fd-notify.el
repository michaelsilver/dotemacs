;; Notifications notify for mac os x
(defvar terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier.")

                                        ; (terminal-notifier-notify "Emacs notification" "Something amusing happened")
(when terminal-notifier-command
  ;; A nasty hack to override dbus. You will need terminal-notifier
  (defun notifications-notify (&rest params)
    "Show a message with `terminal-notifier-command`."
    (let* ((title (plist-get params :title))
           (body (plist-get params :body))
           (app-icon (plist-get params :app-icon))
           (app-name (plist-get params :app-name))
           (actions (plist-get params :actions))
           (urgency (plist-get params :urgency))
           (timeout (plist-get params :timeout))
           (bus (plist-get params :bus))
           (replaces-id (plist-get params :replaces-id))
           (args (append
                  (and title (list "-title" title))
                  (and body (list "-message" body))
                  (and app-icon (list "-appIcon" app-icon))
                  (and app-name (list "-group" app-name))
                  (and app-name (list "-subtitle" app-name))
                  '("-activate" "org.gnu.Emacs"))))

      (apply 'start-process "terminal-notifier"
             "*terminal-notifier*"
             terminal-notifier-command
             args)))
  )



(provide 'fd-notify)
