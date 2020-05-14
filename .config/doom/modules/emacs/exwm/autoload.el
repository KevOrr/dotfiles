;;; emacs/exwm/autoload.el -*- lexical-binding: t; -*-
;;; Original author: @djeis97

(defvar +exwm/app-launcher-prompt "Run shell command in background: "
  "Prompt for `+exwm/app-launcher'")

(defun +exwm//flatenum (i ls)
  (loop for i from i
        for l in ls
        append (list i l)))

;;;###autoload
(defun +exwm/update-randr ()
  (let ((exwm--randr-displays (split-string
                               (shell-command-to-string
                                "xrandr | grep ' connected' | cut -d' ' -f1 "))))
    (setq exwm-workspace-number (list-length exwm--randr-displays))
    (setq exwm-randr-workspace-monitor-plist (+exwm//flatenum 0 exwm--randr-displays))))


;;;###autoload
(defun +exwm/bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))


;;;###autoload
(defun +exwm/app-launcher (command)
  "Launches an application in your PATH.
  Can show completions at point for COMMAND using helm or ido"
  (interactive (list (read-shell-command +exwm/app-launcher-prompt)))
  (start-process-shell-command command nil command))
