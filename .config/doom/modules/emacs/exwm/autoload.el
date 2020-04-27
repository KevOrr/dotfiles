;;; emacs/exwm/autoload.el -*- lexical-binding: t; -*-
;;; Original author: @djeis97


(defun exwm//flatenum (i ls)
  (if ls (cons i (cons (first ls) (exwm//flatenum  (1+ i) (cdr ls)))) (list)))

;;;###autoload
(defun djeis97/exwm-update-randr ()
  (let ((exwm--randr-displays (split-string
                               (shell-command-to-string
                                "xrandr | grep ' connected' | cut -d' ' -f1 "))))
    (setq exwm-workspace-number (list-length exwm--randr-displays))
    (setq exwm-randr-workspace-monitor-plist (exwm//flatenum 0 exwm--randr-displays))))


;;;###autoload
(defun exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))


;;;###autoload
(defun djeis97/exwm-app-launcher (command)
  "Launches an application in your PATH.
  Can show completions at point for COMMAND using helm or ido"
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (start-process-shell-command command nil command))
