;;; ~/.doom.d/+exwm.el -*- lexical-binding: t; -*-
;;; Original author: @djeis97

(setq +exwm-doom-leader "<f20>")

(defun +private/exwm-runner (pname pbuffer command)
  (let ((proc (start-process-shell-command pname pbuffer command)))
    (when pbuffer
      (with-current-buffer (process-buffer proc)
        (comint-mode)
        (doom-mark-buffer-as-real-h)))))

(after! exwm
  (add-hook! (exwm-update-title)
    (exwm-workspace-rename-buffer exwm-title))

  (setq exwm-systemtray-height 24)

  (add-to-list 'exwm-manage-configurations
               '((string= exwm-instance-name "pinentry-gtk-2")
                 char-mode t))
  (add-to-list 'exwm-manage-configurations
               '((string= exwm-instance-name "pinentry")
                 char-mode t))

  (exwm-input-set-key (kbd "s-c") #'exwm-input-release-keyboard)
  (exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)

  (add-hook!
   'exwm-init-hook
   (+private/exwm-runner "Slack" "*Slack*" "slack")
   (+private/exwm-runner "polybar" "*Polybar*" "polybar main")
   (+private/exwm-runner "dunst" "*Dunst*" "dunst")
   )

  )
