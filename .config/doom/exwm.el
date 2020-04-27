;;; ~/.doom.d/+exwm.el -*- lexical-binding: t; -*-
;;; Original author: @djeis97

(setq +exwm-doom-leader "<f20>")

(defun djeis97/exwm-layout-toggle-fullscreen ()
  "Togggles full screen for Emacs and X windows"
  (interactive)
  (if exwm--id
      (exwm-layout-toggle-fullscreen exwm--id)))

(defun djeis97-exwm/runner (pname pbuffer command)
  (let ((proc (start-process-shell-command pname pbuffer command)))
    (when pbuffer
      (with-current-buffer (process-buffer proc)
        (comint-mode)
        (doom-mark-buffer-as-real-h)))))

(after! exwm
  (add-hook! (exwm-update-title)
    (cond
     ((and (string= "Google-chrome" exwm-class-name) (string-suffix-p "Cookie Clicker" exwm-title))
      (exwm-workspace-rename-buffer "Cookie Clicker"))
     ((string= "Vivaldi-stable" exwm-class-name)
      (exwm-workspace-rename-buffer (format "%s - Vivaldi Stable" exwm-title)))
     (t (exwm-workspace-rename-buffer exwm-title))))

  (add-to-list 'exwm-manage-configurations
               '((string= "Uzbl-core" exwm-class-name)
                 tiling-header-line (:eval (tabbar-line))
                 tiling-mode-line nil))
  (add-to-list 'exwm-manage-configurations
               '((string= exwm-instance-name "pinentry-gtk-2")
                 char-mode t))
  (add-to-list 'exwm-manage-configurations
               '((string= exwm-instance-name "pinentry")
                 char-mode t))

  (exwm-input-set-key (kbd "s-c") #'exwm-input-release-keyboard)
  (exwm-input-set-key (kbd "s-f") #'djeis97/exwm-layout-toggle-fullscreen)

  ;;(add-hook! 'exwm-init-hook
  ;;  (djeis97-exwm/runner "Dropbox" "*Dropbox*" "dropbox")
  ;;  (djeis97-exwm/runner "polybar" "*Polybar*" "polybar main")
  ;;  (djeis97-exwm/runner "dunst" "*Dunst*" "dunst"))
  )
