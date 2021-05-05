;;; ~/.doom.d/+exwm.el -*- lexical-binding: t; -*-
;;; Original author: @djeis97

(setq +exwm-doom-leader "<f20>")

(defun +private/exwm-runner (pname pbuffer command)
  (let ((proc (start-process-shell-command pname pbuffer command)))
    (when pbuffer
      (with-current-buffer (process-buffer proc)
        (comint-mode)
        (doom-mark-buffer-as-real-h)))))

(defun +private/run-screen-layout ()
  (interactive)
  (ivy-read "Choose screen layout from ~/.screenlayout: "
            (cl-remove-if (lambda (s) (member s '("." "..")))
                          (directory-files "~/.screenlayout/"))
            :require-match t
            :action '(0
                      ("o" (lambda (f) (call-process (concat "~/.screenlayout/" f) nil 0)) "run")
                      ("v" (lambda (f) (find-file (concat "~/.screenlayout/" f))) "view"))
            :caller '+private/run-screen-layout))

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
   (+private/exwm-runner "polybar" "*Polybar*" "polybar -r main")
   (+private/exwm-runner "dunst" "*Dunst*" "dunst"))

  (defun +ivy-posframe-display-exwm (str)
    (ivy-posframe--display str
                           (lambda (info)
                             (let* ((workarea (elt exwm-workspace--workareas exwm-workspace-current-index))
                                    (x (aref workarea 0))
                                    (y (aref workarea 1))

                                    (fw (aref workarea 2))
                                    (fh (aref workarea 3))

                                    (pw (plist-get info :posframe-width))
                                    (ph (plist-get info :posframe-height)))

                               (cons (+ x (/ (- fw pw) 2)) (+ y (/ (- fh ph) 2)))))))

  (setq ivy-posframe-display-functions-alist
        '((t . +ivy-posframe-display-exwm))

        ivy-posframe-parameters '((parent-frame nil)
                                  (z-group . above)))

  ;; force set frame-position on every posframe display
  (advice-add 'posframe--set-frame-position :before
              (lambda (&rest _)
                (setq-local posframe--last-posframe-pixel-position nil))))
