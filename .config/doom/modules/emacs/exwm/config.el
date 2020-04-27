;;; emacs/exwm/config.el -*- lexical-binding: t; -*-
;;; Original author: @djeis97

(defvar +exwm-doom-leader nil)

(use-package exwm
  :commands (exwm-enable)
  :init
  ;; (add-hook 'exwm-init-hook 'exwm/add-ivy-persp-advice)

  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  ;; disable dialog boxes since they are unusable in exwm
  (setq use-dialog-box nil)
  :config
  (add-hook! exwm-mode #'doom-mark-buffer-as-real-h)
  (exwm-input-set-key (kbd (or +exwm-doom-leader doom-leader-alt-key)) doom-leader-map)

  ;; `exwm-input-set-key' allows you to set a global key binding (available in
  ;; any case). Following are a few examples.
  ;; + We always need a way to go back to line-mode from char-mode
  (exwm-input-set-key (kbd "s-r") 'exwm-reset)
  ;; + Bind a key to switch workspace interactively
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  ;; + Set shortcuts to switch to a certain workspace.
  (exwm-input-set-key (kbd "s-1") (lambda! (exwm-workspace-switch 0)))
  (exwm-input-set-key (kbd "s-2") (lambda! (exwm-workspace-switch 1)))
  (exwm-input-set-key (kbd "s-3") (lambda! (exwm-workspace-switch 2)))
  (exwm-input-set-key (kbd "s-4") (lambda! (exwm-workspace-switch 3)))
  (exwm-input-set-key (kbd "s-5") (lambda! (exwm-workspace-switch 4)))
  (exwm-input-set-key (kbd "s-6") (lambda! (exwm-workspace-switch 5)))
  (exwm-input-set-key (kbd "s-7") (lambda! (exwm-workspace-switch 6)))
  (exwm-input-set-key (kbd "s-8") (lambda! (exwm-workspace-switch 7)))
  (exwm-input-set-key (kbd "s-9") (lambda! (exwm-workspace-switch 8)))
  (exwm-input-set-key (kbd "s-0") (lambda! (exwm-workspace-switch 9)))
  ;; + Application launcher ('M-&' also works if the output buffer does not
  ;;   bother you). Note that there is no need for processes to be created by
  ;;   Emacs.
  (exwm-input-set-key (kbd "s-SPC") #'djeis97/exwm-app-launcher)
  ;; The following example demonstrates how to set a key binding only available
  ;; in line mode. It's simply done by first push the prefix key to
  ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
  ;; The example shorten 'C-c q' to 'C-q'.
  (push ?\C-q exwm-input-prefix-keys)
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (when (featurep! +systray)
    (require 'exwm-systemtray)
    (exwm-systemtray-enable)
    (setq exwm-workspace-show-all-buffers nil)
    (setq exwm-systemtray-height 11))

  (when (featurep! +attached-minibuffer)
    (add-hook! exwm-init #'exwm-workspace-attach-minibuffer))

  (if (featurep! +managed-randr)
      (progn
        (require 'exwm-randr)
        (add-hook! (exwm-init exwm-randr-refresh) #'djeis97/exwm-update-randr)
        (exwm-randr-enable))
    (setq exwm-workspace-number 1))

  (when (featurep! +exim)
    (push ?\C-\\ exwm-input-prefix-keys)))

(use-package exim
  :when (featurep! +exim)
  :after exwm
  :demand t
  :hook (exwm-mode . evil-emacs-state)
  :hook (exwm-init . exim-start))

(use-package exwm-edit
  :when (featurep! +exwm-edit)
  :after exwm
  :demand t)
