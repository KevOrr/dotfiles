;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defmacro setq-all (val &rest syms)
  (pcase syms
    ('() val)
    (`(,sym . ,rest) `(setq ,sym (setq-all ,val ,@rest)))))

(load! "exwm.el")
(load! "lisp.el")
(load! "autodpi.el")
(load! "org-setup.el")
(load! "coq.el")
(load! "git.el")

;; Uncomment in order to "permanently" show workspaces list in minibuffer
;; (after! persp-mode
;;   (defun display-workspaces-in-minibuffer ()
;;     ;; (with-current-buffer " *Minibuf-0*"
;;     ;;   (erase-buffer)
;;     ;;   (insert (+workspace--tabline)))
;;     )
;;   (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;   (+workspace/display))

(after! lsp-mode
  (custom-set-faces
   '(lsp-headerline-breadcrumb-path-hint-face ((t (:underline nil :inherit lsp-headerline-breadcrumb-path-face))) t)
   '(lsp-headerline-breadcrumb-symbols-hint-face ((t (:underline nil :inherit lsp-headerline-breadcrumb-symbols-face))) t)
   '(lsp-lsp-flycheck-info-unnecessary-face ((t (:foreground "dim gray" :underline nil))) t)
   ))

(after! magit
  (magit-wip-mode +1))

(use-package! open-junk-file
  :config
  (setq open-junk-file-format "~/dropbox/junk/%Y/%m/%d-%H%M%S."))

(use-package! git-auto-commit-mode
  :defer t
  :init
  (push '(git-auto-commit-mode +1) safe-local-eval-forms)
  (pushnew! safe-local-variable-values
            '(gac-automatically-push-p . t)
            '(gac-automatically-push-p . nil)
            '(gac-automatically-add-new-files-p . t)
            '(gac-automatically-add-new-files-p . nil))
  :config
  (setq gac-automatically-push-p nil)
  (setq gac-automatically-add-new-files-p nil))

(use-package! eaf
  :load-path (lambda () (concat doom-local-dir "straight/repos/emacs-application-framework"))
  :hook (eaf-mode . doom-mark-buffer-as-real-h))

(use-package! unibeautify)

(use-package! idris2-mode)

(defun +private/find-junk-file ()
  (interactive)
  (let ((file (format-time-string open-junk-file-format (current-time))))
    (let ((default-directory (file-name-directory file)))
      (find-file (file-name-nondirectory file)))))

(map! :map ivy-minibuffer-map
      "C-h" #'ivy-backward-kill-word)

(use-package! format-all
  :defer t
  :config

  (define-format-all-formatter yapf
    (:executable "yapf")
    (:install "pip install yapf")
    (:modes python-mode)
    (:format (format-all--buffer-easy executable )))

  (define-format-all-formatter ormolu
    (:executable "ormolu")
    (:install "cabal install ormolu")
    (:modes "Haskell" "Literate Haskell")
    (:format (format-all--buffer-easy executable)))

  (define-format-all-formatter formolu
    (:executable "formolu")
    (:install "cabal install formolu")
    (:modes "Haskell" "Literate Haskell")
    (:format (format-all--buffer-easy executable)))

  (setq-default format-all-formatters
                '(("Python" yapf)
                  ("Haskell" formolu)
                  ))

  (setq-hook! 'haskell-mode-hook +format-with :none))

(defun +private/start-gnome-control-center (&rest panel-and-args)
  (interactive)
  (apply #'start-process "gnome-control-center" nil "gnome-control-center"
         panel-and-args))

(defun +private/start-gnome-control-center (&optional panel &rest args)
  (interactive)
  (apply #'start-process "gnome-control-center" nil "gnome-control-center"
         (if panel
             (list* panel args)
           args)))

(map! :leader
      :desc "M-x" "SPC" 'execute-extended-command
      :desc "M-x" "<f20>" 'execute-extended-command

      (:prefix ("a" . "applications")
       :desc "Launch application" "a" #'counsel-linux-app
       :desc "List processes" "p" #'list-processes
       :desc "Run screen layout" "l" #'+private/run-screen-layout
       (:prefix ("s" . "settings")
        :desc "Settings" "s" #'+private/start-gnome-control-center
        :desc "Wi-Fi" "w" (cmd! (+private/start-gnome-control-center "wifi"))
        :desc "Bluetooth" "b" (cmd! (+private/start-gnome-control-center "bluetooth"))
        :desc "Network" "n" (cmd! (+private/start-gnome-control-center "network"))
        :desc "Power" "p" (cmd! (+private/start-gnome-control-center "power"))
        :desc "Displays" "d" (cmd! (+private/start-gnome-control-center "display"))
        :desc "Printers" "P" (cmd! (+private/start-gnome-control-center "printers"))
        :desc "Date & Time" "t" (cmd! (+private/start-gnome-control-center "datetime"))))


      ;; Notes
      (:prefix "n"
       ;; org-journal
       (:prefix "j"
        :desc "Open Journal" "o" (cmd! (org-journal-new-entry 0))))

      ;; Files
      (:prefix "f"
       "J" #'+private/find-junk-file)

      ;; Windows
      (:prefix "w"
       "/" #'evil-window-vsplit
       "-" #'evil-window-split))

(after! vterm
  (evil-define-key 'motion vterm-mode-map [remap evil-paste-after] 'vterm-yank))

(use-package! agda2-mode
  :config
  (custom-set-faces
   '(agda2-highlight-unsolved-meta-face ((t (:underline (:style wave :color "red")))))
   '(agda2-highlight-unsolved-constraint-face ((t (:underline (:style wave :color "orange")))))
   ))

(set-formatter!
  'clang-format
  '("clang-format"
    ("-assume-filename=%S" (or buffer-file-name mode-result ""))
    "-style=file")
  :modes
  '((c-mode ".c")
    (c++-mode ".cpp")
    (java-mode ".java")
    (objc-mode ".m")
    (protobuf-mode ".proto")))

(setq-default
 ;; doom
 doom-font (font-spec :family "Source Code Pro" :size (+private/desired-font-pt))
 doom-theme 'doom-one
 enable-local-variables :safe
 window-divider-default-right-width 4

 ;; completion
 company-idle-delay 0.5
 company-minimum-prefix-length 2

 ;; t was buggy
 posframe-mouse-banish nil

 display-line-numbers-type t
 +treemacs-git-mode 'deferred
 ivy-magic-tilde nil
 tramp-use-ssh-controlmaster-options nil
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "xdg-open"
 c-basic-offset 4
 c-default-style '((c-mode . "bsd")
                   (c++-mode . "bsd")
                   (java-mode . "java")
                   (other . "bsd"))
 coq-compile-before-require t
 custom-safe-themes '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)
 custom-unlispify-remove-prefixes t
 evil-want-Y-yank-to-eol nil
 flycheck-pylintrc (expand-file-name "~/.pylintrc")
 lsp-pyright-diagnostic-mode "workspace"

 processing-location (expand-file-name "~/.local/share/processing/processing-java")
 processing-application-dir (expand-file-name "~/.local/share/processing")
 processing-sketchbook-dir (expand-file-name "~/sketchbook")
 ;; proof-three-window-enable nil
 ;; proof-three-window-mode-policy 'hybrid
 flycheck-ghc-args '("-Wall" "-Wmissing-exported-signatures" "-Wcompat" "-Widentities"
                     "-Wredundant-constraints" "-Wmissed-specialisations")
 flycheck-error-list-minimum-level 'warning
 flycheck-navigation-minimum-level 'warning
 doom-modeline-checker-simple-format nil
 )

(load! "config.local.el" nil t)
