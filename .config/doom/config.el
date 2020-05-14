;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defmacro setq-all (val &rest syms)
  (pcase syms
    ('() val)
    (`(,sym . ,rest) `(setq ,sym (setq-all ,val ,@rest)))))

(load! "exwm.el")
(load! "smartparens.el")
(load! "autodpi.el")
(load! "org-setup.el")

(use-package! open-junk-file
  :custom
  (open-junk-file-format "~/dropbox/junk/%Y/%m/%d-%H%M%S."))

(defun +private/find-junk-file ()
  (interactive)
  (let ((file (format-time-string open-junk-file-format (current-time))))
    (let ((default-directory (file-name-directory file)))
      (counsel-find-file (file-name-base file)))))

(map! :leader
      :desc "M-x" "SPC" #'counsel-M-x
      :desc "M-x" "<f20>" #'counsel-M-x

      (:prefix-map ("a" . "applications")
       :desc "Launch application" "a" #'counsel-linux-app)

      ;; Notes
      (:prefix "n"
       ;; org-journal
       (:prefix "j"
        :desc "Open Journal" "o" (lambda! (org-journal-new-entry 0))))

      ;; Files
      (:prefix "f"
       "J" #'+private/find-junk-file)

      ;; Windows
      (:prefix "w"
       "/" #'evil-window-vsplit
       "-" #'evil-window-split))

(after! vterm
  (evil-define-key 'motion vterm-mode-map [remap evil-paste-after] 'vterm-yank))

(setq
 ;; doom
 doom-font (font-spec :family "Source Code Pro" :size (+private/desired-font-pt))
 doom-theme 'doom-one

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
 coq-compiler (expand-file-name "~/.opam/coq8.11/bin/coqc")
 coq-prog-name (expand-file-name "~/.opam/coq8.11/bin/coqtop")
 coq-dependency-analyzer (expand-file-name "~/.opam/coq8.11/bin/coqdep")
 custom-safe-themes '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)
 custom-unlispify-remove-prefixes t
 evil-want-Y-yank-to-eol nil
 flycheck-pylintrc (expand-file-name "~/.pylintrc")

 processing-location (expand-file-name "~/.local/share/processing/processing-java")
 processing-application-dir (expand-file-name "~/.local/share/processing")
 processing-sketchbook-dir (expand-file-name "~/sketchbook")
 ;; proof-three-window-enable nil
 ;; proof-three-window-mode-policy 'hybrid
 flycheck-ghc-args '("-Wall" "-Wmissing-exported-signatures" "-Wcompat" "-Widentities"
                     "-Wredundant-constraints" "-Wmissed-specialisations"))

(custom-set-faces
 '(slime-highlight-edits-face ((t (:background "gray10"))))
 '(proof-locked-face ((t (:background "gray10")))))
