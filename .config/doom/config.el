;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "exwm.el")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name ""
;;       user-mail-address "")

(use-package! open-junk-file
  :custom
  (open-junk-file-format "~/dropbox/junk/%Y/%m/%d-%H%M%S."))

(after! org
  (require 'org-ref)
  (require 'org-ref-ivy-cite)
  (require 'org-roam-bibtex)
  (add-hook! org-roam-mode #'org-roam-bibtex-mode)
  (setq org-preview-latex-default-process 'dvisvgm))

(map! :leader
      :desc "M-x" "SPC" #'counsel-M-x
      :desc "M-x" "<f20>" #'counsel-M-x

      (:prefix-map ("a" . "applications")
       :desc "Launch application" "a" #'counsel-linux-app)

      ;; Files
      (:prefix "f"
       "J" #'open-junk-file)

      ;; Windows
      (:prefix "w"
       "/" #'evil-window-vsplit
       "-" #'evil-window-split))

;; https://emacs.stackexchange.com/a/44930
(defun kevorr/get-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))

(defvar kevorr/pt-per-dpi (/ 14 159.0))

(setq
 doom-font (font-spec :family "Source Code Pro"
                      :size (ceiling (* kevorr/pt-per-dpi (kevorr/get-dpi))))
 doom-theme 'doom-one
 org-directory "~/Documents/org/"
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
 org-journal-file-type 'weekly
 org-journal-date-format "%A, %d %B %Y"
 org-journal-time-format "TODO "
 org-journal-file-format "%Y-%m-%d.org"
 org-journal-enable-agenda-integration t
 org-journal-enable-cache t
 org-journal-hide-entries-p nil
 org-babel-load-languages '((python . t)
                            (shell . t)
                            (dot . t)
                            (emacs-lisp . t)
                            (lisp . t))
 ;; org-confirm-babel-evaluate 'personal-layer/org-confirm-babel-evaluate
 org-image-actual-width nil
 org-log-into-drawer t
 org-todo-keywords '((sequence "TODO" "DONE(!)"))
 ;; org-latex-pdf-process '("latexmk -pdf %f && latexmk -c %f")
 org-latex-packages-alist '(("style=numeric,backend=biber" "biblatex" nil)
                            "\\addbibresource{references.bib}"
                            ("" "listings" nil)
                            ("" "sourcecodepro" nil)
                            ("" "mathtools" nil)
                            ("" "mathabx" nil))
 org-latex-listings nil
 org-latex-listings-options '(("prebreak" "\\dlsh")
                              ("basicstyle" "\\small\\ttfamily")
                              ("breaklines" "true"))
 org-startup-with-inline-images t
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
