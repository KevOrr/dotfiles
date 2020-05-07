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
  (setq org-preview-latex-default-process 'dvisvgm)
  (add-hook! org-mode #'auto-fill-mode))

(map! :leader
      :desc "M-x" "SPC" #'counsel-M-x
      :desc "M-x" "<f20>" #'counsel-M-x

      (:prefix-map ("a" . "applications")
       :desc "Launch application" "a" #'counsel-linux-app)

      ;; Notes
      (:prefix "n"
       ;; org-journal
       (:prefix "j"
        :desc "Open Journal" "o" (lambda () (interactive) (org-journal-new-entry 0))))

      ;; Files
      (:prefix "f"
       "J" #'open-junk-file)

      ;; Windows
      (:prefix "w"
       "/" #'evil-window-vsplit
       "-" #'evil-window-split))

;; https://emacs.stackexchange.com/a/44930
(defun kevorr//pyth (w h)
  (sqrt (+ (* w w)
           (* h h))))

(defun kevorr/frame-monitor-diag-pixels (&optional frame)
  (cl-destructuring-bind (_ _ w h) (frame-monitor-geometry frame)
    (kevorr//pyth w h)))

(defun kevorr/frame-monitor-diag-inch (&optional frame)
  (cl-destructuring-bind (w h) (frame-monitor-attribute 'mm-size frame)
    (/ (kevorr//pyth w h)
       25.4)))

(defun kevorr/get-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (/ (kevorr/frame-monitor-diag-pixels frame)
     (kevorr/frame-monitor-diag-inch frame)))

;; On one monitor, which is 14" and 158.2 dpi, I enjoy 14pt
(defvar kevorr/measured-monitor-inches 14.0)
(defvar kevorr/measured-monitor-dpi 158.2)
(defvar kevorr/target-pt 14.0)

(defun kevorr/desired-font-pt (&optional frame)
  (let* ((pt-per-dpi (/ kevorr/target-pt kevorr/measured-monitor-dpi))
         (pt-per-sqrt-in (/ kevorr/target-pt (sqrt kevorr/measured-monitor-inches)))
         (dpi (kevorr/get-dpi frame))
         (sqrt-in (sqrt (kevorr/frame-monitor-diag-inch frame))))
    (ceiling
     (+ (* pt-per-dpi dpi)
        (* pt-per-sqrt-in sqrt-in))
     2)))

(setq
 ;; doom
 doom-font (font-spec :family "Source Code Pro" :size (kevorr/desired-font-pt))
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

 ;; org
 org-directory "~/Documents/org/"
 org-image-actual-width nil
 org-log-into-drawer t
 ;; TODO who is overriding this?
 org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)"))
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

 ;; org-journal
 org-journal-dir "~/Documents/org/journal"
 org-journal-file-type 'weekly
 org-journal-date-format "%A, %d %B %Y"
 org-journal-time-format "TODO "
 org-journal-file-format "%Y-%m-%d.org"
 org-journal-enable-agenda-integration t
 ;; org-journal-enable-cache t
 org-journal-hide-entries-p nil
 ;; carry over all TO-DO items that are not in a "done"-like state
 org-journal-carryover-items "/!"
 org-journal-search-results-order-by :desc

 ;; org-babel
 org-babel-load-languages '((python . t)
                            (shell . t)
                            (dot . t)
                            (emacs-lisp . t)
                            (lisp . t))
 ;; org-confirm-babel-evaluate 'personal-layer/org-confirm-babel-evaluate

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
