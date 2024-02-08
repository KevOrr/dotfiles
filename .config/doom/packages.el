;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! benchmark-init)

;; util
(package! flycheck-pos-tip)
(package! open-junk-file)

;; org
(package! org-ref)
(package! ebib)
(package! org-roam-bibtex)
;;(package! org-roam-server)
;; (package! ivy-bibtex)

;; lisp
(package! evil-cleverparens)

;; git
(package! git-auto-commit-mode)

;; eaf
(package! eaf
  :recipe (:host github :repo "manateelazycat/emacs-application-framework"))

;; unibeautify
(package! unibeautify
  :recipe (:host github :repo "Unibeautify/emacs"))

;; Idris 2
(package! idris2-mode
  :recipe (:host github :repo "redfish64/idris2-mode"))

;; Just
(package! just-mode)
