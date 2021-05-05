;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! benchmark-init)

;; util
(package! flycheck-pos-tip)
(package! open-junk-file)

;; org
(package! org-ref)
(package! ebib)
(package! org-roam-bibtex)
(package! org-roam-server)
(package! ivy-bibtex)

;; git
(package! git-auto-commit-mode)

;; eaf
(package! eaf
  :recipe (:host github :repo "manateelazycat/emacs-application-framework"))
