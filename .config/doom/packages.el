;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! benchmark-init)

;; util
(package! flycheck-pos-tip)
(package! open-junk-file)
(package! format-all :pin "498cf73ddcf9f3c8b9400b16a5ee1272fb95e326")

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
