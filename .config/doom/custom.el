(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((flycheck-clang-language-standard . c++20)
     (+format-with . yapf)
     (gac-automatically-add-new-files-p)
     (gac-automatically-add-new-files-p . t)
     (gac-automatically-push-p)
     (gac-automatically-push-p . t)
     (+org-roam-open-buffer-on-find-file)
     (+org-roam-open-buffer-on-find-file . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-unsolved-constraint-face ((t (:underline (:style wave :color "orange")))))
 '(agda2-highlight-unsolved-meta-face ((t (:underline (:style wave :color "red")))))
 '(lsp-headerline-breadcrumb-path-hint-face ((t (:underline nil :inherit lsp-headerline-breadcrumb-path-face))) t)
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t (:underline nil :inherit lsp-headerline-breadcrumb-symbols-face))) t)
 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:foreground "dim gray" :underline nil))) t)
 '(proof-locked-face ((t (:background "gray10"))))
 '(slime-highlight-edits-face ((t (:background "gray10")))))
(put 'narrow-to-region 'disabled nil)
(put 'list-threads 'disabled nil)
