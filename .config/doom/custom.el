(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-haskell-formatting-provider "fourmolu")
 '(safe-local-variable-values
   '((+format-with . yapf)
     (+format-with-lsp)
     (+format-with . :none)
     (flycheck-clang-language-standard . c++20)
     (flycheck-gcc-language-standard . c++20)
     (cider-clojure-cli-global-options "-A:capstone")
     (cider-figwheel-main-default-options . ":dev")
     (cider-default-cljs-repl . figwheel-main)
     (cider-clojure-cli-global-options . "-A:dev")
     (gac-automatically-add-new-files-p)
     (gac-automatically-add-new-files-p . t)
     (gac-automatically-push-p)
     (gac-automatically-push-p . t)
     (+org-roam-open-buffer-on-find-file)
     (+org-roam-open-buffer-on-find-file . t)))
 '(warning-suppress-types
   '((idris2-load-file-success-hook)
     ((undo discard-info))
     ((undo discard-info))
     (before-save-hook)
     (before-save-hook)
     (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-headerline-breadcrumb-path-hint-face ((t (:underline nil :inherit lsp-headerline-breadcrumb-path-face))) t)
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t (:underline nil :inherit lsp-headerline-breadcrumb-symbols-face))) t)
 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:foreground "dim gray" :underline nil))) t)
 '(proof-locked-face ((t (:background "gray10"))))
 '(slime-highlight-edits-face ((t (:background "gray10")))))
(put 'narrow-to-region 'disabled nil)
