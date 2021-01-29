(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((cider-clojure-cli-global-options "-A:capstone")
     (cider-figwheel-main-default-options . ":dev")
     (cider-default-cljs-repl . figwheel-main)
     (cider-clojure-cli-global-options . "-A:dev")
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
 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:foreground "dim gray" :underline nil))) t)
 '(proof-locked-face ((t (:background "gray10"))))
 '(slime-highlight-edits-face ((t (:background "gray10")))))
