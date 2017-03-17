(defconst monte-packages
  ;;'((monte-emacs :location (recipe :fetcher github :repo "kevorr/monte-emacs" :files "elisp/*.el"))
  '((monte-emacs :location local)))

(defun monte/init-monte-emacs ()
  (use-package monte-emacs
    :defer t
    :mode ("\\.mt\\'" . monte-mode)))
