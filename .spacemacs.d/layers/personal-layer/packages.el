(defconst personal-layer-packages
  '(evil-cleverparens
    slime-company
    slime
    python
    web-mode
    ))

(defun personal-layer/post-init-slime-company ()
  )

(defun personal-layer/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :defer t))

(defun personal-layer/post-init-evil-cleverparens ()
  (add-hook 'lisp-mode-hook 'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode))

(defun personal-layer/post-init-slime ()
  (push 'slime-highlight-edits slime-contribs))

(defun personal-layer/post-init-python ()
  (add-to-list 'auto-mode-alist '("\\.pyt\\'" . python-mode)))

(defun personal-layer/post-init-web-mode ()
  (add-hook 'web-mode-hook 'hungry-delete-mode))

(defun personal-layer/post-init-org-plus-contrib (0)
  (defvar personal-layer//org-babel-trusted-blocks nil)

  (defun personal-layer/org-confirm-babel-evaluate (lang body)
    (let ((trusted (find (cons lang body) personal-layer//org-babel-trusted-blocks :test 'equal)))
      (if trusted
          nil ; Don't need to prompt
        (if (not (y-or-n-p "Always trust this block?"))
            t ; need to prompt, don't store in trusted
          ;; Newly trusted, add to trusted list
          (push (cons lang body) personal-layer//org-babel-trusted-blocks)
          nil)))))
