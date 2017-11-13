(defconst personal-layer-packages
  '(evil-cleverparens
    slime-company
    slime
    python
    web-mode
    dired-du
    (python-smartparens-fixes :location local)
    keyfreq
    jinja2-mode
    highlight-indent-guides
    (ligatti-langs :location local)
    bison-mode
    adjust-parens
    ))

(defgroup personal-layer nil
  "Personal spacemacs layer settings."
  :prefix "personal-layer-")

(defcustom personal-layer-use-highlight-indent-guides t
  "Load highlight-indent-guides package and add hooks defined in personal-layer on startup"
  :group 'personal-layer
  :type '(boolean))

(defcustom personal-layer-use-slime-highlight-edits t
  "Use slime-highlight-edits from slime-contribs"
  :group 'personal-layer
  :type '(boolean)
  :set (lambda (sym val)
         (message "Trying to set %S to value %S" sym val)
         (if val
             (spacemacs|use-package-add-hook slime
               :config (push 'slime-highlight-edits slime-contribs))
           (spacemacs|use-package-add-hook slime
             :config (setq slime-contribs (delete 'slime-highlight-edits slime-contribs))))
         (set sym val)))

(defun personal-layer/post-init-slime-company ())

(defun personal-layer/init-dired-du ()
  (use-package dired-du
    :defer t))

(defun personal-layer/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :defer t))

(defun personal-layer/init-python-smartparens-fixes ()
  (use-package python-smartparens-fixes
    :defer t
    :commands (python-smartparens-fixes-mode
               py-sp-raise-sexp
               py-sp-unwrap-sexp)
    :init (progn
            (add-hook 'python-mode-hook 'python-smartparens-fixes-mode)
            (add-hook 'inferior-python-mode-hook 'python-smartparens-fixes-mode))))

(defun personal-layer/post-init-evil-cleverparens ()
  (add-hook 'lisp-mode-hook 'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode))

(defun personal-layer/post-init-slime ()
  (if personal-layer-use-slime-highlight-edits
      (push 'slime-highlight-edits slime-contribs)))

(defun personal-layer/post-init-python ()
  (add-to-list 'auto-mode-alist '("\\.pyt\\'" . python-mode)))

(defun personal-layer/post-init-web-mode ()
  (add-hook 'web-mode-hook 'hungry-delete-mode))

(defun personal-layer/post-init-org-plus-contrib ()
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

(defun personal-layer/init-keyfreq ()
  (use-package keyfreq
    :defer t
    :config (progn
              (keyfreq-mode 1)
              (keyfreq-autosave-mode 1))))

(defun personal-layer/init-jinja2-mode ()
  (use-package jinja2-mode
    :defer t
    :config (add-to-list 'jinja2-user-keywords "assets")))

(if personal-layer-use-highlight-indent-guides
    (defun personal-layer/init-highlight-indent-guides ()
      (use-package highlight-indent-guides
        :defer t
        :config (progn
                  (setq highlight-indent-guides-method 'character)
                  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
                  (add-hook 'js-mode-hook 'highlight-indent-guides-mode)
                  (add-hook 'web-mode 'highlight-indent-guides-mode)
                  (add-hook 'c-mode-hook 'highlight-indent-guides-mode)
                  (add-hook 'c++-mode-hook 'highlight-indent-guides-mode)))))

(defun personal-layer/init-ligatti-langs ()
  (use-package ligatti-langs
    :defer t
    :mode ("\\.dj\\'" . dj-mode)
    :mode ("\\.dism\\'" . dism-mode)))

(defun personal-layer/init-bison-mode ()
  (use-package bison-mode
    :defer t))

(defun personal-layer/init-adjust-parens ()
  (use-package adjust-parens
    :defer t
    :commands (adjust-parens-mode)
    :init (add-hook 'lisp-mode-hook 'adjust-parens-mode)))
