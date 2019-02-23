(defconst personal-layer-packages
  '(slime-company
    slime
    python
    ;; web-mode
    dired-du
    (python-smartparens-fixes :location local)
    keyfreq
    jinja2-mode
    (highlight-indent-guides :toggle personal-layer-use-highlight-indent-guides)
    (ligatti-langs :location local)
    bison-mode
    adjust-parens
    inf-clojure))
    ;; (processing2-emacs :location (recipe :fetcher github
    ;;                                      :repo "ptrv/processing2-emacs"))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq org-pretty-entities t)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'lisp-mode-hook 'parinfer-mode)
(add-hook 'web-mode-hook 'hungry-delete-mode)

;; To be set in .dir-locals.el
;; Set `c-project-include' to be a string or list of strings for project-relative
;; include directories
(setq-default c-project-include nil)
(defun add-project-local-c-includes ()
  (let ((c-project-include (etypecase c-project-include
                             (list c-project-include)
                             (string (list c-project-include))))
        (root (ignore-errors (projectile-project-root))))
    (when root
      (dolist (item c-project-include)
        (add-to-list (make-local-variable 'flycheck-gcc-include-path)
                     (concat (file-name-as-directory root) item))))))
(add-hook 'c-mode 'add-project-local-c-includes)
(add-hook 'c++-mode 'add-project-local-c-includes)

(setq-default
 tramp-use-ssh-controlmaster-options nil
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "xdg-open"
 c-basic-offset 4
 c-default-style '((c-mode . "bsd")
                   (c++-mode . "bsd")
                   (java-mode . "java")
                   (other . "bsd"))
 custom-safe-themes '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)
 custom-unlispify-remove-prefixes t
 evil-want-Y-yank-to-eol nil
 flycheck-pylintrc "~/.pylintrc"
 org-babel-load-languages '((python . t)
                            (shell . t)
                            (dot . t)
                            (emacs-lisp . t)
                            (lisp . t))
 org-confirm-babel-evaluate 'personal-layer/org-confirm-babel-evaluate
 org-image-actual-width nil
 org-log-into-drawer t
 org-todo-keywords '((sequence "TODO" "DONE(!)"))
 org-latex-pdf-process '("latexmk -pdf %f && latexmk -c %f")
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
 processing-location "/home/kevin/.local/share/processing/processing-java"
 processing-application-dir "/home/kevin/.local/share/processing"
 processing-sketchbook-dir "/home/kevin/sketchbook"
 )
(custom-set-faces
 '(slime-highlight-edits-face ((t (:background "black"))))
 '(proof-locked-face ((t (:background "gray10"))))
 )

(defun personal-layer/post-init-slime-company ())

(defun personal-layer/init-dired-du ()
  (use-package dired-du
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
      (push 'slime-highlight-edits slime-contribs)
    (setq slime-contribs (delete 'slime-highlight-edits slime-contribs))))

(defun personal-layer/post-init-python ()
  (add-to-list 'auto-mode-alist '("\\.pyt\\'" . python-mode)))

(defun personal-layer/post-init-org-plus-contrib ()
  (defvar personal-layer//org-babel-trusted-blocks nil)

  (defun personal-layer/org-confirm-babel-evaluate (lang body)
    (let ((trusted (find (cons lang body) personal-layer//org-babel-trusted-blocks :test 'equal)))
      (if trusted
          nil                           ; Don't need to prompt
        (if (not (y-or-n-p "Always trust this block?"))
            t                           ; need to prompt, don't store in trusted
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

(defun personal-layer/init-highlight-indent-guides ()
  (if personal-layer-use-highlight-indent-guides
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

(defun personal-layer/init-inf-clojure ()
  (use-package inf-clojure
    :defer t
    :init
    (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
    (advice-add 'inf-clojure-eldoc-setup :around
                (lambda (orig-fun &rest args) nil))))

;; (defun personal-layer/init-processing2-emacs ()
;;   (use-package processing2-emacs
;;     :defer t
;;     :commands (processing-mode)
;;     :mode ("\\.pde\\'" . processing-mode)))
