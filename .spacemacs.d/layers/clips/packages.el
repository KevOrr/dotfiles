;;; packages.el --- clips layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Elijah Malaby <jay@XANA-laptop>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `clips-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `clips/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `clips/pre-init-PACKAGE' and/or
;;   `clips/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst clips-packages
  '(clips-mode
    parinfer
    smartparens)
  "The list of Lisp packages required by the clips layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun clips/init-clips-mode ()
  (use-package clips-mode
    :defer t
    :init
    (spacemacs/register-repl 'clips-mode 'run-clips "clips")
    :config
    (spacemacs/set-leader-keys-for-major-mode 'clips-mode
      "'" 'run-clips
      "ef" 'inf-clips-eval-form
      "eF" 'inf-clips-load-file
      "sR" 'inf-clips-reset-engine)
    (add-hook 'clips-mode-hook (lambda () (run-hooks 'change-major-mode-after-body-hook)))
    (setf inferior-clips-program "clips")))

(defun clips/post-init-parinfer ()
  (when (configuration-layer/package-used-p 'clips-mode)
    (add-hook 'clips-mode-hook 'parinfer-mode)))

(defun clips/post-init-smartparens ()
  (when (configuration-layer/package-used-p 'clips-mode)
    (add-hook 'clips-mode-hook 'smartparens-mode)))

;;; packages.el ends here
