;;; coq.el -*- lexical-binding: t; -*-

;; From https://gist.github.com/anton-trunov/b55fae56f92c35531fc480232bc74160
;; and https://github.com/ProofGeneral/PG/issues/210
;; The following works with OPAM 2.0.x
;; Put this piece of code into your .emacs and use it interactively as
;; M-x coq-change-compiler
;; If you change your OPAM installation by e.g. adding more switches, then
;; run M-x coq-update-opam-switches and coq-change-compiler will show the updated set of switches.

(defun opam-installed-switches ()
  "Get a list of all installed OPAM switches."
  (process-lines "opam" "switch" "--safe" "list" "--short"))

(defun opam-current-switch ()
  "Get the current OPAM switch."
  (ignore-errors (car (process-lines "opam" "switch" "--safe" "show"))))

(defun opam-config-list (switch package &rest packages)
  "Get config variables for packages in switch"
  (let ((lines (apply #'process-lines (append (list "opam" "config" "list" "--safe")
                                              (if switch (list "--switch" switch))
                                              (list package)
                                              packages))))
    (save-match-data
      (-non-nil
       (--map
        (when (string-match "^\\([^:]+\\):\\([^ \t]+\\)[ \t]*\\(.+?\\)[ \t]+#" it)
          (cons (cons (match-string 1 it)
                      (match-string 2 it))
                (match-string 3 it)))
        lines)))))

(setq opam-current-switch (opam-current-switch))

(defun coq-opam-switches-with-coq-to-bindir ()
  "Returns alist of (\"<OPAM switch> (<Coq version>)\" . <Coq's binary directory>."
  (--filter
   (stringp (cdr it))
   (-map
    (lambda (switch)
      (let ((coq-vars (opam-config-list switch "coq")))
        (cons
         (concat switch " (coq-" (cdr (assoc '("coq" . "version") coq-vars)) ")")
         (cdr (assoc '("coq" . "bin") coq-vars)))))
    (opam-installed-switches))))

(setq coq-bin-dirs-alist (coq-opam-switches-with-coq-to-bindir))

(defun coq-update-opam-switches ()
  "Update the alist of OPAM switches with Coq installed"
  (interactive)
  (setq coq-bin-dirs-alist (coq-opam-switches-with-coq-to-bindir))
  (message "OPAM switches updated."))

(defun coq-change-compiler (coq-bin-dir)
  "Change Coq executables to use those in COQ-BIN-DIR."
  (interactive
   (list (cdr (assoc (ivy-read "Compiler: " coq-bin-dirs-alist
                               :require-match t
                               :preselect opam-current-switch)
                     coq-bin-dirs-alist))))
  (when (stringp coq-bin-dir)
    (when-let ((opam-current-switch (car (rassoc coq-bin-dir coq-bin-dirs-alist))))
      (set 'opam-current-switch (car (split-string opam-current-switch))))
    (setq coq-compiler (concat coq-bin-dir "/coqc"))
    (setq coq-prog-name (concat coq-bin-dir "/coqtop"))
    (setq coq-dependency-analyzer (concat coq-bin-dir "/coqdep"))
    (message "Using Coq binaries from %s." coq-bin-dir)
    (when (proof-shell-available-p)
      (proof-shell-exit))))

(coq-change-compiler (cdr (assoc opam-current-switch coq-bin-dirs-alist)))

(use-package! company-coq
  :defer t
  :config
  (pushnew! company-coq-disabled-features 'spinner))
