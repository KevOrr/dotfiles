;;; coq.el -*- lexical-binding: t; -*-

;; From https://gist.github.com/anton-trunov/b55fae56f92c35531fc480232bc74160
;; and https://github.com/ProofGeneral/PG/issues/210
;; The following works with OPAM 2.0.x
;; Put this piece of code into your .emacs and use it interactively as
;; M-x coq-change-compiler

(require 'dash)
(require 'consult)

(defun opam-installed-switches ()
  "Get a list of all installed OPAM switches."
  (process-lines "opam" "switch" "--safe" "list" "--short"))

(defun opam-current-switch ()
  "Get the current OPAM switch."
  (ignore-errors (car (process-lines "opam" "switch" "--safe" "show"))))

(defun get-opam-var (var &optional switch)
  (condition-case nil
      (car (apply #'process-lines "opam" "var" "--safe"
                  (append (if switch (list "--switch" switch)) (list var))))
    (error nil)))

(defun get-coq-vars (&optional switch)
  "Get coq:version and coq:bin for switch"
  (let ((version (get-opam-var "coq:version" switch))
        (bin (get-opam-var "coq:bin" switch)))
    (unless (or (null version) (null bin))
      (cons version bin))))

(setq opam-current-switch (opam-current-switch))

(defun coq-opam-switches-with-coq-to-bindir ()
  "Returns alist of (\"<OPAM switch> (<Coq version>)\" . <Coq's binary directory>."
  (->> (opam-installed-switches)
       (mapcar
        (lambda (switch)
          (and-let*
              ((coq-version (get-opam-var "coq:version" switch))
               (coq-bin (get-opam-var "coq:bin" switch)))
            (cons
             (concat switch " (coq-" coq-version ")")
             coq-bin))))
       (remq nil)))


(defun coq-change-compiler (coq-bin-dir)
  "Change Coq executables to use those in COQ-BIN-DIR."
  (interactive
   (let* ((name (consult--read
                 coq-bin-dirs-alist
                 :prompt "Compiler: "
                 :require-match t
                 :default opam-current-switch))
          (switch (cdr (assoc name alist))))
     (list switch)))
  (when (stringp coq-bin-dir)
    ;; (when-let ((opam-current-switch (car (rassoc coq-bin-dir (coq-bin-dirs-alist)))))
    ;;   (set 'opam-current-switch (car (split-string opam-current-switch))))
    (setq coq-compiler (concat coq-bin-dir "/coqc"))
    (setq coq-prog-name (concat coq-bin-dir "/coqtop"))
    (setq coq-dependency-analyzer (concat coq-bin-dir "/coqdep"))
    (message "Using Coq binaries from %s." coq-bin-dir)
    (when (proof-shell-available-p)
      (proof-shell-exit))))

(defvar coq-bin-dirs-alist
  (let* ((switches (coq-opam-switches-with-coq-to-bindir))
         (switch
          (car (--filter (string-prefix-p (concat opam-current-switch " (coq-") (car it))
                         switches))))
    (if switch
        (coq-change-compiler (cdr switch))
      (warn "Current opam switch %s not found" opam-current-switch))
    switches))

(use-package! company-coq
  :defer t
  :config
  (pushnew! company-coq-disabled-features 'spinner))
