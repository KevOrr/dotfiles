;;; coq.el -*- lexical-binding: t; -*-

;; Lazy Coq/OPAM switch picker for Proof General.
;; No subprocesses run at file-load time. The switch list is computed on
;; first use and cached to disk; subsequent Emacs sessions read the cache.
;; Run M-x +private-coq-refresh-switches after installing/removing an OPAM switch.

(defvar +private-coq-opam-root nil)
(defun +private-coq-opam-root ()
  (or +private-coq-opam-root
      (setq +private-coq-opam-root
            (or (getenv "OPAMROOT")
                (expand-file-name "~/.opam")))))

(defvar +private-coq-current-switch 'unset)
(defun +private-coq-current-switch ()
  (when (eq +private-coq-current-switch 'unset)
    (setq +private-coq-current-switch
          (ignore-errors
            (car (process-lines "opam" "switch" "--safe" "show")))))
  +private-coq-current-switch)

(defun +private-coq-installed-switches ()
  (process-lines "opam" "switch" "--safe" "list" "--short"))

(defun +private-coq-switch-coq-version (switch)
  "Return Coq version installed in SWITCH, or nil if Coq isn't there.
One opam call per switch (replaces the previous coq:version + coq:bin pair)."
  (ignore-errors
    (let ((v (car (process-lines "opam" "list" "--safe"
                                 "--installed" "--short"
                                 "--columns=version"
                                 "--switch" switch "coq"))))
      (and v (not (string-empty-p v)) v))))

(defun +private-coq-switch-bin-dir (switch)
  "Derive the bin dir for SWITCH from the OPAM root layout."
  (expand-file-name (concat switch "/bin") (+private-coq-opam-root)))

(defvar coq-bin-dirs-cache-file
  (expand-file-name "coq-bin-dirs.eld" user-emacs-directory))

(defun +private-coq-load-cache ()
  (when (file-readable-p coq-bin-dirs-cache-file)
    (with-temp-buffer
      (insert-file-contents coq-bin-dirs-cache-file)
      (ignore-errors (read (current-buffer))))))

(defun +private-coq-save-cache (alist)
  (with-temp-file coq-bin-dirs-cache-file
    (prin1 alist (current-buffer))))

(defun +private-coq-compute-bin-dirs-alist ()
  "Scan OPAM switches and return alist of (\"<switch> (coq-<version>)\" . <bin>)."
  (delq nil
        (mapcar
         (lambda (switch)
           (when-let* ((version (+private-coq-switch-coq-version switch))
                       (bin (+private-coq-switch-bin-dir switch)))
             ;; Guard against path-derivation being wrong (e.g. local switches).
             (when (file-executable-p (expand-file-name "coqc" bin))
               (cons (format "%s (coq-%s)" switch version) bin))))
         (+private-coq-installed-switches))))

(defvar coq-bin-dirs-alist nil)
(defun coq-bin-dirs-alist (&optional refresh)
  "Return cached alist of OPAM switches with Coq.
With REFRESH non-nil, rescan and rewrite the on-disk cache."
  (cond
   ((and (not refresh) coq-bin-dirs-alist)
    coq-bin-dirs-alist)
   ((and (not refresh) (setq coq-bin-dirs-alist (+private-coq-load-cache)))
    coq-bin-dirs-alist)
   (t
    (setq coq-bin-dirs-alist (+private-coq-compute-bin-dirs-alist))
    (+private-coq-save-cache coq-bin-dirs-alist)
    coq-bin-dirs-alist)))

(defun coq-refresh-switches ()
  "Rescan OPAM switches for Coq and refresh the on-disk cache."
  (interactive)
  (coq-bin-dirs-alist t)
  (message "Coq switches refreshed: %d found" (length coq-bin-dirs-alist)))

(defun +private-coq-default-switch-entry ()
  "Find the alist entry corresponding to the current OPAM switch."
  (when-let ((current (+private-coq-current-switch)))
    (let ((prefix (concat current " (coq-")))
      (seq-find (lambda (it) (string-prefix-p prefix (car it)))
                (coq-bin-dirs-alist)))))

(defun coq-change-compiler (coq-bin-dir)
  "Change Coq executables to use those in COQ-BIN-DIR."
  (interactive
   (progn
     (require 'consult)
     (let* ((alist (coq-bin-dirs-alist))
            (default (car (+private-coq-default-switch-entry)))
            (name (consult--read alist
                                 :prompt "Compiler: "
                                 :require-match t
                                 :default default)))
       (list (cdr (assoc name alist))))))
  (when (stringp coq-bin-dir)
    (setq coq-compiler (concat coq-bin-dir "/coqc")
          coq-prog-name (concat coq-bin-dir "/coqtop")
          coq-dependency-analyzer (concat coq-bin-dir "/coqdep"))
    (message "Using Coq binaries from %s." coq-bin-dir)
    (when (and (fboundp 'proof-shell-available-p)
               (proof-shell-available-p))
      (proof-shell-exit))))

;; Apply the cached default switch only when a .v buffer is actually opened.
;; (Doom's :lang coq module loads proof-site at startup, so hooking that
;; would defeat the laziness.) Runs once per session, then removes itself.
(defun +private-coq-apply-default-switch ()
  (remove-hook 'coq-mode-hook #'+private-coq-apply-default-switch)
  (when-let ((entry (+private-coq-default-switch-entry)))
    (coq-change-compiler (cdr entry))))

(add-hook 'coq-mode-hook #'+private-coq-apply-default-switch)

(use-package! company-coq
  :defer t
  :config
  (pushnew! company-coq-disabled-features 'spinner))
