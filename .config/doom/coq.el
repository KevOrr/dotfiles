;;; coq.el -*- lexical-binding: t; -*-

;; From https://gist.github.com/anton-trunov/b55fae56f92c35531fc480232bc74160
;; and https://github.com/ProofGeneral/PG/issues/210
;; The following works with OPAM 2.0.x
;; Put this piece of code into your .emacs and use it interactively as
;; M-x coq-change-compiler
;; If you change your OPAM installation by e.g. adding more switches, then
;; run M-x coq-update-opam-switches and coq-change-compiler will show the updated set of switches.

(require 'dash)
(require 'consult)

;;; Adapted from @djeis
(defun thready-process-lines (program &rest args)
  (with-temp-buffer
    (let ((proc (make-process
                 :name program
                 :buffer (current-buffer)
                 :stderr nil
                 :command (cons program args)
                 :connection-type 'pipe
                 :sentinel #'ignore)))
      (while (accept-process-output proc))
      (unless (eq (process-exit-status proc) 0)
        (error "%s exited with status %s" program (process-exit-status proc)))
      (goto-char (point-min))
      (let (lines)
        (while (not (eobp))
          (setq lines (cons (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                            lines))
          (forward-line 1))
        (nreverse lines)))))

(defun opam-installed-switches ()
  "Get a list of all installed OPAM switches."
  (thready-process-lines "opam" "switch" "--safe" "list" "--short"))

(defun opam-current-switch ()
  "Get the current OPAM switch."
  (ignore-errors (car (thready-process-lines "opam" "switch" "--safe" "show"))))

(defun opam-config-list (switch package &rest packages)
  "Get config variables for packages in switch"
  (let ((lines (apply #'thready-process-lines (append (list "opam" "config" "list" "--safe")
                                                      (if switch (list "--switch" switch))
                                                      (cons package packages)))))
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


;; (let ((tr (make-thread (lambda () (process-lines "sh" "-c" "sleep 5; echo hi")))))
;;   (defun foo ()
;;     (thread-join tr)))

;; (let ((tr (make-thread
;;            (lambda ()
;;              (with-temp-buffer
;;                (let ((p (make-process
;;                          :name "sh"
;;                          :buffer (current-buffer)
;;                          :stderr nil
;;                          :command '("sh" "-c" "sleep 5; echo hi")
;;                          :sentinel #'ignore)))
;;                  (while (accept-process-output p))
;;                  (buffer-string)))))))
;;   (defun foo ()
;;     (thread-join tr)))

;; let-over-defun doesn't seem to play nicely with either edebug or Doom's eval
;; module, so let's do this instead
(defun -make-coq-bin-dirs-alist-getter ()
  (let ((tr (make-thread
            (lambda ()
              (let* ((switches (coq-opam-switches-with-coq-to-bindir))
                     (switch
                      (car (--filter (string-prefix-p (concat opam-current-switch " (coq-") (car it))
                                     switches))))
                (if switch
                    (coq-change-compiler (cdr switch))
                  (warn "Current opam switch %s not found" opam-current-switch))
                switches))))
       (val nil))
   (lambda (&optional update)
     (cond
      (update (setq val (coq-opam-switches-with-coq-to-bindir)))
      ((null val) (setq val (thread-join tr))))
     val)))

(defvar get-coq-bin-dirs-alist (-make-coq-bin-dirs-alist-getter))

(defun coq-update-opam-switches ()
  "Update the alist of OPAM switches with Coq installed"
  (interactive)
  (funcall get-coq-bin-dirs-alist)
  (message "OPAM switches updated."))

(defun coq-change-compiler (coq-bin-dir)
  "Change Coq executables to use those in COQ-BIN-DIR."
  (interactive
   (let* ((alist (funcall get-coq-bin-dirs-alist))
          (name (consult--read
                 alist
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

(use-package! company-coq
  :defer t
  :config
  (pushnew! company-coq-disabled-features 'spinner))
