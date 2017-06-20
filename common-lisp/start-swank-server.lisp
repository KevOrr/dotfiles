(eval-when (:compile-toplevel :execute)
  #-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

(eval-when (:compile-toplevel :execute)
  (ql:quickload :swank))

;; (defun find-slime (&optional (where #p"~/.emacs.d/elpa/slime-*/"))
;;   (let ((directory where))
;;     (loop :for candidate :in (directory where)
;;           :if (cl-ppcre:))))

(defun run-server ()
  (let ((port-str (sb-posix:getenv "SYSTEMD_SWANK_PORT"))
        (swank:*communication-style* nil)
        (swank:*use-dedicated-output-stream* nil))
    (when port-str
      (swank:create-server :port (parse-integer port-str) :dont-close t))))

(run-server)
