;; -*- no-byte-compile: t; -*-
;;; Original author: @djeis97

(package! xelb :recipe (:type git :host github :repo "ch11ng/xelb"))
(package! exwm :recipe (:type git :host github :repo "ch11ng/exwm"))
(when (featurep! +exim)
  (package! exim :recipe (:type git :host github :repo "ch11ng/exim")))
(when (featurep! +exwm-edit)
  (package! exwm-edit :recipe (:type git :host github :repo "agzam/exwm-edit")))
