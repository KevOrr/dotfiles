;;; ~/.config/doom/autodpi.el -*- lexical-binding: t; -*-

;; On one monitor, which is 14 inches and 158.2 dpi, I enjoy 14pt
(defvar +private/measured-monitor-inches 14.0)
(defvar +private/measured-monitor-dpi 158.2)
(defvar +private/target-pt 14.0)

;; https://emacs.stackexchange.com/a/44930
(defun +private//pyth (w h)
  (sqrt (+ (* w w)
           (* h h))))

(defun +private/frame-monitor-diag-pixels (&optional frame)
  (cl-destructuring-bind (_ _ w h) (frame-monitor-geometry frame)
    (when (and w h)
      (+private//pyth w h))))

(defun +private/frame-monitor-diag-inch (&optional frame)
  (cl-destructuring-bind (w h) (frame-monitor-attribute 'mm-size frame)
    (when (and w h)
      (/ (+private//pyth w h)
         25.4))))

(defun +private/get-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (if-let ((pixels (+private/frame-monitor-diag-pixels frame))
           (inches (+private/frame-monitor-diag-inch frame)))
      (/ pixels inches)))

(defun +private/desired-font-pt (&optional frame)
  (if-let* ((pt-per-dpi (/ +private/target-pt +private/measured-monitor-dpi))
            (pt-per-sqrt-in (/ +private/target-pt (sqrt +private/measured-monitor-inches)))
            (dpi (+private/get-dpi frame))
            (sqrt-in (sqrt (+private/frame-monitor-diag-inch frame))))
      (ceiling
       (+ (* pt-per-dpi dpi)
          (* pt-per-sqrt-in sqrt-in))
       2)))
