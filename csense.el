;;; csense.el --- Coding assistant front-end

;; Copyright (C) 2007  

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;  Tested on Emacs 22.

;;; Code:


;;; User configuration

(defvar csense-information-function nil
  "Function called with no arguments to get information about the symbol at point.

The function should return a plist with the follwing values:")

(make-variable-buffer-local 'csense-information-function)


(defvar csense-completion-function nil
  "Function called with no arguments to get completions for the symbol at point.

The function should return a list of completions available at point. Each completion
must be a plist with the follwing values:")

(make-variable-buffer-local 'csense-completion-function)


(defvar csense-max-tooltip-line-length 70
  "Maximum length of lines in tooltips.")

;;;----------------------------------------------------------------------------

(defun csense-setup ()
  "Setup Code Sense for the current buffer."
  (interactive)
  (local-set-key (kbd "<f1>") 'csense-do-something-clever)
  (local-set-key (kbd "C-<f1>") 'csense-show-popup-help-for-symbol))



(defun csense-do-something-clever ()
  "Do something clever at point."
  (interactive)
  (let ((char-syntax-after (char-syntax (char-after)))
        (char-syntax-before (char-syntax (char-before))))
    (if (and (or (eq char-syntax-before ?w)
                 (eq char-syntax-before ?_))
             (or (eq char-syntax-after ?w)
                 (eq char-syntax-after ?_)))
        (let* ((info (funcall csense-information-function))
               (doc (plist-get info 'doc)))
          (if doc
              (csense-show-popup-help (csense-wrap-text doc))
            (pp info)))

      (pp (funcall csense-completion-function)))))


(defun csense-show-popup-help (message)
  "Show MESSAGE in popup at point."
  (let* ((old-propertize (symbol-function 'propertize))
         (x-max-tooltip-size '(120 . 40))
         (lines (split-string message "\n"))
         (tooltip-width (* (frame-char-width)
                           (apply 'max (mapcar 'length lines))))
         (tooltip-height (* (frame-char-height) (min (length lines)
                                                     (cdr x-max-tooltip-size))))
         (xy (csense-calculate-popup-position tooltip-height tooltip-width))
         (tooltip-frame-parameters (append `((left . ,(car xy))
                                             (top . ,(cdr xy)))
                                           tooltip-frame-parameters)))

    ;; move the mouse cursor from the way
    (set-mouse-position (selected-frame) 0 100)

    ;; the definition of `propertize' is substituted with a dummy
    ;; function temporarily, so that tooltip-show doesn't override the
    ;; properties of msg
    (fset 'propertize (lambda (string &rest properties)
                        string))
    (unwind-protect
        (tooltip-show message)
      (fset 'propertize old-propertize))))


(defun csense-calculate-popup-position (height width)
  "Calculate pixel position of popup at point with size HEIGHT
and WIDTH in characters."
  (let* ((point-pos (posn-at-point))
         (point-xy (posn-x-y point-pos))
         (x (let ((x (+ (car point-xy) (frame-parameter nil 'left))))
              (if (> (+ x width) (x-display-pixel-width))
                  (- (x-display-pixel-width) width 10)
                x)))
         (y (let* ((point-y (+ (cdr point-xy) (frame-parameter nil 'top)))
                   (y (- point-y height)))
              (if (< y 0)
                  (+ point-y (* 4 (frame-char-height)))
                y))))
    (cons x y)))



(defun csense-wrap-text (text)
  "Wrap text if some of its lines are longer than
`csense-max-tooltip-line-length'."
  (let ((count 0)
        (pos 0)
        prevspace)
    (while (< pos (length text))
      (let ((char (aref text pos)))
        (cond ((= char ?\n)
               (setq count 0))
              ((= char ? )
               (if (< count csense-max-tooltip-line-length)
                   (progn (setq prevspace pos)
                          (incf count))

                 ;; insert newline
                 (if prevspace
                     (progn (aset text prevspace ?\n)
                            (setq count (- pos prevspace)))
                   (aset text pos ?\n)
                   (setq count 0))

                 (setq prevspace nil)))
              (t
               (incf count)))
        (incf pos))))
  text)


(provide 'csense)
;;; csense.el ends here
