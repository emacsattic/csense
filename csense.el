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
        (pp (funcall csense-information-function))

      (pp (funcall csense-completion-function)))))


(provide 'csense)
;;; csense.el ends here
