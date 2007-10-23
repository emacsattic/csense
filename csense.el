;;; csense.el --- Code Sense for Emacs

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

;; 

;;; Code:


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
    (if (or (eq char-syntax-before ?w)
            (eq char-syntax-before ?_))
        (if (or (eq char-syntax-after ?w)
                (eq char-syntax-after ?_))
            (message (csense-cs-get-information-for-symbol-at-point))
          (message "comp"))
      (message "comp"))))


(provide 'csense)
;;; csense.el ends here
