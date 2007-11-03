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

;; Generic Code Sense frontend for assistance during coding.
;;
;; Install it by requiring one of the language-specific frontends.
;;

;;  Tested on Emacs 22.

;;; Code:

(require 'etags)

;;; User configuration

(defvar csense-max-tooltip-line-length 70
  "Maximum length of lines in tooltips.")


(defvar csense-multi-tooltip-bindings
  `((,(kbd "<down>") . csense-multi-tooltip-next)
    (,(kbd "<up>") . csense-multi-tooltip-previous))
  "Keybindings for controlling multi tooltips.")


(defface csense-tooltip-header-face
  '((t (:background "moccasin"))) 
  "Face for header lines in tooltips.")

(defface csense-multiple-tooltip-indicator-face
  '((t (:background "lawn green")))
  "Face for indicator field which shows the number of current
  tooltip in case of multiple results.")

(defvar csense-tooltip-current-line-color "honeydew2"
  "Color of the current line in tooltips for which information is shown.")


;;;----------------------------------------------------------------------------

(defvar csense-information-function nil
  "Function called with no arguments to get available information at point.

The function should return formatted textual documentation or nil
if there is no help available.")

(make-variable-buffer-local 'csense-information-function)


(defvar csense-completion-function nil
  "Function called with no arguments to get completions for the symbol at point.

The function should return a list of completions available at point. Each completion
must be a plist with the follwing values:")

(make-variable-buffer-local 'csense-completion-function)


(defun csense-setup ()
  "Setup Code Sense for the current buffer."
  (interactive)
  (local-set-key (kbd "<f1>") 'csense-show-help)
  (local-set-key (kbd "C-<f1>") 'csense-go-to-definition))



(defun csense-show-help ()
  "Do something clever at point."
  (interactive)
  (let* ((infos (funcall csense-information-function)))
    (if infos
        (let ((docs (mapcar (lambda (info)
                              (plist-get info 'doc))
                            infos)))                             
          (if (> (length docs) 1)
              (csense-show-multi-tooltip docs)
            (csense-show-tooltip-at-pos (car docs))))
         
      (message "No help available."))))


(defun csense-go-to-definition ()
  "Go to definition of symbol at point."
  (interactive)
  (let* ((info (car (funcall csense-information-function))))
    (if info
        (if (plist-get info 'file)
            (progn (ring-insert find-tag-marker-ring (point-marker))
                   (switch-to-buffer (find-file (plist-get info 'file)))
                   (goto-char (plist-get info 'pos)))

          (if (plist-get info 'url)
              (browse-url (plist-get info 'url))

            (assert nil nil "Assertion failure: No file or url found.")))

      (message "There is nothing at point."))))


(defun csense-show-tooltip-at-pos (message &optional x y)
  "Show MESSAGE in popup at X;Y or at point if coordinates are
not given."
  (let* ((old-propertize (symbol-function 'propertize))
         (x-max-tooltip-size '(120 . 40))
         (xy (if x
                 (cons x y)

               (let* ((dimensions (csense-get-text-dimensions message))
                      (tooltip-width (car dimensions))
                      (tooltip-height (cdr dimensions)))
                 (csense-calculate-popup-position tooltip-width
                                                  tooltip-height))))
         (tooltip-hide-delay 600)
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


(defun csense-get-text-dimensions (text)
  "Return text width and height in pixels."
  (let* ((lines (split-string text "\n"))
         (width (* (frame-char-width)
                   (apply 'max (mapcar 'length lines))))
         (height (* (frame-char-height) (length lines))))
    (cons width height)))


(defun csense-calculate-popup-position (width height)
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


(defun csense-get-code-context (file pos)
  "Return colored line context from FILE around POS."
  (let* ((buffer (get-file-buffer file))
         result kill)
    (unless buffer
      (setq buffer (find-file-noselect file))
      (setq kill t))

    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (setq result
              (concat (csense-truncate-path (buffer-file-name))
                      ":\n\n"
                      (csense-remove-leading-whitespace
                       (concat
                        (buffer-substring (save-excursion
                                            (forward-line -5)
                                            (point))
                                          (line-beginning-position))
                        (csense-color-string-background
                         (buffer-substring (line-beginning-position)
                                           (1+ (line-end-position)))
                         csense-tooltip-current-line-color)
                        (buffer-substring (1+ (line-end-position))
                                          (save-excursion
                                            (forward-line +5)
                                            (point)))))))))

    (if kill
        (kill-buffer buffer))

    result))


(defun csense-color-string-background (oldstr color)
  "Color OLDSTR with COLOR and return it."
  (let ((prevpos 0)
        pos 
        (str (copy-sequence oldstr))
        (continue t))
    (while continue
      (setq pos (next-single-property-change prevpos 'face str))
      (unless pos
        (setq pos (length str))
        (setq continue nil))
              
      (let ((face (get-text-property prevpos 'face str)))
        (put-text-property prevpos pos 'face
                           (list (cons 'background-color color)
                                 (cons 'foreground-color (if face
                                                             (face-foreground face))))
                           str))
      (setq prevpos pos))
    str))


(defun csense-truncate-path (path &optional length)
  "If PATH is too long truncate some components from the
beginning."
  (let ((maxlength (if length
                       length
                     70)))
    (if (<= (length path) maxlength)
        path

      (let* ((components (reverse (split-string path "/")))
             (tmppath (car components)))
        (setq components (cdr components))

        (while (and components
                    (< (length tmppath) maxlength))
          (setq path tmppath)
          (setq tmppath (concat (car components)
                                "/"
                                tmppath))
          (setq components (cdr components)))

        (concat ".../" path)))))


(defun csense-color-header (str)
  "Color first line of STR with `csense-tooltip-header-face'."
  (let ((pos (string-match "\n" str)))
    (if (not pos)
        str

      (concat (propertize (substring str 0 (1+ pos))
                          'face 'csense-tooltip-header-face)
              (substring str (1+ pos))))))


(defun csense-remove-leading-whitespace (str)
  "Remove leading identical whitespace from lines of STR."
  (let* ((lines (split-string str "\n"))
         char (count 0)) 
    (while (every (lambda (line)
                    (or (<= (length line) count)
                        (if char
                            (eq (aref line count) char)
                      
                          (setq char (aref line count))
                          (eq (char-syntax char) ?\ ))))
                  lines)
      (incf count)
      (setq char nil))

    (if (= count 0)
        str

      (let ((result (mapconcat (lambda (line)
                                 (if (>= count (length line))
                                     line
                                   (substring line count)))
                               lines "\n"))
            (oldpos -1)
            (newpos -1))
        ;; put back text properties to newlines
        (while (setq newpos (string-match "\n" result (1+ newpos)))
          (setq oldpos (string-match "\n" str (1+ oldpos)))
          (assert oldpos nil "Assertion failure: Old newline not found.")
          (put-text-property newpos (1+ newpos)
                             'face (get-text-property oldpos 'face str)
                             result))
        result))))


;; multi tooltip

(defvar csense-multi-tooltip-texts nil
  "List of texts shown in the current multi tooltip.")

(defvar csense-multi-tooltip-current nil
  "Index of current text shown in the tooltip. 0-based")

(defvar csense-multi-tooltip-saved-keys nil
  "Saved bindings for keys rebound by `csense-multi-tooltip-bindings'.")

(defvar csense-multi-tooltip-position nil
  "It's a list (X . Y) describing the position of the tooltip.")


(defun csense-show-multi-tooltip (texts)
  "Show several alternate texts in a tooltip. The current text
can be selected by the user."
  (let ((count 0))
    (setq csense-multi-tooltip-texts 
          (mapcar (lambda (text)
                    (concat (propertize
                             (concat " "
                                     (int-to-string (incf count))
                                     "/"
                                     (int-to-string (length texts))
                                     " ")
                             'face 'csense-multiple-tooltip-indicator-face)
                            "\n" text))
                  texts)))

  (let ((dimensions (mapcar (lambda (text)
                              (csense-get-text-dimensions text))
                            csense-multi-tooltip-texts)))
    (setq csense-multi-tooltip-position
          (csense-calculate-popup-position 
           (apply 'max (mapcar 'car dimensions))
           (apply 'max (mapcar 'cdr dimensions)))))

  (csense-multi-tooltip-show 0)

  (setq csense-multi-tooltip-saved-keys nil)
  (dolist (binding csense-multi-tooltip-bindings)
    (let ((key (car binding))
          (command (cdr binding)))
      (push (cons key (lookup-key (current-local-map) key))
            csense-multi-tooltip-saved-keys)
      (define-key (current-local-map) key command)))

  (add-hook 'pre-command-hook 'csense-multi-tooltip-pre-command))


(defun csense-multi-tooltip-pre-command ()
  "Pre-command hook for monitoring multi tooltips."
  (unless (or (eq this-command 'csense-multi-tooltip-next)
              (eq this-command 'csense-multi-tooltip-previous))
    (remove-hook 'pre-command-hook 'csense-multi-tooltip-pre-command)

    (dolist (binding csense-multi-tooltip-saved-keys)
      (define-key (current-local-map) (car binding) (cdr binding)))))


(defun csense-multi-tooltip-show (index)
  "Show the INDEXth tooltip from `csense-multi-tooltip-texts'."
  (setq csense-multi-tooltip-current index)
  (csense-show-tooltip-at-pos (nth index csense-multi-tooltip-texts)
                              (car csense-multi-tooltip-position)
                              (cdr csense-multi-tooltip-position)))


(defun csense-multi-tooltip-next ()
  "Show next tooltip."
  (interactive)
  (csense-multi-tooltip-show
   (let ((next (1+ csense-multi-tooltip-current)))    
     (if (= next (length csense-multi-tooltip-texts))
         0
       next))))


(defun csense-multi-tooltip-previous ()
  "Show previous tooltip."
  (interactive)
  (csense-multi-tooltip-show
   (1- (if (= csense-multi-tooltip-current 0)
           (length csense-multi-tooltip-texts)
         csense-multi-tooltip-current))))


(provide 'csense)
;;; csense.el ends here
