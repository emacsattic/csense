;;; csense-cs.el --- Code sense backend for C#

;; Copyright (C) 2007  

;; Keywords: convenience

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


;;; Code:

(require 'rx)


(defconst csense-cs-symbol-regexp
  '(symbol-start 
    (group (syntax word)
           (* (or (syntax word)
                  (syntax symbol)))
           symbol-end))
  "Regular expression for matching a symbol.")

(eval-after-load "csense-cs"
    (defconst csense-cs-symbol-regexp-groups
      (csense-cs-get-regexp-group-num csense-cs-symbol-regexp)
      "Number of regexp groups."))

(defconst csense-cs-type-regexp
  '(symbol-start 
    (syntax word)
    (* (or (syntax word)
           (syntax symbol)))
    symbol-end
    (group 
     (?  "<" (+ (not (any ">"))) ">")))
  "Regular expression for matching a type.")

(eval-after-load "csense-cs"
    (defconst csense-cs-type-regexp-groups
      (csense-cs-get-regexp-group-num csense-cs-type-regexp)
      "Number of regexp groups."))


(defun csense-cs-get-completions-for-symbol-at-point ()
  "Return list of completions for symbol at point."
  (append 
   (csense-cs-get-local-variables)
   (csense-cs-get-member-variables)))


(defun csense-cs-get-local-variables ()
  "Return a list of variables for the current function."
  (let* ((func-info (csense-cs-get-function-info))
         (funbegin (plist-get func-info 'func-begin))
         result)
    (when funbegin
      (dolist (regexp (list 
                       ;; foreach
                       (eval `(rx  "foreach" (* space) 
                                   "(" (* space) ,@csense-cs-type-regexp
                                   (+ space)
                                   ,@csense-cs-symbol-regexp (+ space) "in"))
                       ;; local variable
                       (eval `(rx  ,@csense-cs-type-regexp
                                   (+ space) ,@csense-cs-symbol-regexp
                                   (* space) "="))))
        (save-excursion
          (while (re-search-backward regexp funbegin t)
            (push (match-string-no-properties 
                   (+ csense-cs-type-regexp-groups
                      csense-cs-symbol-regexp-groups)) result))))

      ;; function arguments
      (save-excursion
        (goto-char funbegin)
        (backward-sexp)
        (let ((regexp (eval `(rx ,@csense-cs-symbol-regexp 
                                 (or "," ")")))))
          (while (re-search-forward regexp funbegin t)
            (push (match-string-no-properties csense-cs-symbol-regexp-groups)
                  result)))))

    result))


(defun csense-cs-get-member-variables ()
  "Return a list of member variables for the current class."
  (let ((func-info (csense-cs-get-function-info)))
    (if func-info
        (save-excursion
          (goto-char (plist-get func-info 'parent-begin))
          (let ((sections (csense-cs-get-declaration-sections)))
            (mapcan (lambda (section)
                      (let (vars)
                        (goto-char (car section))
                        (while (re-search-forward
                                (eval `(rx  ,@csense-cs-type-regexp
                                            (+ space) ,@csense-cs-symbol-regexp
                                            (or (and (* space) "=")
                                                ";")))
                                (cdr section) t)
                          (push (match-string-no-properties 
                                 (+ csense-cs-type-regexp-groups
                                    csense-cs-symbol-regexp-groups))
                                vars))
                        vars))
                    sections))))))


(defun csense-cs-get-type-of-symbol-at-point ()  
  "Return the type of symbol at point or nil if no symbol is found."
  (let ((symbol (csense-cs-get-symbol-at-point)))
    (if symbol
        (or (save-excursion
              (skip-syntax-backward "w_")              
              (when (eq (char-before) ?\.)
                (backward-char)
                (csense-cs-get-type-of-symbol-at-point)))

            (csense-cs-lookup-unqualified-symbol symbol)))))


(defun csense-cs-lookup-unqualified-symbol (symbol)
  "Look up SYMBOL which is not qualified by an other symbol."
  (let* ((func-info (csense-cs-get-function-info))
         (funbegin (plist-get func-info 'func-begin)))
    (if (and symbol funbegin)
        (save-excursion
          (let ((symbol-regex '(symbol-start (eval symbol) symbol-end))
                (type-regex  '(symbol-start 
                               (group (syntax word)
                                      (* (or (syntax word)
                                             (syntax symbol))))
                               (?  "<" (+ (not (any ">"))) ">"))))
                                             
            (if (some (lambda (regex)
                        (re-search-backward regex funbegin t))
                      (list 
                       ;; foreach
                       (eval `(rx  "foreach" (* space) 
                                   "(" (* space) ,@type-regex (+ space)
                                   ,@symbol-regex (+ space) "in"))
                       ;; local variable
                       (eval `(rx  ,@type-regex
                                   (+ space) ,@symbol-regex (* space) "="))))
                (match-string-no-properties 1)

              ;; function param
              (let ((paramlist-begin (progn
                                       (goto-char funbegin)
                                       (backward-sexp)
                                       (point))))
                (goto-char funbegin)
                (if (re-search-backward
                     (eval `(rx  ,@type-regex
                                 (+ space) ,@symbol-regex (* space) (any ",)")))
                     paramlist-begin t)
                    (match-string-no-properties 1)

                  ;; member
                  (goto-char (plist-get func-info 'parent-begin))
                  (let ((sections (csense-cs-get-declaration-sections)))
                    (some (lambda (section)
                            (goto-char (car section))
                            (if (re-search-forward
                                 (eval `(rx  ,@type-regex
                                             (+ space) ,@symbol-regex 
                                             (or (and (* space) "=")
                                                 ";")))
                                 (cdr section) t)
                                (match-string-no-properties 1)))
                          sections))))))))))


(defun csense-cs-get-declaration-sections ()
  "Return list of buffer sections (BEGIN . END) which are outside
of functions, so they can contain member declarations.

Cursor must be at the beginning paren of structure which sections
are to be returned."
  (condition-case nil
      (let (sections
            (veryend (save-excursion
                       (forward-sexp)
                       (point)))
            (end 0))

        (save-excursion
          ;; step into structure
          (forward-char)

          (while (not (eq veryend end))
            (unless (eq end 0)
              (goto-char end)
              (forward-sexp))

            (let ((begin (point)))        
              (setq end 
                    (or (save-excursion
                          (let (pos)
                            (while (and (search-forward "{" veryend t)
                                        (if (save-excursion
                                              (beginning-of-line)
                                              ;; FIXME: doesn't handle
                                              ;; multiline comments
                                              (looking-at (rx (* space) "//")))
                                            t
                                          (setq pos (1- (point)))
                                          nil)))
                            pos))
                        veryend))

              (push (cons begin end) sections)))
                      
          (nreverse sections)))

    (scan-error)))


(defun csense-cs-get-symbol-at-point ()
  "Return symbol at point or nil."
  (save-excursion
    (let ((symbol (buffer-substring-no-properties
                   (save-excursion
                     (skip-syntax-backward "w_")
                     (point))
                   (save-excursion
                     (skip-syntax-forward "w_")
                     (point)))))
      (if (equal symbol "")
          nil
        symbol))))


(defun csense-cs-get-function-info ()
  "Return a plist of information about the current function or nil
if point is not in a function.

The plist contains `func-begin', the beginning position of the
function, and `parent-begin', the beginning position of the
parent."
  (save-excursion
    (let (result)
      (while (let ((open (save-excursion
                           (re-search-backward "{" nil t)))
                   (close (save-excursion
                            (re-search-backward "}" nil t))))
               (when open
                 (if (and close 
                          (> close open))
                     (progn 
                       (goto-char (1+ close))
                       (backward-sexp)
                       ;; search further
                       t)

                   (goto-char open)
                   (forward-line -1)
                   (if (looking-at "\\s-*\\(class\\|struct\\\)")
                       (progn
                         (setq result (plist-put result 'parent-begin open))
                         ;; stop search
                         nil)
                     (setq result (plist-put result 'func-begin open))
                     ;; search further for containing class
                     t)))))
      (if (plist-get result 'func-begin)
          result))))


(defun csense-cs-get-regexp-group-num (list)
  "Return the number of groups in rx regexp represented as LIST."
  (let ((num 0))
    (mapc (lambda (x)
            (if (listp x)
                (setq num (+ num (csense-cs-get-regexp-group-num x)))
              (if (eq x 'group)
                  (incf num))))
          list)
    num))

(provide 'csense-cs)
;;; csense-cs.el ends here
