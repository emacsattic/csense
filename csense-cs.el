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

;;; Assumptions:
;;;   
;;;  - In the project each class has a unique name.
;;;


;;; Code:

(require 'csense)
(require 'rx)


;;; User configuration

(defvar csense-cs-source-files nil
  "List of source files of the project with full path.")


(defvar csense-cs-assemblies nil
  "List of external assembly files used by the project.

If the assembly has a corresponding XML file in the same
directory then it will be used as well.")


(defvar csense-cs-assembly-parser-program "netsense.exe"
  "Path to program used to load information from assemblies.")

;;;----------------------------------------------------------------------------

(defconst csense-cs-symbol-regexp
  '(symbol-start 
    (group (syntax word)
           (* (or (syntax word)
                  (syntax symbol)))
           symbol-end))
  "Regular expression for matching a symbol.")


(defconst csense-cs-type-regexp
  '(symbol-start 
    (group 
     (syntax word)
     (* (or (syntax word)
            (syntax symbol)))
     symbol-end
     (?  (or (and "<" (+ (not (any ">"))) ">")
             "[]"))))
  "Regular expression for matching a type.")


(defconst csense-cs-typed-symbol-regexp
  (append csense-cs-type-regexp '((+ space)) csense-cs-symbol-regexp)
  "Regular expression for matching a type.")


(defvar csense-cs-type-hash (make-hash-table :test 'equal)
  "Hash containing known type information.")


(defun csense-cs-setup-csense-frontend ()
  "Setup up CodeSense frontend for C#."
  (require 'csense)
  (add-hook 'csharp-mode-hook 'csense-cs-setup)
  (add-hook 'csharp-mode-hook 'csense-setup))
  

(defun csense-cs-setup ()
  "Setup CodeSense for the current C# buffer."
  (csense-cs-initialize)
  (setq csense-information-function 
        'csense-cs-get-information-for-symbol-at-point)
  (setq csense-completion-function 
        'csense-cs-get-completions-for-symbol-at-point))


(defun csense-cs-initialize ()
  "Initialize CSharp support."  
  (unless (> (hash-table-count csense-cs-type-hash) 0)
    (dolist (assembly csense-cs-assemblies)    
      (message "Loading information from assembly: %s" assembly)
      (with-temp-buffer
        (unless (= (call-process csense-cs-assembly-parser-program
                                 nil t nil assembly)
                   0)
          (error "Cannot load information from assembly: %s" assembly))

        (goto-char (point-min))

        (dolist (type (read (current-buffer)))
          (puthash (plist-get type 'name) type csense-cs-type-hash))))

    (message "Done.")))


(defun csense-cs-get-information-for-symbol-at-point ()
  "Return available information for symbol at point."
  (csense-cs-get-type-of-symbol-at-point))


(defun csense-cs-get-completions-for-symbol-at-point ()
  "Return list of possible completions for symbol at point."
  (save-excursion
    (skip-syntax-backward "w_")
    (if (csense-cs-backward-to-container)
        (plist-get (csense-cs-get-type-of-symbol-at-point) 'members)
      (csense-cs-get-local-symbol-information-at-point))))


(defun csense-cs-get-local-symbol-information-at-point ()
  "Return list of information about symbols locally available at point."
  (let ((func-info (csense-cs-get-function-info)))
    (if func-info
        (append (csense-cs-get-local-variables func-info)
                (save-excursion
                  (goto-char (plist-get func-info 'class-begin))
                  (csense-cs-get-members (plist-get func-info 'class-name)))))))


(defun csense-cs-get-local-variables (func-info)
  "Return a list of variables for the current function."
  (let ((funbegin (plist-get func-info 'func-begin))
        result)
    (dolist (regexp (list 
                     ;; foreach
                     (eval `(rx  "foreach" (* space) 
                                 "(" (* space) ,@csense-cs-typed-symbol-regexp
                                 (+ space) "in"))
                     ;; local variable
                     (eval `(rx  ,@csense-cs-typed-symbol-regexp
                                 (* space) (or "=" ";")))))
      (save-excursion
        (while (re-search-backward regexp funbegin t)
          (push (csense-cs-get-typed-symbol-regexp-result) result))))

    ;; function arguments
    (save-excursion
      (goto-char funbegin)
      (backward-sexp)
      (let ((regexp (eval `(rx ,@csense-cs-typed-symbol-regexp
                               (or "," ")")))))
        (while (re-search-forward regexp funbegin t)
          (push (csense-cs-get-typed-symbol-regexp-result) result))))

    result))


(defun csense-cs-get-members (class)
  "Return a list of members for the current class.
Cursor must be before the opening paren of the class.

CLASS is the name of the class."
  (save-excursion
    (let ((sections (csense-cs-get-declaration-sections)))
      (mapcan (lambda (section)
                (let ((section-begin (car section))
                      (section-end (cdr section))
                      members)
                  (goto-char section-begin)
                  ;; member variables
                  (while (re-search-forward
                          (eval `(rx  ,@csense-cs-typed-symbol-regexp
                                      (or (and (* space) "=")
                                          ";")))
                          section-end t)
                    (push (csense-cs-get-typed-symbol-regexp-result) members))

                  ;; check possible stuff at end of section

                  ;; property
                  (if (re-search-forward
                       (eval `(rx  ,@csense-cs-typed-symbol-regexp
                                   (* (or space ?\n)) "{"))
                       ;; the opening brace of the property is
                       ;; the section closing brace, so it must
                       ;; also be included in the match
                       (1+ section-end) t)
                      (push (csense-cs-get-typed-symbol-regexp-result) members)

                    ;; member function
                    (if (and (re-search-forward
                              (eval `(rx  ,@csense-cs-typed-symbol-regexp
                                          (* space) "("))
                              section-end t)
                                   
                             ;; closing paren followed by a
                             ;; an opening brace
                             (save-match-data
                             (save-excursion
                               (goto-char (1- (match-end 0)))
                               (forward-sexp)
                               (looking-at (rx (* (or space ?\n)) ?{)))))
                        (let ((symbol (csense-cs-get-match-result
                                               (list csense-cs-type-regexp
                                                     csense-cs-symbol-regexp))))
                          ;; weed out constructors and Main function
                          (unless (or (equal symbol class)
                                      (equal symbol "Main"))
                            (push (csense-cs-get-typed-symbol-regexp-result)
                                  members)))))
                  members))
              sections))))


(defun csense-cs-get-declaration-sections ()
  "Return list of buffer sections (BEGIN . END) of a class.

Cursor must be at the beginning paren of class which sections are
to be returned."
  (condition-case nil
      (let (sections
            (veryend (save-excursion
                       (forward-sexp)
                       (point)))
            (end 0))

        (save-excursion
          ;; step into structure
          (search-forward "{")

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


(defun csense-cs-get-type-of-symbol-at-point ()  
  "Return the type of symbol at point or nil if no symbol is found."
  (let ((end (save-excursion
               (skip-syntax-forward "w_")
               (point))))
    (save-excursion
      (unless (= (skip-syntax-backward "w_") 0)
        (let ((symbol (buffer-substring-no-properties (point) end)))
          (if (csense-cs-backward-to-container)
              (or (some (lambda (symbol-info)
                          (if (equal (plist-get symbol-info 'name) symbol)
                              (csense-get-class-information
                               (plist-get symbol-info 'type))))

                        (plist-get (csense-cs-get-type-of-symbol-at-point)
                                   'members))

                  (error "Don't know what '%s' is." symbol))

            (or 
             ;; try it as a local symbol
             (let ((class (some (lambda (symbol-info)
                                  (if (equal (plist-get symbol-info 'name) symbol)
                                      (plist-get symbol-info 'type)))
                                (csense-cs-get-local-symbol-information-at-point))))
               (if class
                   (csense-get-class-information class)))

             ;; try it as a class
             (csense-get-class-information symbol)

             (error "Don't know what '%s' is." symbol))))))))


(defun csense-cs-backward-to-container ()
  "If standing at a container reference then go bacward to the
container, and return t."
  (when (eq (char-before) ?\.)
    (backward-char)
    (skip-syntax-backward " ")
    (if (eq (char-before) ?\n)
        (backward-char))
    (if (eq (char-before) ?\))
        (backward-sexp))
    t))


(defun csense-get-class-information (class)
  "Look up and return information about CLASS. See Assumptions."
  (or 
   ;; try to search for it in the source files
   (some (lambda (file)
           (let* ((buffer (get-file-buffer file))
                  result kill)
             (unless buffer
               (setq buffer (find-file-noselect file))
               (setq kill t))

             (with-current-buffer buffer
               (save-excursion
                 (goto-char (point-min))
                 (if (re-search-forward (eval `(rx "class" (+ space)
                                                   (group ,class)))
                                        nil t)
                     (setq result (list 'name class
                                        'file file
                                        'pos (match-beginning 1)
                                        'members (csense-cs-get-members class))))))

             (if kill
                 (kill-buffer buffer))

             result))
         csense-cs-source-files)

   ;; maybe it's a fully qualified class name in an assembly
   (gethash class csense-cs-type-hash)

   ;; try usings
   (let (class-info)
     (save-excursion
       (goto-char (point-min))
       (while (and (not class-info)
                   (re-search-forward (rx line-start (* space) 
                                          "using" (+ space) 
                                          (group (+ nonl)) (* space) ";")
                                      nil t))
         (let ((class-name (concat (match-string-no-properties 1) "." class)))
           ;; handle string and object aliases
           (if (equal class-name "System.string")
               (setq class-name "System.String")
             (if (equal class-name "System.object")
                 (setq class-name "System.Object")))
           (setq class-info (gethash class-name csense-cs-type-hash)))))
     class-info)

   (error "Class '%s' not found. Are you perhaps missing an assembly?" class)))


(defun csense-cs-get-function-info ()
  "Return a plist of information about the current function or nil
if point is not in a function.

The plist values:

 `func-begin'

    The position of the beginning paren of the function.

  `class-begin'

    The position of the beginning paren of the class.

  `class-name'

    The name of the containing class.
"
  (save-excursion
    (let (result)
      (while (let ((open (save-excursion
                           (re-search-backward "{" nil t)))
                   (close (save-excursion
                            (re-search-backward "}" nil t))))
               (if open
                   (if (and close 
                            (> close open))
                       (progn 
                         (goto-char (1+ close))
                         (backward-sexp)
                         ;; search further
                         t)

                     (goto-char open)
                     (forward-line -1)
                     (if (looking-at (eval `(rx (* not-newline)
                                                "class" (+ space)
                                                ,@csense-cs-symbol-regexp)))
                         (progn
                           (setq result (plist-put result 'class-begin open))
                           (setq result 
                                 (plist-put result 'class-name
                                            (csense-cs-get-match-result
                                             (list csense-cs-symbol-regexp))))
                           nil)
                       (setq result (plist-put result 'func-begin open))
                       ;; search further for containing class
                       t))

                 ;; containing class not found
                 ;; terminate the search
                 (setq result nil))))
      (if (plist-get result 'func-begin)
          result))))


(defun csense-cs-get-typed-symbol-regexp-result ()
  "Return the result of matching a `csense-cs-typed-symbol-regexp' as a plist."
  (list 'name (csense-cs-get-match-result 
               (list csense-cs-type-regexp
                     csense-cs-symbol-regexp))
        'file (buffer-file-name)
        'pos (match-beginning (csense-cs-get-regexp-group-num 
                               (list csense-cs-type-regexp
                                     csense-cs-symbol-regexp)))
        'type (csense-cs-get-match-result 
               (list csense-cs-type-regexp))))


(defun csense-cs-get-match-result (regexps)
  "Return the last matching group by adding up the number of
matching groups in REGEXPS."
  (match-string-no-properties 
   (apply '+ (mapcar 'csense-cs-get-regexp-group-num regexps))))


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
