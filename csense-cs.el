;;; csense-cs.el --- Opportunistic code sense backend for C#

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

;; If you want to use the Code Sense frontend then simply require the
;; C# frontend:
;;
;;    (require 'csense-cs-frontend)
;;
;;
;; The parser here takes a completely opportunistic approach. It
;; doesn't aim completeness or correctness, it simply does what it
;; needs to do by taking shortcuts and making outrageous assumptions.
;;
;; It doesn't do the proper thing in all the cases, it simply does a
;; good enough job in most of the cases.
;;
;; Below are the list of assummptions made. These simplify things
;; and although they could be implemented properly, it is defered
;; until I actually need them to work correctly.
;;
;;
;; Assumptions:
;;   
;;  - In the project each class has a unique name regardless of
;;    namespaces.
;;
;;  Tested on Emacs 22.


;;; Code:

(require 'csense)
(require 'csharp-mode)
(require 'rx)
(require 'cl)


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
            (syntax symbol))))
    symbol-end
    (group
     (?  (or (and "<" (+ (not (any ">"))) ">")
             (and "[" "]")))))
  "Regular expression for matching a type.")

(defconst csense-cs-type-regexp-type-group 1
  "Index of the grouping within the regexp which holds the
  type.")

(defconst csense-cs-type-regexp-extra-group 2
  "Index of the grouping within the regexp which holds extra
  information for the type. For example, if it's a generic or an
  array.")


(defconst csense-cs-typed-symbol-regexp
  (append csense-cs-type-regexp '((+ space)) csense-cs-symbol-regexp)
  "Regular expression for matching a type.")

(defconst csense-cs-type-aliases
  '(("bool"	. "Boolean")
    ("byte"	. "Byte")
    ("char"	. "Char")
    ("decimal"	. "Decimal")
    ("double"	. "Double")
    ("float"	. "Single")
    ("int"	. "Int32")
    ("long"	. "Int64")
    ("object"	. "Object")
    ("sbyte"	. "SByte")
    ("short"	. "Int16")
    ("string"	. "String")
    ("uint"	. "UInt32")
    ("ulong"	. "UInt64")
    ("ushort"	. "UInt16")
    ("void"	. "Void"))
  "C# type aliases and the corresponding .NET types.")


(defvar csense-cs-type-hash (make-hash-table :test 'equal)
  "Hash containing known type information.")


(defvar csense-cs-newline-whitespace-syntax-table
  (make-syntax-table csharp-mode-syntax-table))

(modify-syntax-entry ?\n " " csense-cs-newline-whitespace-syntax-table)


(add-hook 'csharp-mode-hook 'csense-setup)
(add-hook 'csharp-mode-hook 'csense-cs-setup)
  

(defun csense-cs-setup ()
  "Setup C# backend for the current buffer."
  (csense-cs-initialize))


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

        (dolist (type 
                 (condition-case nil
                     (read (current-buffer))
                   (error (message (concat "Couldn't parse the following "
                                           "line (position: %s): %s ")
                                   (- (point) (line-beginning-position))
                                   (buffer-substring (line-beginning-position)
                                                     (line-end-position)))
                          (error (concat "Couldn't read information from "
                                         "assembly. See the *Messages* buffer "
                                         "for details.")))))
          (puthash (plist-get type 'name) type csense-cs-type-hash))))

    (message "Done.")))


(defun csense-cs-get-information-at-point ()
  "Return available information at point."
  (let ((char-syntax-after (char-syntax (char-after)))
        (char-syntax-before (char-syntax (char-before))))
    (if (or (eq char-syntax-before ?w)
            (eq char-syntax-before ?_)
            (eq char-syntax-after ?w)
            (eq char-syntax-after ?_))
        (csense-cs-get-information-for-symbol-at-point))))


(defun csense-cs-get-completions-for-symbol-at-point ()
  "Return list of possible completions for symbol at point."
  (if (csense-cs-get-function-info)
      (save-excursion
        (if (csense-cs-backward-to-container)
            (csense-get-members-for-symbol
             ;; assuming only overloaded functions return more than
             ;; one value (true?), we'll take the first value
             ;; automatically
             (car (csense-cs-get-information-for-symbol-at-point)))
          (csense-cs-get-local-symbol-information-at-point)))

    (error "Completion works only within functions. For now.")))


(defun csense-cs-get-local-symbol-information-at-point ()
  "Return list of information about symbols locally available at point."
  (let ((func-info (csense-cs-get-function-info)))
    (if func-info
        (append (csense-cs-get-local-variables func-info)
                (save-excursion
                  (goto-char (plist-get func-info 'class-begin))
                  (csense-cs-get-members (plist-get func-info 'class-name)))))))


(defun csense-cs-get-local-variables (func-info)
  "Return a list of variables visible in the current scope within
the function."
  (let ((funbegin (plist-get func-info 'func-begin))
        (pos (point))
        result)
    (save-excursion
      (csense-cs-up-scopes 
       (lambda (type)
         (save-excursion
           (if (eq type 'sibling)
               ;; go to the end of the sibling scope to check for any
               ;; local variables bound after it
               ;;
               ;; since `csense-cs-up-scopes' already used
               ;; `backward-sexp' before we got here, this shouldn't
               ;; fail
               (forward-sexp)

             ;; if it's a parent scope and we're not at the beginning
             ;; of the function yet then check if it's a control
             ;; structure which binds some variable (there can be more
             ;; than one, one after the other)
             (unless (<= (point) funbegin)
               (save-excursion
                 (condition-case nil
                     (while (and 
                             (progn
                               (with-syntax-table 
                                   csense-cs-newline-whitespace-syntax-table
                                 (skip-syntax-backward " "))
                               (eq (char-before) ?\)))
                             (progn
                               (backward-sexp)
                               (looking-at 
                                (eval `(rx  "(" (* space) 
                                            ,@csense-cs-typed-symbol-regexp))))
                             (progn
                               (push (csense-cs-get-typed-symbol-regexp-result)
                                     result)
                               (backward-word)
                               t)))

                   (scan-error nil)))))

           (while (re-search-forward
                   (eval `(rx  ,@csense-cs-typed-symbol-regexp
                               (* space) (or "=" ";")))
                   pos t)
             (let ((var (csense-cs-get-typed-symbol-regexp-result)))
               ;; avoid matching return statements
               (unless (equal (plist-get var 'type) "return")
                 (push var result)))))
             
           (setq pos (point))
           (> (point) funbegin)))

      ;; function arguments
      (condition-case nil
          (progn
            (goto-char funbegin)
            (backward-sexp)
            (let ((regexp (eval `(rx ,@csense-cs-typed-symbol-regexp
                                     (or "," ")")))))
              (while (re-search-forward regexp funbegin t)
                (push (csense-cs-get-typed-symbol-regexp-result) result))))

        (scan-error nil)))

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
                        (let* ((symbol-info
                                 (csense-cs-get-typed-symbol-regexp-result))
                               (name (plist-get symbol-info 'name)))
                          ;; weed out constructors and Main function
                          (unless (or (equal name class)
                                      (equal name "Main"))
                            (push symbol-info members)))))
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


(defun csense-cs-get-information-for-symbol-at-point ()  
  "Get information about symbol at point or throw an error if symbol is unknown.

The return value is a list of plists."
  (save-excursion
    (let ((symbol (buffer-substring-no-properties
                   (progn (skip-syntax-backward "w_")
                          (point))
                   (save-excursion
                     (skip-syntax-forward "w_")
                     (point)))))

      (assert (not (equal symbol "")) nil
                   "Assertion failure: Symbol shouldn't be empty here")

      (if (csense-cs-backward-to-container)
          (let ((parent-info (csense-cs-get-information-for-symbol-at-point)))
            ;; we're interested only in the return type, so it's
            ;; enough to work with the first result
            (setq parent-info (car parent-info))
            
            (or (delete-if
                 'null
                 (mapcar (lambda (symbol-info)
                           (if (equal (plist-get symbol-info 'name) symbol)
                               symbol-info))
                         (csense-get-members-for-symbol parent-info)))

              (error "Don't know what '%s' is." symbol)))

        ;; wrap the result in a list for consistency, even if there is
        ;; only element. it simplifies handling the result in various
        ;; places
        (list 
         (or
          ;; handle this
          (if (equal symbol "this")
              (let ((function-info (csense-cs-get-function-info)))
                (if function-info
                    (csense-get-class-information 
                     (plist-get function-info 'class-name)))))

          ;; try it as a local symbol
          (some (lambda (symbol-info)
                  (if (equal (plist-get symbol-info 'name) symbol)
                      symbol-info))
                (csense-cs-get-local-symbol-information-at-point))

          ;; let's say it's a class
          (csense-get-class-information symbol)))))))


(defun csense-get-members-for-symbol (symbol-info)
  "Return list of members for symbol described by SYMBOL-INFO."
  (or 
   ;; it's a class itself
   (plist-get symbol-info 'members)

   ;; it's a variable or a method, so look up
   ;; class information first and then members
   (plist-get (csense-get-class-information
               (plist-get symbol-info 'type))
              'members)))  


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
                 (when (re-search-forward 
                        (eval `(rx "class" (+ space)
                                   symbol-start (group ,class) symbol-end))
                        nil t)
                   ;; position the cursor for csense-cs-get-members
                   ;; FIXME: it should be done some other way, it's clumsy
                   (save-match-data
                     (search-forward "{"))
                   (backward-char)
                   (setq result (list 'name class
                                      'file file
                                      'pos (match-beginning 1)
                                      'members (csense-cs-get-members class))))))

             (if kill
                 (kill-buffer buffer))

             result))
         csense-cs-source-files)

   (let ((class-info
          ;; maybe it's a fully qualified class name in an assembly
          (gethash class csense-cs-type-hash)))
     (unless class-info
       ;; try usings
       (save-excursion
         (goto-char (point-min))
         (while (and (not class-info)
                     (re-search-forward (rx line-start (* space) 
                                            "using" (+ space) 
                                            (group (+ nonl)) (* space) ";")
                                        nil t))
           (let ((class-name (concat (match-string-no-properties 1) 
                                     "." class)))
             ;; handle aliases
             (some (lambda (alias)
                     (if (equal class-name (concat "System." (car alias)))
                         (setq class-name (concat "System." (cdr alias)))))
                   csense-cs-type-aliases)

             (setq class-info (gethash class-name csense-cs-type-hash))))))

     (when class-info
       ;; copy tree is done, so that destructive operations on the result
       ;; do not affect the hash contents
       (setq class-info (copy-tree class-info))

       ;; a link to the parent class is put into every member
       (plist-put class-info
                  'members (mapcar (lambda (member)
                                     (plist-put member 'class class-info))
                                   (plist-get class-info 'members)))))

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
      (csense-cs-up-scopes
       (lambda (type)
         (if (eq type 'sibling)
             ;; we're not interested in sibling scopes
             t

           (if (and (plist-get result 'func-begin)
                    (save-excursion
                      (forward-line -1)
                      (looking-at (eval `(rx (* not-newline)
                                             "class" (+ space)
                                             ,@csense-cs-symbol-regexp)))))
               (progn
                 (setq result (plist-put result 'class-begin open))
                 (setq result (plist-put result 'class-name
                                         (csense-cs-get-match-result
                                          (list csense-cs-symbol-regexp))))
                 ;; class found, stop search
                 nil)

             (setq result (plist-put result 'func-begin (point)))
             ;; search further for containing class
             t))))

      (if (plist-get result 'class-name)
          result))))


(defun csense-cs-up-scopes (callback)
  "Go up scopes from point invoking CALLBACK every time the
beginning of a new scope is found.

CALLBACK is called with one argument which is the symbol `parent'
or `sibling' indicating the type of scope found,

The traversing of scopes continues if CALLBACK returns non-nil."
  (condition-case nil
      (save-excursion
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
                           (funcall callback 'sibling))

                       (goto-char open)
                       (funcall callback 'parent))

                   ;; no more parens
                   ;; terminate the search
                   nil))))

    (scan-error nil)))


(defun csense-cs-get-typed-symbol-regexp-result ()
  "Return the result of matching a `csense-cs-typed-symbol-regexp' as a plist."
  (list 'name (csense-cs-get-match-result 
               (list csense-cs-type-regexp
                     csense-cs-symbol-regexp))
        'file (buffer-file-name)
        'pos (match-beginning (csense-cs-get-regexp-group-num 
                               (list csense-cs-type-regexp
                                     csense-cs-symbol-regexp)))
        'type (if (equal (match-string-no-properties
                          csense-cs-type-regexp-extra-group)
                         "[]")
                  "System.Array"
                (match-string-no-properties csense-cs-type-regexp-type-group))))


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
