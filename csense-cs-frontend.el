;;; csense-cs-frontend.el --- C# support functions for Code Sense frontend

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

;; Require this package to use C# support with the Code Sense
;; frontend.
;;
;; See also the relevant configuration settings in csense.el and
;; csense-cs.el
;; 
;; Tested on Emacs 22.

;;; Code:

(require 'csense-cs)


(defconst csense-cs-frontend-msdn-url
  "http://msdn2.microsoft.com/en-us/library/%s.aspx"
  "URL to MSDN documentation.")


(add-hook 'csharp-mode-hook 'csense-cs-frontend-setup)
  

(defun csense-cs-frontend-setup ()
  "Setup CodeSense for the current C# buffer."
  (setq csense-information-function 
        'csense-cs-frontend-proxy)
  (setq csense-completion-function 
        'csense-cs-get-completions-for-symbol-at-point))


(defun csense-cs-frontend-proxy ()
  "Perform various modifications, before passing the retrieved
data for the CSense frontend."
  (mapcar (lambda (info)
            (setq info (csense-cs-frontend-doc-formatter info))

            ;; if it came from an assembly and it's in the
            ;; System. namespace then add the URL for documentation
            (unless (plist-get info 'file)
              (if (csense-cs-frontend-string-begins-with
                   (if (plist-get info 'members)
                       (plist-get info 'name)
                     (plist-get (plist-get info 'class) 'name))
                   "System.")
                  (setq info 
                        (plist-put info 'url
                                   (format csense-cs-frontend-msdn-url
                                           (if (plist-get info 'members)
                                               (plist-get info 'name)
                                             (concat
                                              (plist-get (plist-get info 'class)
                                                         'name)
                                              "."
                                              (plist-get info 'name))))))))

            info)
                                        
          (csense-cs-get-information-at-point)))


(defun csense-cs-frontend-doc-formatter (info)
  "Format documentation for the CSense frontend."
  (plist-put
   info 
   'doc
   (csense-wrap-text
    (csense-color-header 
     ;; if it was found in the sources then show the relevant part of
     ;; the source code
     (if (plist-get info 'file)
         (csense-get-code-context (plist-get info 'file)
                                  (plist-get info 'pos))

       ;; othewise format the retrieved documentation
       (let ((doc (plist-get info 'doc)))
         (setq doc
               (concat (if (plist-get info 'members)
                           (concat "class " 
                                   (plist-get info 'name))

                         ;; class member
                         (concat 
                          (plist-get info 'type)
                          " "
                          (plist-get info 'name)

                          (if (member 'params info)
                              (concat "("
                                      (mapconcat 
                                       (lambda (param)
                                         (concat (plist-get param 'type)
                                                 " "
                                                 (plist-get param 'name)))
                                       (plist-get info 'params)
                                       ", ")
                                      ")"))))
                       "\n\n"
                       (if doc
                           ;; remove generics
                           (replace-regexp-in-string
                            "`[0-9]+" ""
                            ;; remove references
                            (replace-regexp-in-string 
                             (rx "<see cref=\"" nonl ":" 
                                 (group (*? nonl)) "\"></see>")
                             "\\1"
                             doc))
                         "No documentation")))

         ;; replace aliased types with their shorter version
         (dolist (alias csense-cs-type-aliases)
           (setq doc (replace-regexp-in-string 
                      (concat "System." (cdr alias)) (car alias) doc t)))

         ;; remove namespace from classnames for readability
         ;; (brute force approach)
         (setq doc (replace-regexp-in-string
                    "\\([a-zA-z]+\\.\\)+\\([a-zA-Z]\\)" "\\2"
                    doc))

         doc))))))


(defun csense-cs-frontend-string-begins-with (str begin)
  "Return t if STR begins with the string BEGIN, or nil otherwise."
  (let ((begin-length (length begin)))
    (and (>= (length str)
             begin-length)
         (string= (substring str 0 begin-length)
                  begin))))


(provide 'csense-cs-frontend)
;;; csense-cs-frontend.el ends here
