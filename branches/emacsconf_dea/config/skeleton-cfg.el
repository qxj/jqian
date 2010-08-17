;; -*- coding: utf-8 mode: Emacs-Lisp -*-
;; skeleton-cfg.el --- skeleton mode
;; Time-stamp: <2010-03-10 19:26:06 Wednesday by julian>

;;; {{{ This code helps you to make skeleton using this method. You may modify freely.
(defvar make-skeleton-saved-winconf nil)
(defvar make-skeleton-header ";; help for skeleton
;; (find-w3m \"http://www.panix.com/~tehom/my-code/skel-recipe.txt\")
;; (describe-function 'skeleton-insert)
;; These lines are ignored.
"
  "Help string for skeleton.")

(defun make-skeleton ()
  "Create skeleton of skeleton.
It is based on `A recipe for using skeleton.el'.
http://www.panix.com/~tehom/my-code/skel-recipe.txt

C-c C-e: Erase the skeleton contents.
C-c C-c: Finish the input.
"
  (interactive)
  (setq make-skeleton-saved-winconf (current-window-configuration))
  (switch-to-buffer "*make skeleton*")
  (make-skeleton-mode)
  (if (zerop (buffer-size))
      (make-skeleton-erase-buffer)))

(defun make-skeleton-finish ()
  (interactive)
  (set-window-configuration make-skeleton-saved-winconf)
  (insert "(define-skeleton ")
  (save-excursion
    (insert "-skeleton-\n"
            "\"Insert \" nil\n")
    (let ((lines (with-current-buffer "*make skeleton*"
                   ;; skip header
                   (goto-char (point-min))
                   (re-search-forward "^[^;]")
                   (beginning-of-line)
                   (split-string (buffer-substring (point) (point-max)) "\n"))))
      (dolist (line lines nil)
        (back-to-indentation)
        (insert (format "%S > \\n\n" line))))
    (insert ")\n")))

(defun make-skeleton-erase-buffer ()
  "Erase the skeleton contents."
  (interactive)
  (erase-buffer)
  (insert make-skeleton-header))


(define-derived-mode make-skeleton-mode fundamental-mode "skeleton"
  "Major mode for creating a skeleton of skeleton."
  (define-key make-skeleton-mode-map "\C-c\C-c" 'make-skeleton-finish)
  (define-key make-skeleton-mode-map "\C-c\C-e" 'make-skeleton-erase-buffer))
;;; }}}

;;; perl
(defun my-perl-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
                               (?` ?` _ "''")
                               (?\(  _ ")")
                               (?\[  _ "]")
                               (?\"  _ "\"")))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
(add-hook 'cperl-mode-hook 'my-perl-mode-auto-pair)
(add-hook 'python-mode-hook 'my-perl-mode-auto-pair)

;;; c, c++, objc, javascript
(defun my-c-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
                               (?` ?` _ "''")
                               (?\(  _ ")")
                               (?\[  _ "]")
                               (?\"  _ "\"")
                               (?{ \n  _ \n ?} >)))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
(add-hook 'c-mode-hook 'my-c-mode-auto-pair)
(add-hook 'c++-mode-hook 'my-c-mode-auto-pair)
(add-hook 'objc-mode-hook 'my-c-mode-auto-pair)
(add-hook 'js2-mode-hook 'my-c-mode-auto-pair)
(add-hook 'java-mode-hook 'my-c-mode-auto-pair)

;;; c-mode auto complete
(define-skeleton skeleton-c-mode-for
  "generate for () { automatic" nil
  > "for (" _ ") {\n"
  > "\n"
  > "}" >)
(define-abbrev-table 'c-mode-abbrev-table '(("for" "" skeleton-c-mode-for 1)))
(define-skeleton skeleton-c-mode-while
  "generate whil () { automatic" nil
  > "while ( " _ " ) {\n"
  > "\n"
  > "}" >)
(define-abbrev-table 'c-mode-abbrev-table '(("whil" "" skeleton-c-mode-while 1)))
(define-skeleton skeleton-c-mode-if
  "generate if () { automatic" nil
  > "if( " _ " ) {\n"
  > "\n"
  > "}" >)
(define-abbrev-table 'c-mode-abbrev-table '(("if" "" skeleton-c-mode-if 1)))


;;; elisp, perl
(defun my-perl-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
                               (?` ?` _ "''")
                               (?\(  _ ")")
                               (?\[  _ "]")
                               (?\"  _ "\"")))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "[") 'skeleton-pair-insert-maybe))
(add-hook 'perl-mode-hook 'my-perl-mode-auto-pair)
(add-hook 'emacs-lisp-mode-hook 'my-perl-mode-auto-pair)

;;; html, css, sgml
(defun my-html-mode-auto-pair ()
  (interactive)
  (make-local-variable 'skeleton-pair-alist)
  (setq skeleton-pair-alist  '(
                               (?` ?` _ "''")
                               (?\( _ ")")
                               (?\< _ ">")
                               (?{ \n _ \n ?} >)
                               (?\" _ "\"")))
  (setq skeleton-pair t)
  (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
  (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe))
(add-hook 'html-mode-hook 'my-html-mode-auto-pair)
(add-hook 'css-mode-hook 'my-html-mode-auto-pair)
;;; (add-hook 'sgml-mode-hook 'my-html-mode-auto-pair)


(define-skeleton my-insert-c-comment-header
  "Inserts a c comment in a rectangle into current buffer."
  ""
  '(setq str (skeleton-read "Comment: "))
  ;; `str' is set explicitly here, because otherwise the skeleton
  ;; program would set it, only when it is going to insert it into the
  ;; buffer. But we need to determine the length of the string
  ;; beforehand, with `(length str)' below.
  '(when (string= str "") (setq str " - "))
  '(setq v1 (make-string (- fill-column 6) ?*))
  '(setq v2 (- fill-column 10 (length str)))
  "/* " v1 " */" \n
  "/* **" 
  (make-string (floor v2 2) ?\ )
  str
  (make-string (ceiling v2 2) ?\ )
  "** */" \n
  "/* " v1 " */")

;;; (define-skeleton my-xhtml-trans-skeleton
;;;   "Inserts a skeletal XHTML file with the DOCTYPE declaration
;;;     for the XHTML 1.0 Transitional DTD"
;;;   nil
;;;   "<?xml version=\"1.0\""
;;;   (if buffer-file-coding-system
;;; 	  (concat " encoding=\""
;;; 			  (setq v1 
;;; 					(symbol-name 
;;; 					 (coding-system-get buffer-file-coding-system 
;;;                                         'mime-charset))) "\""))
;;;   "?>\n"
;;;   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
;;;   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
;;;   "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
;;;   "<head>\n"
;;;   (if buffer-file-coding-system
;;; 	  (concat 
;;; 	   "<meta http-equiv=\"Content-type\" content=\"text/html; charset=" 
;;; 	   v1 "\" />\n"))
;;;   "<meta name=\"Author\" content=\"" 
;;;   (skeleton-read "Enter author: ") "\" />\n"
;;;   "<title>" (skeleton-read "Enter title: ") "</title>\n"
;;;   "</head>\n"
;;;   "<body>\n"
;;;   "\n"
;;;   "</body>\n"
;;;   "</html>"
;;;   '(indent-region (point-min) (point-max))
;;;   '(goto-char (point-max))
;;;   '(forward-line -2)
;;;   '(indent-according-to-mode))

