(require 'cc-mode)
(require 'cl)
(require 'tempo)

;;{{{ template
;; (deh-require 'template
;;   (template-initialize)
;;   (setq template-default-directories (list my-template-dir))
;;   (add-to-list 'template-default-expansion-alist
;;                '("ENDATE"
;;                  (let ((system-time-locale "C"))
;;                    (insert (format-time-string "%d %b %Y")))))
;;   (dolist (cmd '(ido-select-text ido-magic-forward-char
;;                                  ido-exit-minibuffer))
;;     (add-to-list 'template-find-file-commands cmd)))
;;}}}

;;{{{ template-simple
(deh-require 'template-simple
  (setq template-directory-list (list my-template-dir)))
;;}}}

;;{{{ tempo
(deh-require 'tempo-x
  (setq tempo-interactive t)
  (global-set-key " " 'tempo-x-space)
  )
;;}}}

;;;;;;;;;;;;;;;;;;;; define mode abbrev ;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{ perl
(defvar tempo-perl-tags nil)
(deh-section "abbv-perl"
  (tempo-define-template
   "perl-dbi"
    '("use DBIx::Simple;"
      n> "my @cp = ('dbi:mysql:" (p "Database: ") "', 'ywb', 'pe');"
      n> "my ($db, $sql, $result);"
      n> "$db = DBIx::Simple->connect(@cp)"
      n "|| die DBIx::Simple->error;" >)
    "usedbi"
    "Use DBIx::Simple"
    'tempo-perl-tags)

  (tempo-define-template
   "perl-funcdesc"
   '("#==========================================================" n
     "# " p n
     "# @params " p n
     "# @return " p n
     "#==========================================================")
   "funcdesc"
   "Insert perl function description"
   'tempo-perl-tags)

  (tempo-define-template
   "perl-from2"
   '("from_to(" (m "OCT") ", " (m "FROM") ", "  (m "TO") ")")
   "from2"
   "Insert encode function from Encode module"
   'tempo-perl-tags))
;;}}}
(deh-section "php-abbv"
  (defvar tempo-php-tags nil)
  (tempo-define-template "php-docb"
                         '("/**" n
                           "* " > p n
                           > "*/")
                         "docb"
                         "Insert DocBlock"
                         'tempo-php-tags)
  (tempo-define-template "php-funb"
                         '("/**" n
                            "* " > p n
                           > "* " n
                           > "* @param " n
                           > "* @return " n
                           > "*/")
                         "funb"
                         "Insert Function DocBlock"
                         'tempo-php-tags)
  (tempo-define-template "php-fileb"
                         '("/**" n
                           "* " > p n
                           > "* " n
                           > "* @copyright Copyright (c) 2009, Taobao. inc" n
                           > "* @package " n
                           > "* @author Ye Wenbin<buzhi@taobao.com>" n
                           > "*/")
                         "fileb"
                         "Insert File DocBlock"
                         'tempo-php-tags)
  (tempo-define-template "php-classb"
                         '("/**" n
                           "* " > p n
                           > "* " n
                           > "* @package " n
                           > "*/")
                         "classb"
                         "Insert Class DocBlock"
                         'tempo-php-tags)
  )

;;{{{  html
(defvar tempo-html-tags nil)
(tempo-define-template
 "doctype"
 '((pi ("doctype: " ("strict" "transitional" "frameset")) "transitional"  type t)
   (assoc-default (tempo-lookup-named 'type)
                  '(("strict" .
                     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
                    ("transitional" .
                     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
                    ("frameset" .
                     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"))))
 "doctypex"
 "expand doctype"
 'tempo-html-tags)
(tempo-define-template
 "charset"
 '("<meta http-equiv=\"Content-Type\" content=\"text/html; charset="
   (pi ("charset: " ("utf-8" "gb2312")) "utf-8") "\" />")
 "charsetx"
 "expand charset"
 'tempo-html-tags)
;;}}}

;;{{{  c-mode
(deh-section "abbv-c-mode"
  (defvar tempo-c-tags nil
    "Tempo tags using in c-mode.")
  (tempo-define-template
   "c-if"
   '("if ( " p " ) {"
     n> p
     n "}" >)
   "ifx"
   "Expand if"
   'tempo-c-tags)
  (tempo-define-template
   "c-for"
   '((snippet
      "for ( " (S i) "=0; " (S i) "<" (S max) "; " (S i) "++ ) {"
      n> p
      n "}" >))
   "forx"
   "Expand for"
   'tempo-c-tags)
  (tempo-define-template
   "c-main"
   '("int main (int argc, char *argv[])"
     n "{" >
     n> p
     n "return 0;" >
     n "}" >)
   "mainx"
   "expand main"
   'tempo-c-tags))
;;}}}

;;{{{ emacs lisp
(deh-section "abbv-elisp"
  (defvar tempo-elisp-tags nil)
  (tempo-define-template "defun"
                         '("defun " p " (" p ")"
                           n> "\"" p "\""
                           n> ")")
                         "defun"
                         "Insert a defun expression"
                         'tempo-elisp-tags)

  (tempo-define-template "defvar"
                         '("defvar " p
                           n> "\"" p "\")")
                         "defvar"
                         "Insert a defvar expression"
                         'tempo-elisp-tags))
;;}}}

;;{{{ metapost
(deh-section "abbv-metapost"
  (tempo-define-template
   "meta-begfig"
   '("beginfig(" (read-from-minibuffer "fig id: "
                                       (if (save-excursion
                                             (re-search-backward "beginfig(\\([0-9]+\\))" nil t))
                                           (number-to-string
                                            (1+ (string-to-number (match-string 1))))
                                         "1")) ")"
                                         n> p
                                         n> "endfig;"
                                         n>)))
;;}}}

;; ;;{{{ scheme
;; (deh-section "abbv-scheme"
;;   (define-skeleton skeleton-scheme-mode-define
;;     "" nil
;;     "(define " _ ")")

;;   (define-skeleton skeleton-scheme-mode-lambda
;;     "" nil
;;     "(lambda (" (skeleton-read "Param: ") ") " _ ")"))
;; ;;}}}

;;{{{ msf-abbrev
;; (deh-require 'msf-abbrev
;;   (setq msf-abbrev-indent-after-expansion t)
;;   (setq msf-abbrev-root "~/.emacs.d/mode-abbrevs")
;;   (setq msf-abbrev-mode-alias
;;         '((c++-mode . c-mode)
;;           (cperl-mode . perl-mode)))
;;   (if (file-exists-p msf-abbrev-root)
;;       (dolist (hook '(c++-mode-hook c-mode-hook
;;                                     java-mode-hook
;;                                     cperl-mode-hook))
;;         (add-hook hook 'msf-abbrev-mode))))
;;}}}

;;{{{  other abbrev
(define-abbrev-table 'lisp-mode-abbrev-table
  '(
    ("itv" "interactive" nil 1)
    ))

(define-abbrev-table 'shell-mode-abbrev-table
  '(
    ("apti" "sudo aptitude install" nil 0)
    ("apts" "aptitude search" nil 1)
    ("wgetm" "wget -p -np -k -m" nil 0)
    ))
;;}}}
