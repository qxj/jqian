;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


;; disable autopair, looking forward electric-pair-mode in emacs24
(deh-require-reserved 'autopair
  ;; It's not an ideal way to turn on autopair-global-mode, because it's
  ;; unstable and its keybinds often works in unexcepted manner.
  (deh-add-hook (java-mode-hook
                  sh-mode-hook
                  c-mode-common-hook
                  python-mode-hook
                  emacs-lisp-mode-hook
                  html-mode-hook)
    (autopair-mode 1))
  ;; some tricks
  (deh-add-hook c++-mode-hook
    (push ? (getf autopair-dont-pair :comment))
    ;; (push '(?< . ?>) (getf autopair-extra-pairs :code))
    )
  (deh-add-hook emacs-lisp-mode-hook
    (push '(?` . ?') (getf autopair-extra-pairs :comment))
    (push '(?` . ?') (getf autopair-extra-pairs :string))) )

(deh-section "auto-complete"
  (require 'auto-complete-config)
  ;; specify a file stores data of candidate suggestion
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" my-data-dir))
  (setq ac-auto-start 3
        ac-auto-show-menu 1.5
        ;; ac-candidate-limit ac-menu-height ; improve drop menu performance
        ac-ignore-case nil
        ac-show-menu-immediately-on-auto-complete nil
        ;; ac-expand-on-auto-complete nil
        ;; ac-trigger-key nil
        ac-quick-help-delay 1.5
        ac-disable-faces nil
        ac-dwim t)

  ;; disable auto-complete in comments
  ;; (setq ac-disable-faces
  ;;       '(font-lock-string-face font-lock-doc-face))
  (setq ac-disable-faces '(font-lock-string-face))

  ;; for terminal, works well with `global-hl-line-mode'
  (if (null window-system)
      (set-face-background 'ac-completion-face "blue"))

  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" my-startup-dir))
  (add-to-list 'ac-user-dictionary-files
               (expand-file-name "ac.dict" my-startup-dir))

  ;;# enable auto-complete in some modes
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'LaTeX-mode)

  (ac-config-default)

  ;; donot use RET for auto complete, only TAB
  (deh-define-key ac-completing-map
    ((kbd "<return>")  nil)
    ((kbd "RET")       nil)
    ((kbd "TAB")       'ac-complete)
    ;; ((kbd "M-/") . 'ac-stop)
    )
  ;; when completion menu is displayed
  (setq ac-use-menu-map t)
  (deh-define-key ac-menu-map
    ("\C-n"  'ac-next)
    ("\C-p"  'ac-previous))

  (ac-set-trigger-key "TAB")

  ;; press <TAB> to active `auto-complete'
  ;; (deh-define-key ac-mode-map
  ;;   ((kbd "TAB")  'auto-complete-tab-action))
  (defun auto-complete-tab-action ()
    "If cursor at one word end, try auto complete it. Otherwise,
indent line."
    (interactive)
    (if (looking-at "\\>")
        (auto-complete)
      (indent-for-tab-command)))

  ;; c/c++
  ;; hack auto-complete.el (deperated)
  ;; add ac-prefix "->" to function `ac-prefix-c-dot'
  (defun ac-cc-mode-setup ()
    "customized setup for `c-mode-common-hook'"
    (dolist (command `(c-electric-backspace
                       c-electric-backspace-kill))
      (add-to-list 'ac-trigger-commands-on-completing command))
    (setq ac-sources (append '(ac-source-yasnippet
                               ;; ac-source-gtags
                               ac-source-semantic
                               ac-source-imenu) ac-sources))
    ;; firstly compile clang trunk: http://mike.struct.cn/blogs/entry/15/
    (when (executable-find "clang")
      (require 'auto-complete-clang)
      (add-to-list 'ac-sources 'ac-source-clang))
    )

  ;; python
  (defun ac-python-mode-setup ()
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
  (add-hook 'python-mode-hook 'ac-python-mode-setup)

  ;; Org
  (defun ac-org-mode-setup ()
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
  (add-hook 'org-mode-hook 'ac-org-mode-setup)

  ;; Slime
  (defun ac-slime-candidates ()
    "Complete candidates of the symbol at point."
    (if (memq major-mode '(lisp-mode))
        (let* ((end (point))
               (beg (slime-symbol-start-pos))
               (prefix (buffer-substring-no-properties beg end))
               (result (slime-simple-completions prefix)))
          (destructuring-bind (completions partial) result
            completions))))

  (ac-define-source slime
    '((candidates . ac-slime-candidates)
      (requires . 3)
      (symbol . "s")))

  (defun ac-slime-setup ()
    (slime-mode t)
    (push 'ac-source-slime ac-sources))

  (add-hook 'lisp-mode-hook 'ac-slime-setup)

  ;; for autopair
  (defun ac-settings-4-autopair ()
    "`auto-complete' settings for `autopair'."
    (defun ac-trigger-command-p (command)
      "Return non-nil if `this-command' is a trigger command."
      (or
       (and
        (symbolp command)
        (or (memq command ac-trigger-commands)
            (string-match "self-insert-command" (symbol-name command))
            (string-match "electric" (symbol-name command))
            (let* ((autopair-emulation-alist nil)
                   (key (this-single-command-keys))
                   (beyond-autopair (or (key-binding key)
                                        (key-binding (lookup-key local-function-key-map key)))))
              (or
               (memq beyond-autopair ac-trigger-commands)
               (and ac-completing
                    (memq beyond-autopair ac-trigger-commands-on-completing)))))))))
  (deh-after-load "autopair"
    (ac-settings-4-autopair)) )

(deh-section-if "completion"
  (add-to-list 'completion-at-point-functions 'semantic-completion-at-point-function)
  (setq completion-cycle-threshold 5)
  (add-to-list 'completion-styles 'substring)
  )

(deh-require 'yasnippet
  (setq yas-snippet-dirs my-snippet-dir)
  (yas-load-directory yas-snippet-dirs)
  (yas-global-mode 1)

  (setq yas-wrap-around-region t)

  (require 'dropdown-list)
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt))

  ;; FOR `hippie-try-expand' setting
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

  ;; FOR `auto-complete-mode', so disable default yasnippet expand action
  (if (fboundp 'auto-complete-mode)
      (progn
        (define-key yas-keymap (kbd "<right>") 'yas-next-field-or-maybe-expand)
        (define-key yas-keymap (kbd "<left>") 'yas-prev-field)))

  (deh-define-key yas-minor-mode-map
    ([(tab)]     nil)
    ((kbd "TAB") nil)                    ; Remove yas-expand keybind
    ((kbd "C-c TAB") 'yas-expand)
    ((kbd "C-c y") 'yas-insert-snippet)) ; List all snippets for current mode

  (defadvice yas-insert-snippet (around use-completing-prompt activate)
    "Use `yas/completing-prompt' for `yas/prompt-functions' but only here..."
    (let ((yas-prompt-functions '(yas-completing-prompt))) ad-do-it))

;;;###autoload
  (defun yasnippet-reload-after-save ()
    (let* ((bfn (expand-file-name (buffer-file-name)))
           (root (expand-file-name yas-snippet-dirs)))
      (when (string-match (concat "^" root) bfn)
        (yas-load-snippet-buffer)))) )

;;; abbrev
(deh-section "abbrev-table"
  ;; Digested from (Emacswiki)[http://www.emacswiki.org/emacs/AbbrevMode#toc7]
  (require 'cl)
  (defvar my-abbrev-tables nil)
  (defun my-abbrev-hook ()
    (let ((def (assoc (symbol-name last-abbrev) my-abbrev-tables)))
      (when def
        (execute-kbd-macro (cdr def)))
      t))
  (put 'my-abbrev-hook 'no-self-insert t)
  (defmacro declare-abbrevs (table abbrevs)
    (if (consp table)
        `(progn ,@(loop for tab in table
                        collect `(declare-abbrevs ,tab ,abbrevs)))
      `(progn
         ,@(loop for abbr in abbrevs
                 do (when (third abbr)
                      (push (cons (first abbr) (read-kbd-macro (third abbr)))
                            my-abbrev-tables))
                 collect `(define-abbrev ,table
                            ,(first abbr) ,(second abbr) ,(and (third abbr)
                                                               ''my-abbrev-hook))))))
  (put 'declare-abbrevs 'lisp-indent-function 2)

  (deh-after-load "sh-script"
    (declare-abbrevs sh-mode-abbrev-table
        (("redx" "\033[1;31m\033[0m" "C-u 4 C-b")
         ("greenx" "\033[1;32m\033[0m" "C-u 4 C-b")
         ("bluex" "\033[1;34m\033[0m" "C-u 4 C-b"))))

  ;; define global abbrev
  (define-abbrev-table 'global-abbrev-table
    '(("alpha" "α" nil 0)
      ("beta" "β" nil 0)
      ("gamma" "γ" nil 0)
      ("theta" "θ" nil 0)
      ("inf" "∞" nil 0)
      ("ar1" "→" nil 0)
      ("ar2" "⇒" nil 0)
      ("gt" "»" nil 0)
      ("lt" "«" nil 0)))
)

;;; skeleton
(deh-section "skeleton"
  ;; avoid abbre-mode when expand skeleton
  (setq skeleton-further-elements '((abbrev-mode nil))
        skeleton-end-hook nil)

  (defmacro define-skel-comment (name comment-start comment-end
                                      &optional char-to-fill)
    "Define a skeleton to insert one line comment as a
`fill-column' wide rectangle into current buffer.

For example: (define-skel-comment \"elisp\" \";;\" \";;\" ?\\;)
"
    (declare (debug t) (indent 2))
    (let ((char-to-fill (or char-to-fill ?*))
          (padding-length (+ (length comment-start) (length comment-end))))
      `(define-skeleton ,(intern (format "skel-%s-comment" name))
         ,(format "Insert a %s comment as a rectangle" name)
         ""
         '(setq str (skeleton-read "Comment: "))
         '(when (string= str "") (setq str " - "))
         '(setq v1 (make-string (- fill-column ,(+ padding-length 2))
                                ,char-to-fill))
         '(setq v2 (- fill-column ,(+ padding-length 6) (length str)))
         ,comment-start " " v1 " " ,comment-end \n
         ,comment-start " " ,(make-string 2 char-to-fill)
         (make-string (floor v2 2) ?\ )
         str
         (make-string (ceiling v2 2) ?\ )
         ,(make-string 2 char-to-fill) " " ,comment-end \n
         ,comment-start " " v1 " " ,comment-end)))

  (define-skel-comment "elisp" ";;" ";;" ?\;)
  (define-skel-comment "c" "/*" "*/")
  (define-skel-comment "c++" "//" "//" ?/)
  )

;;;; autopair
(deh-section "skeleton-pair"
  (setq skeleton-pair t
        skeleton-pair-on-word nil)

  ;;# `skeleton-pair-alist' will override `skeleton-pair-default-alist'
  (setq skeleton-pair-alist
        '((?( _ ?))
          (?[ _ ?])
          (?{ _ ?})
          ;; (?` _ ?')
          (?\" _ ?\")))

  (setq skeleton-pair-filter-function
        '(lambda ()
           (cond
            ((eq last-command-event ?\")
             (or (looking-at   (regexp-quote (string last-command-event)))
                 (looking-back (regexp-quote (string last-command-event)))
                 (looking-back "[[:graph:]]")))
            (t
             (looking-at (regexp-quote (string last-command-event)))))))

  (defmacro skeleton-autopair-define-key (keymap &optional alist)
    "Helper to define keymap for all autopair keys in customized
`skeleton-pair-alist'. While `skeleton-pair-alist' is a skeleton
definition, so this macro is only suitable for such skeleton
definition:

    '((KEY-BEFORE ... KEY-AFTER) ...)

for example:

    '((?( _ ?))
      (?{ \n _ \n ?})
      ...)

"
    (declare (debug t))
    (nconc (list 'progn)
           (mapcan (lambda (pair)
                     (list
                      `(define-key ,keymap ,(string (car pair)) 'skeleton-autopair-insert)
                      `(define-key ,keymap ,(string (car (last pair))) 'skeleton-autopair-insert)
                      ))
                   (or alist skeleton-pair-alist))))

  (skeleton-autopair-define-key global-map)

  (deh-add-hook sh-mode-hook
    (set (make-local-variable 'skeleton-pair-alist)
         '((?( _ ?))
           (?[ _ ?])
           (?{ _ ?})
           (?` _ ?`)
           (?\" _ ?\")))
    (skeleton-autopair-define-key sh-mode-map))

  (deh-add-hook c-mode-common-hook
    (set (make-local-variable 'skeleton-pair-alist)
         '((?( _ ?))
           (?[ _ ?])
           (?{ \n  _ \n ?} >)
           (?\' _ ?\')
           (?\" _ ?\")))
    (skeleton-autopair-define-key c-mode-base-map ((?( ?)) (?[ ?]) (?{ ?}) (?\') (?\"))))

  (defun skeleton-autopair-insert (arg)
    (interactive "P")
    (cond
     ((assq last-command-event skeleton-pair-alist)
      (skeleton-autopair-open arg))
     (t
      (skeleton-autopair-close arg))))

  (defun skeleton-autopair-open (arg)
    (interactive "P")
    (let ((pair (assq last-command-event skeleton-pair-alist)))
      (cond
       ((and (not mark-active)
             (eq (car pair) (car (last pair)))
             (eq (car pair) (char-after)))
        (skeleton-autopair-close arg))
       (t
        (skeleton-pair-insert-maybe arg)))))

  (defun skeleton-autopair-close (arg)
    (interactive "P")
    (cond
     (mark-active
      (let (pair open)
        (dolist (pair skeleton-pair-alist)
          (when (eq last-command-event (car (last pair)))
            (setq open (car pair))))
        (setq last-command-event open)
        (skeleton-pair-insert-maybe arg)))
     ((looking-at
       (concat "[ \t\n]*"
               (regexp-quote (string last-command-event))))
      (replace-match (string last-command-event))
      (delete-region (match-beginning 0) (match-end 0))
      (insert (string last-command-event))
      ;; (indent-according-to-mode)
      )
     (t
      (self-insert-command (prefix-numeric-value arg))
      ;; (indent-according-to-mode)
      )))

  (defadvice delete-backward-char (before autopair activate)
    "When you \\[delete-backward-char] a character, if both
charactors surrounding the current cursor belong to a pair, they
will be deleted together."
    (when (and (char-after)
               (eq this-command 'delete-backward-char)
               (eq (char-after)
                   (car (last (assq (char-before) skeleton-pair-alist)))))
      (delete-char 1))))

;;; tempo
;; skeleton and tempo can be replaced by yasnippet now
(deh-require-reserved 'tempo
  (setq tempo-interactive t)
  (tempo-define-template "lambda"
                         '(> "(lambda (" p ")" n> r> ")">)
                         nil            ; tag
                         "Insert a template for an anonymous procedure"
                         nil            ; taglist
                         )
  ;; combine with abbrev
  (define-abbrev lisp-mode-abbrev-table "lambda" "" 'tempo-template-lambda)
  (global-set-key (kbd "<C-tab>") 'tempo-complete-tag)
  (global-set-key "\C-c\C-f" 'tempo-forward-mark)
  )

;;; auto insert
(deh-section "autoinsert"
  (auto-insert-mode 1)
  (setq auto-insert-directory my-template-dir
        auto-insert-query 'function
        auto-insert 'other)

  (define-auto-insert '("\\.h$" . "C/C++ header")
    '((let* ((modes '("c" "c++"))
             (selected (ido-completing-read "C or C++ header? : " modes nil nil nil nil (car modes))))
        selected)
      "/* -*- mode: " str | -13 " -*-" ?\n
      (my-common-header " * ")
      " */" ?\n ?\n
      "#ifndef "
      (setq v1 (upcase (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name))
                               "_"
                               (file-name-extension buffer-file-name))))
      ?\n
      "#define " v1 "\n\n"
      _
      "\n\n#endif // " v1
      '(progn (set-auto-mode))))

  (define-auto-insert '("\\.\\(hh\\|hpp\\)$" . "C++ header")
    '(nil
      "// -*- mode: c++ -*-" ?\n
      (my-common-header "// ")
      "//" ?\n ?\n
      "#ifndef "
      (setq v1 (upcase (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name))
                               "_"
                               (file-name-extension buffer-file-name))))
      ?\n
      "#define " v1 "\n\n"
      _
      "\n\n#endif // " v1))

  (define-auto-insert '("\\.c$" . "C program")
    '(nil
      "/* -*- mode: c -*-" ?\n
      (my-common-header " * ")
      " */" ?\n ?\n
      "#include \""
      (let ((stem (file-name-sans-extension buffer-file-name)))
        (if (file-exists-p (concat stem ".h"))
            (file-name-nondirectory (concat stem ".h"))))
      & "\"\n" | -10
      _))

  (define-auto-insert '("\\.\\(cc\\|cpp\\)$" . "C++ program")
    '(nil
      "// -*- mode: c++ -*-" ?\n
      (my-common-header "// ")
      "//" ?\n ?\n
      "#include \""
      (let ((stem (file-name-sans-extension buffer-file-name)))
        (cond ((file-exists-p (concat stem ".h"))
               (file-name-nondirectory (concat stem ".h")))
              ((file-exists-p (concat stem ".hpp"))
               (file-name-nondirectory (concat stem ".hpp")))
              ((file-exists-p (concat stem ".hh"))
               (file-name-nondirectory (concat stem ".hh"))))
        )
      & "\"\n" | -10
      _))

  ;; (define-auto-insert '(makefile-mode . "Makefile")
  ;;   ["makefile.tpl"])

  (define-auto-insert '(makefile-mode . "Makefile")
    '(nil
      (my-common-header "# ")
      "\n" _))

  (define-auto-insert '(python-mode . "Python script")
    '(nil
      "#!/usr/bin/env python" ?\n
      "# -*- coding: utf-8; tab-width: 4; -*-" ?\n
      (my-common-header "# ")
      "#\n\n"
      "import sys" ?\n ?\n
      "def main():" ?\n
      > _ ?\n ?\n
      "if __name__ == \"__main__\":" ?\n
      > "main()"
      ))

  (define-auto-insert '(php-mode . "PHP script")
    '(nil
      "<?php" ?\n
      (my-common-header "// ")
      "//\n\n"
       _ ?\n ?\n
      "?>"
      ))

  (define-auto-insert '(sh-mode . "Shell script")
    '(nil
      "#!/bin/sh" ?\n
      (my-common-header "# ")
      "#\n\n"
       _
      ))

  (define-auto-insert '(org-mode . "Org document")
    '("Title: "
      "#+TITLE: " str & ?\n | -9
      "#+AUTHOR: " (progn user-full-name) ?\n
      "#+EMAIL: " (progn user-mail-address) ?\n
      "#+DATE: " (format-time-string "%Y-%m-%d") ?\n
      (let* ((modes '("org" "latex" "beamer"))
             (selected (ido-completing-read "Which kind of document? : " modes nil nil nil nil (car modes))))
        (if (string= selected "org")
            ""
          (concat "#+LATEX_HEADER: \\setmainfont{Big Caslon}\n"
                  "#+LATEX_HEADER: \\setsansfont{Optima}\n"
                  "#+LATEX_HEADER: \\setmonofont{American Typewriter}\n"
                  "#+LATEX_HEADER: \\setCJKmainfont{Kai}\n"
                  "#+LATEX_HEADER: \\setCJKsansfont{Hei}\n"
                  "#+LATEX_HEADER: \\setCJKmonofont{STFangsong}\n"
                  (if (string= selected "beamer")
                    (concat "#+LATEX_CLASS_OPTIONS: [presentation]\n"
                            "#+BEAMER_FRAME_LEVEL: "
                            "#+BEAMER_HEADER_EXTRA: \\usetheme{"
                            (let ((themes '("default"
                                            "Berkeley"
                                            "CambridgeUS"
                                            "Frankfurt"
                                            "PaloAlto"
                                            "Montpellier"
                                            "Pittsburgh"
                                            "Rochester"
                                            "boxes"
                                            "Goettingen")))
                              (ido-completing-read "Select a theme: " themes nil nil nil nil (car themes)))
                            "}"
                            "\\usecolortheme{"
                            (let ((colors '("default"
                                            "albatross"
                                            "beaver"
                                            "beetle"
                                            "crane"
                                            "dolphin"
                                            "dove"
                                            "fly"
                                            "lily"
                                            "orchid"
                                            "rose"
                                            "seagull"
                                            "seahorse"
                                            "sidebartab"
                                            "structure"
                                            "whale"
                                            "wolverine"
                                            "default")))
                              (ido-completing-read "Select a color: " colors nil nil nil nil (car colors)))
                            "}\n"
                            "#+COLUMNS: %35ITEM %10BEAMER_env(Env) %10BEAMER_envargs(Env Args) %4BEAMER_col(Col) %8BEAMER_extra(Extra)\n"
                            "#+OPTIONS: tags:nil\n"))) ) )
      ?\n _ ?\n ?\n
      "#+COMMENT: Local Variables:" ?\n
      "#+COMMENT: mode: org" ?\n
      "#+COMMENT: coding: utf-8" ?\n
      "#+COMMENT: fill-column: 78" ?\n
      "#+COMMENT: End:")
    )
  ;; helper functions
  (defun my-common-header (comment-string &optional encoding)
    (concat
     (mapconcat (lambda (line) (concat comment-string line))
                `(
                  ,(format "@(#) %s %s Time-stamp: <>"
                           (file-name-nondirectory (buffer-file-name))
                           (if encoding " -*- coding: utf-8 -*-" ""))
                  ,(format "Copyright %s %s"
                           (substring (current-time-string) -4)
                           (or (getenv "ORGANIZATION") user-full-name))
                  ,(format "Author: %s <%s>"
                           user-full-name
                           user-mail-address)
                  ,(format "Version: $Id: %s,v 0.1 %s %s Exp $"
                           (file-name-nondirectory (buffer-file-name))
                           (format-time-string "%Y-%m-%d %H:%M:%S")
                           (user-login-name))
                  )
                "\n")
     "\n"))

  ;;# copy from template-simple.el
  (add-hook 'write-file-functions 'my-update-header)
  (defun my-update-header ()
    (interactive)
    (when (and buffer-file-name
               (not (string-match (regexp-opt (list my-data-dir my-template-dir)) buffer-file-name)))
      (save-excursion
        (goto-char (point-min))
        (let ((end (progn (forward-line 3) (point))) ; check only first 3 lines
              (regexp "@(#)\\([^ \t\n]+\\)")
              (fn (file-name-sans-versions (file-name-nondirectory buffer-file-name))))
          (goto-char (point-min))
          (while (search-forward-regexp regexp end t)
            (and (not (string= (match-string 1) fn))
                 (y-or-n-p (format "Update file header %s to %s? "
                                   (match-string 1) fn))
                 (replace-match fn nil t nil 1)))))))
  )

;;; hippie
(deh-section "hippie-expand"
  ;; Recommand hippie-expand other than dabbrev-expand for `M-/'
  (deh-after-load "dabbrev" (defalias 'dabbrev-expand 'hippie-expand))
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-visible
          yas-hippie-try-expand
          try-expand-list
          try-expand-line
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          ;; try-expand-list-all-buffers
          ;; try-expand-line-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-complete-lisp-symbol
          try-complete-lisp-symbol-partially
          try-expand-whole-kill)))
