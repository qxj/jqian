;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end


;; disable autopair, looking forward electric-pair-mode in emacs24
(deh-package autopair
  :disabled
  :config
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

(deh-package smartparens
  :disabled
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode 1)
  (setq smartparens-strict-mode t
        sp-autoescape-string-quote nil
        sp-autoinsert-if-followed-by-word t)
  (define-key sp-keymap (kbd "M-o") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "M-i") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-{") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "C-}") 'sp-select-next-thing)
  (define-key sp-keymap (kbd "C-\\") 'sp-select-previous-thing-exchange)
  (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
  ;; "fix"" highlight issue in scratch buffer
  (custom-set-faces '(sp-pair-overlay-face ((t ())))))

(deh-package yasnippet
  :config
  (setq yas-snippet-dirs my-snippet-dir)
  (yas-load-directory yas-snippet-dirs)
  ;; (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode-on) ; for emacs24+

  (setq yas-wrap-around-region t)
  (setq yas/indent-line nil)            ; stop auto-indent behavior when expanding snippets

  (require 'dropdown-list nil t)
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

  (bind-keys
   :map yas-minor-mode-map
   ("TAB" . nil)                    ; Remove yas-expand keybind
   ("<C-tab>" . yas-expand)
   ("C-c TAB" . yas-expand)
   ("C-c y" . yas-insert-snippet)) ; List all snippets for current mode

  (defadvice yas-insert-snippet (around use-completing-prompt activate)
    "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
    (let ((yas-prompt-functions '(yas-completing-prompt))) ad-do-it))

;;;###autoload
  (defun yasnippet-reload-after-save ()
    (let* ((bfn (expand-file-name (buffer-file-name)))
           (root (expand-file-name yas-snippet-dirs)))
      (when (string-match (concat "^" root) bfn)
        (yas-load-snippet-buffer)))) )

(deh-package auto-complete
  :config
  (require 'auto-complete-config)
  ;; specify a file stores data of candidate suggestion
  (setq ac-comphist-file (expand-file-name "ac-comphist.dat" my-data-dir))
  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" my-startup-dir))
  (add-to-list 'ac-user-dictionary-files
               (expand-file-name "ac.dict" my-startup-dir))
  (setq ac-auto-start 3
        ac-auto-show-menu 1.5
        ;; ac-candidate-limit ac-menu-height ; improve drop menu performance
        ac-ignore-case nil
        ac-show-menu-immediately-on-auto-complete nil
        ac-expand-on-auto-complete nil
        ;; ac-trigger-key nil
        ac-quick-help-height 40
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

  ;;# enable auto-complete in some modes
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'LaTeX-mode)

  (ac-config-default)

  (ac-set-trigger-key "TAB")

  ;; donot use RET for auto complete, only TAB
  (bind-keys
   :map ac-completing-map
   ("<return>" .  nil)
   ("RET"      .  nil)
   ("TAB"      .  ac-complete)
   ;; ((kbd "M-/")       'ac-stop)
   )
  ;; when completion menu is displayed
  (setq ac-use-menu-map t)
  (bind-keys
   :map ac-menu-map
   ("C-n"  . ac-next)
   ("C-p"  . ac-previous))

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

  ;; Exclude very large buffers from dabbrev
  (defun sanityinc/dabbrev-friend-buffer (other-buffer)
    (< (buffer-size other-buffer) (* 1 1024 1024)))
  (setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

  ;; c/c++
  (defmethod ede-include-path ((this ede-cpp-root-project))
    "Get the system include path used by ede-project."
    (oref this include-path))

  (defun ac-cc-mode-setup ()
    "customized setup for `c-mode-common-hook'"
    (dolist (command `(c-electric-backspace
                       c-electric-backspace-kill))
      (add-to-list 'ac-trigger-commands-on-completing command))
    (setq ac-sources (append '(ac-source-yasnippet
                               ;; ac-source-gtags
                               ac-source-semantic
                               ac-source-imenu) ac-sources))
    (cond
     ;; https://github.com/Golevka/emacs-clang-complete-async
     ((executable-find "clang-complete")
      (use-package auto-complete-clang-async
        :config
        (setq ac-clang-cflags (mapcar (lambda (dir) (format "-I%s" dir)) my-include-dirs))

        ;;- work with Project.ede (M-x ede-new)
        (deh-after-load "ede"
          (when (ede-current-project)
            (let* ((prj (ede-current-project))
                   (root (ede-project-root-directory prj))
                   ;; 1) parse the ede :spp-table.
                   ;; 2) parse the ede :include dirs
                   ;; 3) parse the ede :system-include dirs
                   ;; 4) add the ac-cc-mode-system-includes
                   (cxxflags (append
                              (mapcar (lambda (def)
                                        (let ((sym (car def))
                                              (val (cdr def)))
                                          (cond ((and (stringp val)
                                                      (not (zerop (length val))))
                                                 (format "-D%s=\"%s\"" sym val))
                                                ((numberp val)
                                                 (format "-D%s=%s" sym (number-to-string val)))
                                                (t
                                                 (concat "-D" sym)))))
                                      (ede-preprocessor-map prj))
                              (mapcar (lambda (dir) (format "-I%s%s" root (substring dir 1 nil)))
                                      (ede-include-path prj))
                              (mapcar (lambda (dir) (format "-I%s" dir))
                                      (ede-system-include-path prj)))))
              (setq ac-clang-cflags (append cxxflags ac-clang-cflags)))))

        ;; (local-set-key (kbd "C-<tab>") 'ac-complete-clang-async)
        (setq ac-sources '(ac-source-clang-async)) ;discard other sources
        (ac-clang-launch-completion-process)))

     ;; firstly compile clang trunk: http://mike.struct.cn/blogs/entry/15/
     ((executable-find "clang")
      (use-package auto-complete-clang
        :config
        (setq ac-clang-cflags (mapcar (lambda (dir) (format "-I%s" dir)) my-include-dirs))
        ;;
        ;; Customize ac-clang-cflags in .dir-locals.el, put it in the
        ;; root directory of projects.
        ;;
        ;; For more information see (info "(emacs) Directory Variables")
        ;;
        ;; One .dir-locals.el example:
        ;;
        ;; ((c++-mode
        ;;   (ac-clang-flags "-I/usr/include/x86_64-linux-gnu/qt5/QtCore"
        ;;                   "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus"
        ;;                   "-I.")
        ;;   (flycheck-clang-include-path "/usr/include/x86_64-linux-gnu/qt5/QtCore"
        ;;                                "/usr/include/x86_64-linux-gnu/qt5/QtDBus"
        ;;                                ".")))
        ;;
        (setq ac-sources '(ac-source-clang)))))

    (deh-package auto-complete-c-headers
      :config
      (add-to-list 'ac-sources 'ac-source-c-headers)
      (setq achead:include-directories (append achead:include-directories my-include-dirs))))
  (deh-add-hook c-mode-common-hook
    (when (derived-mode-p 'c-mode 'c++-mode)
      (ac-cc-mode-setup)))

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
  ;; (deh-after-load "autopair" (ac-settings-4-autopair))
  )

(deh-package completion
  :disabled
  :config
  (deh-after-load "semantic"
    (add-to-list 'completion-at-point-functions 'semantic-completion-at-point-function))
  (setq completion-cycle-threshold 5)
  (add-to-list 'completion-styles 'substring)
  (add-to-list 'completion-styles 'initials t)
  (add-to-list 'completion-at-point-functions
               (lambda ()
                 (unless (minibufferp) (auto-complete))))
  )

;;; abbrev
(deh-section abbrev-table
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
      ("delta" "δ" nil 0)
      ("epsilon" "ε" nil 0)
      ("zeta" "ζ" nil 0)
      ("nu" "ν" nil 0)
      ("xi" "ξ" nil 0)
      ("omicron" "ο" nil 0)
      ("pi" "π" nil 0)
      ("rho" "ρ" nil 0)
      ("sigma" "σ" nil 0)
      ("eta" "η" nil 0)
      ("theta" "θ" nil 0)
      ("iota" "ι" nil 0)
      ("kappa" "κ" nil 0)
      ("lambada" "λ" nil 0)            ; avoid conflict with lambda
      ("mu" "μ" nil 0)
      ("tau" "τ" nil 0)
      ("upsilon" "υ" nil 0)
      ("phi" "ϕ" nil 0)
      ("chi" "χ" nil 0)
      ("psi" "ψ" nil 0)
      ("omega" "ω" nil 0)
      ;; upper
      ("Delta" "Δ" nil 0)
      ("Pi" "Π" nil 0)
      ("Sigma" "Σ" nil 0)
      ("Theta" "Θ" nil 0)
      ("Omega" "Ω" nil 0)
      ;; arrow
      ("inf" "∞" nil 0)
      ("ar1" "→" nil 0)
      ("ar2" "⇒" nil 0)
      ("ra1" "←" nil 0)
      ("gt" "»" nil 0)
      ("lt" "«" nil 0)))
  )

;;; skeleton
(deh-section skeleton
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

;;; auto insert
(deh-package autoinsert
  :config
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
      (setq v1 (my-ifndef-header-guard-string))
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
      (setq v1 (my-ifndef-header-guard-string))
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

  (define-auto-insert '("\\.ede$" . "Project.ede")
    '("Project name: "
      "-*- mode: emacs-lisp -*-\n"
      "(ede-cpp-root-project \"" str "\"\n"
      > ":name \"" str "\"\n"
      > ":file \"" (file-name-directory buffer-file-name) "Makefile\"\n"
      > ":include-path '(\"" (read-string "project include path: ")"\"" _ ")\n"
      > ":system-include-path '(\"/usr/local/include\")\n"
      > ":spp-table '((\"DEBUG\" . \"\")\n"
      > "(\"SYMBOL\" . \"\")\n"
      > "(\"ZERO\" . 0)\n"
      > "(\"ONE\" . 1)\n"
      > "(\"PROJECT_NAME\" . \"" str "\")\n"
      > "(\"PROJECT_VERSION\" . \"1.0.0\")))"))

  ;; (define-auto-insert '(makefile-mode . "Makefile")
  ;;   ["makefile.tpl"])

  (define-auto-insert '(makefile-mode . "Makefile")
    '(nil
      (my-common-header "# ")
      "\n" _))
  (define-auto-insert '("make\\.inc$" . "make.inc")
    ["make.inc"])
  (define-auto-insert '("make\\.rules$" . "make.rules")
    ["make.rules"])

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

  (define-auto-insert '(sql-mode . "SQL script")
    '(nil
      "-- -*- coding: utf-8; tab-width: 2; -*-" ?\n
      (my-common-header "-- ")
      "--\n\n"
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
                           (if encoding (concat " -*- coding: " encoding " -*-") ""))
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
  (defun my-ifndef-header-guard-string ()
    "ifndef header guard for BLADE"
    (let ((blade-root (locate-dominating-file buffer-file-name "BLADE_ROOT")))
      (concat (upcase (replace-regexp-in-string
                       "[^a-zA-Z0-9]" "_"
                       (if blade-root
                           (substring buffer-file-name (length (expand-file-name blade-root)))
                         (file-name-nondirectory buffer-file-name))))
              "_")))
  )

;;; hippie
(deh-section hippie-exp
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

;;; unicode characters
(deh-package xmsi-mode
  :commands xmsi-mode
  ;; (xmsi-mode 1) ; activate the mode.
  )
