(deh-require 'xcscope
  (setq cscope-database-regexps
        '(("."
           (t)
           ("d:/Sources/postgresql-8.3.0/src" ("-d")))
          ("gedit"
           ( t )
           ("/home/ywb/softwares/source/gnome/gtk+-2.10.11/" ("-d"))
           ("/home/ywb/softwares/source/gnome/glib-2.12.11/" ("-d")))))
  (setq cscope-do-not-update-database t
        cscope-adjust nil))

(deh-require 'which-func
  (which-func-mode 1)
  (setq which-func-unknown "unknown"))

(deh-section "ebrowse"
  ;;  (add-to-list 'auto-mode-alist '("BROWSE\\.*" . ebrowse-tree-mode))
  (setq ebrowse-global-prefix-key "\C-z"))

(defconst my-c-style
  ;; Always indent c/c++ sources, never insert tabs
  '((c-basic-offset             . 4)
    ;; (c-tab-always-indent        . t)
    ;; Offset for line only comments
    (c-comment-only-line-offset . 0)
    ;; Controls the insertion of newlines before and after braces.
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    ;; Controls the insertion of newlines before and after certain colons.
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    ;; List of various C/C++/ObjC constructs to "clean up".
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    ;; Association list of syntactic element symbols and indentation offsets.
    (c-offsets-alist            . (
                                   ;; (topmost-intro . 0)
                                   (arglist-close . c-lineup-arglist)
                                   (substatement  . +)
                                   (substatement-open . 0)
                                   (case-label        . +)
                                   (block-open        . 0)
                                   (access-label      . -)
                                   (label             . -)
                                   (inclass           . +)
                                   (inline-open       . 0)
                                   ;; (c-echo-syntactic-information-p . t)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "My C/C++/ObjC Programming Style")

(deh-section "c-mode"
  (defun my-c-mode-common-hook ()
    (my-mode-common-hook)
    (c-add-style "Personal" my-c-style t)
    ;; (c-set-style "Personal")
    (setq c-basic-offset tab-width)
    (set (make-local-variable 'comment-style) 'indent)
    (local-set-key "*" 'self-insert-command)
    (c-toggle-auto-hungry-state 1)
    (c-toggle-hungry-state t)
    ;; (c-toggle-auto-newline t)
    (hs-minor-mode 1)
    (eldoc-mode 1)
    (setq comment-style 'extra-line)
    ;; (expand-add-abbrevs c-mode-abbrev-table expand-c-sample-expand-list)
    ;; (tempo-use-tag-list 'tempo-c-tags)
    )
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

(deh-section "c++-mode"
  (defun my-c++-mode-hook ()
    (my-c-mode-common-hook)
    (add-to-list 'c-style-alist
                 '("mine"
                   (c-basic-offset . 4)
                   (c-comment-only-line-offset . 0)
                   (c-hanging-braces-alist
                    (substatement-open after))
                   (c-offsets-alist
                    ;; (topmost-intro . 0)
                    (substatement . +)
                    (substatement-open . 0)
                    (case-label . +)
                    (access-label . -)
                    (inclass . +)
                    (inline-open . 0))))
    (c-set-style "mine"))
  ;; (c-add-style "Personal" my-c-style t)
  (add-hook 'c++-mode-hook 'my-c++-mode-hook)
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq local-abbrev-table c-mode-abbrev-table))))

(deh-section "gud"
  (add-hook 'gud-mode-hook
            (lambda ()
              (define-key gud-mode-map (kbd "<M-up>") 'comint-previous-prompt)
              (set (make-local-variable 'paragraph-separate) "\\'"))))


