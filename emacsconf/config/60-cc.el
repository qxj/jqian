(deh-section "which-func"
  (deh-add-hooks (c-mode-common-hook emacs-lisp-mode-hook)
    (which-func-mode 1)
    (setq which-func-unknown "unknown")))

(deh-section "ebrowse"
  (add-to-list 'auto-mode-alist '("BROWSE\\.*" . ebrowse-tree-mode))
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
    ;; ;; Association list of syntactic element symbols and indentation offsets.
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
                                   (knr-argdecl-intro . -)))
    ;; (c-echo-syntactic-information-p . t)
    )
  "My C/C++/ObjC Programming Style")

(deh-section "c-mode"
  (defun my-c-mode-common-hook ()
    (my-mode-common-hook)
    (c-add-style "Personal" my-c-style t)
    ;; (c-set-style "Personal")
    (setq c-basic-offset tab-width)
    (local-set-key "*" 'self-insert-command)
    (c-toggle-auto-hungry-state 1)
    (c-toggle-hungry-state t)
    (c-toggle-auto-newline nil)
    (hs-minor-mode 1)
    (eldoc-mode 1)
    ;; (smart-operator-mode 1)
    (set (make-local-variable 'comment-style) 'extra-line)
    ;; (expand-add-abbrevs c-mode-abbrev-table expand-c-sample-expand-list)
    ;; keybinds
    (local-unset-key "\C-c\C-a")        ; trigger for `c-toggle-auto-newline'
    (local-unset-key "\C-c\C-o")        ; trigger for `c-set-offset'
    (local-unset-key "\C-d")            ; trigger for `c-electric-delete-forward'
    (local-set-key "\C-ca" 'sourcepair-load)
    ;; nbbuild
    (if (file-exists-p "Makefile.vs")
        (set (make-local-variable 'compile-command)
             "nbbuild --platform=linuxR_x86 --buildhost aleppo all"))
    (if (file-exists-p "Build")
        (set (make-local-variable 'compile-command)
             "/home/jqian/cloudstore/nbbuild/nbbuild.pl --plat linuxR_x86 --buildhost pinky all"))
    ;; untabify source code
    (make-local-hook 'write-contents-hooks)
    (add-hook 'write-contents-hooks 'my-untabify nil t)
    )
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

(deh-section "c++-mode"
  (defun my-c++-mode-hook ()
    (my-c-mode-common-hook)
    (setq local-abbrev-table c-mode-abbrev-table)
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

  ;; Unfortunately many standard c++ header files have no file
  ;; extension, and so will not typically be identified by emacs as c++
  ;; files. The following code is intended to solve this problem.
  (require 'cl)
  (defun file-in-directory-list-p (file dirlist)
    "Returns true if the file specified is contained within one of
the directories in the list. The directories must also exist."
    (let ((dirs (mapcar 'expand-file-name dirlist))
          (filedir (expand-file-name (file-name-directory file))))
      (and
       (file-directory-p filedir)
       (member-if (lambda (x) ; Check directory prefix matches
                    (string-match
                     (substring x 0 (min(length filedir) (length x))) filedir))
                  dirs))))
  (defun buffer-standard-include-p ()
    "Returns true if the current buffer is contained within one of
the directories in the INCLUDE environment variable."
    (and (getenv "INCLUDE")
         (file-in-directory-list-p
          buffer-file-name (split-string (getenv "INCLUDE") path-separator))))
  (add-to-list 'magic-fallback-mode-alist
               '(buffer-standard-include-p . c++-mode))
  )

(deh-section "gud"
  (add-hook 'gud-mode-hook
            (lambda ()
              (define-key gud-mode-map (kbd "<M-up>") 'comint-previous-prompt)
              (set (make-local-variable 'paragraph-separate) "\\'")
              ))

  (gud-tooltip-mode 1)
  (add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit))

(deh-section "compile"
  (setq compilation-finish-functions
        (lambda (buf str)
          (when (and (string= (buffer-name buf) "*compilation*")
                     (not (string-match "exited abnormally" str)))
            (run-at-time 0.5 nil 'delete-windows-on buf))))
  )

