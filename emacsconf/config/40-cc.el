;; -*- mode: Emacs-Lisp -*-

;; c/c++ programming and debug settings

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

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
  (deh-try-require 'google-c-style)
  (deh-try-require 'zjl-c-hl)

  ;;# if function name is too long, we will indent the parameters forward.
  (defconst my-c-lineup-maximum-indent 20)
  (defun my-c-lineup-arglist (langelem)
    (let ((ret (c-lineup-arglist langelem)))
      (if (< (elt ret 0) my-c-lineup-maximum-indent)
          ret
        (save-excursion
          (goto-char (cdr langelem))
          (vector (+ (current-column) 8))))))
  (defun my-c-indent-lineup-arglist ()
    (setcdr (assoc 'arglist-cont-nonempty c-offsets-alist)
            '(c-lineup-gcc-asm-reg my-c-lineup-arglist)))

  ;;# convert some .h to c++-mode automatically
  (defun my-c-correct-hpp-mode ()
    (if (and (not (derived-mode-p 'c++-mode))
             (string-match "\.h$" (buffer-name))
             (save-excursion
               (goto-char (point-min))
               (search-forward-regexp "^class" nil t)))
        (c++-mode)))
        
  ;;# change face of code in #if 0...#endif
  ;; http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-fa
  (defun my-c-mode-font-lock-if0 (limit)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let ((depth 0) str start start-depth)
          (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
            (setq str (match-string 1))
            (if (string= str "if")
                (progn
                  (setq depth (1+ depth))
                  (when (and (null start) (looking-at "\\s-+0"))
                    (setq start (match-end 0)
                          start-depth depth)))
              (when (and start (= depth start-depth))
                (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
                (setq start nil))
              (when (string= str "endif")
                (setq depth (1- depth)))))
          (when (and start (> depth 0))
            (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
    nil)
  (defun my-c-mode-common-hook-if0 ()
    (font-lock-add-keywords
     nil
     '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

  (defun my-c-mode-common-hook ()
    (my-mode-common-hook)
    ;; (c-add-style "Personal" my-c-style t)
    ;; (c-set-style "stroustrup")
    ;; (call-interactively 'google-set-c-style)
    (my-c-indent-lineup-arglist)
    (my-c-correct-hpp-mode)
    (my-c-mode-common-hook-if0)
    (c-toggle-auto-hungry-state 1)
    (c-toggle-hungry-state t)
    (c-toggle-auto-newline nil)
    (eldoc-mode 1)
    (hide-ifdef-mode 1)
    (subword-mode 1)
    ;; (cwarn-mode 1)
    ;; (smart-operator-mode 1)
    (set (make-local-variable 'comment-style) 'extra-line)
    ;; (expand-add-abbrevs c-mode-abbrev-table expand-c-sample-expand-list)
    ;; keybinds
    ;; (local-unset-key "\C-c\C-a")        ; trigger for `c-toggle-auto-newline'
    (local-unset-key "\C-d")            ; trigger for `c-electric-delete-forward'
    (local-set-key "\C-ca" 'sourcepair-load)
    ;;# keybinds remind
    ;; C-M-a 'c-beginning-of-defun
    ;; C-M-e 'c-end-of-defun
    ;; M-a   'c-beginning-of-statement
    ;; M-e   'c-end-of-statement
    ;; nbbuild
    (if (file-exists-p "Makefile.vs")
        (set (make-local-variable 'compile-command)
             "nbbuild --platform=linuxR_x86 --buildhost aleppo all"))
    (if (file-exists-p "Build")
        (set (make-local-variable 'compile-command)
             "/home/jqian/cloudstore/nbbuild/nbbuild.pl --plat linuxR_x86 --buildhost pinky all"))
    )
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

(deh-section "c++-mode"
  (defun my-c++-mode-hook ()
    (my-c-mode-common-hook)
    (setq local-abbrev-table c-mode-abbrev-table)
    )
  (add-hook 'c++-mode-hook 'my-c++-mode-hook)

  ;; Unfortunately many standard c++ header files have no file
  ;; extension, and so will not typically be identified by emacs as c++
  ;; files. The following code is intended to solve this problem.
  (eval-when-compile (require 'cl))
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

(deh-require-if 'cflow-mode
  (executable-find "cflow")

  (defun my-cflow-function (function-name)
    "Get call graph of inputed function. "
    ;; (interactive "sFunction name:\n")
    (interactive (list (car (senator-jump-interactive "Function name: "
                                                      nil nil nil))))
    (let* ((cmd (format "cflow -b --main=%s %s" function-name buffer-file-name))
           (cflow-buf-name (format "**cflow-%s:%s**"
                                   (file-name-nondirectory buffer-file-name)
                                   function-name))
           (cflow-buf (get-buffer-create cflow-buf-name)))
      (set-buffer cflow-buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (shell-command-to-string cmd))
      (pop-to-buffer cflow-buf)
      (goto-char (point-min))
      (cflow-mode)))
  )

(deh-section "gud"
  (setq gdb-many-windows nil            ; no need many windows
        gdb-use-separate-io-buffer t)

  (deh-add-hook gud-mode-hook
    (define-key gud-mode-map (kbd "<M-up>") 'comint-previous-prompt)
    (define-key gud-mode-map (kbd "C-u") 'comint-kill-input)
    ;;# keybinds remind
    ;; M-r 'comint-history-isearch-backward-regexp
    (set (make-local-variable 'paragraph-separate) "\\'"))

  (defun my-toggle-gdb ()
    "Switch to a gdb buffer or return to the previous buffer."
    (interactive)
    (if (derived-mode-p 'gud-mode)
        (while (derived-mode-p 'gud-mode)
          (bury-buffer))
      (let ((list (buffer-list)))
        (while list
          (if (with-current-buffer (car list)
                (derived-mode-p 'gud-mode))
              (progn
                (call-interactively 'gdb-restore-windows)
                (setq list nil))
            (setq list (cdr list))))
        (unless (derived-mode-p 'gud-mode)
          (call-interactively 'gdb)))))

  (eval-after-load "gud"
    '(progn
       (deh-define-key gud-minor-mode-map
         ((kbd "<M-up>") . 'comint-previous-prompt)
         ([f5]           . 'gud-go)
         ([S-f5]         . 'gud-kill)
         ([f8]           . 'gud-print)
         ([C-f8]         . 'gud-pstar)
         ([f9]           . 'gud-break-or-remove)
         ([C-f9]         . 'gud-enable-or-disable)
         ([S-f9]         . 'gud-watch)
         ([f10]          . 'gud-next)
         ([C-f10]        . 'gud-until)
         ([C-S-f10]      . 'gud-jump)
         ([f11]          . 'gud-step)
         ([C-f11]        . 'gud-finish))))

  (gud-tooltip-mode 1)

  (defun gud-break-or-remove (&optional force-remove)
    "Set/clear breakpoin."
    (interactive "P")
    (save-excursion
      (if (or force-remove
              (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint))
          (gud-remove nil)
        (gud-break nil))))

  (defun gud-enable-or-disable ()
    "Enable/disable breakpoint."
    (interactive)
    (let ((obj))
      (save-excursion
        (move-beginning-of-line nil)
        (dolist (overlay (overlays-in (point) (point)))
          (when (overlay-get overlay 'put-break)
            (setq obj (overlay-get overlay 'before-string))))
        (if (and obj (stringp obj))
            (cond ((featurep 'gdb-ui)
                   (let* ((bptno (get-text-property 0 'gdb-bptno obj)))
                     (string-match "\\([0-9+]\\)*" bptno)
                     (gdb-enqueue-input
                      (list
                       (concat gdb-server-prefix
                               (if (get-text-property 0 'gdb-enabled obj)
                                   "disable "
                                 "enable ")
                               (match-string 1 bptno) "\n")
                       'ignore))))
                  ((featurep 'gdb-mi)
                   (gud-basic-call
                    (concat
                     (if (get-text-property 0 'gdb-enabled obj)
                         "-break-disable "
                       "-break-enable ")
                     (get-text-property 0 'gdb-bptno obj))))
                  (t (error "No gud-ui or gui-mi?")))
          (message "May be there isn't have a breakpoint.")))))

  (defun gud-kill ()
    "Kill gdb process."
    (interactive)
    (with-current-buffer gud-comint-buffer (comint-skip-input))
    ;; (set-process-query-on-exit-flag (get-buffer-process gud-comint-buffer) nil)
    ;; (kill-buffer gud-comint-buffer))
    (dolist (buffer '(gdba gdb-stack-buffer gdb-breakpoints-buffer
                           gdb-threads-buffer gdb-inferior-io
                           gdb-registers-buffer gdb-memory-buffer
                           gdb-locals-buffer gdb-assembler-buffer))
      (when (gdb-get-buffer buffer)
        (let ((proc (get-buffer-process (gdb-get-buffer buffer))))
          (when proc (set-process-query-on-exit-flag proc nil)))
        (kill-buffer (gdb-get-buffer buffer)))))

  (defadvice gdb (before ecb-deactivate activate)
    "if ecb activated, deactivate it."
    (when (and (boundp 'ecb-minor-mode) ecb-minor-mode)
      (ecb-deactivate)))

  (add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit))

(deh-section "buffer-action"
  (autoload 'buffer-action-compile "buffer-action")
  (autoload 'buffer-action-run "buffer-action")

  ;;# hack to find Makefile upon direcories recursively.
  (eval-after-load "buffer-action"
    '(progn
       (defun buffer-action-throw-final-path (try-dir)
         (cond
          ;; tramp root-dir
          ((and (featurep 'tramp)
                (string-match tramp-file-name-regexp try-dir))
           (with-parsed-tramp-file-name try-dir foo
             foo-localname))
          (t try-dir)))

       (defun buffer-action-is-root-dir (try-dir)
         (or
          ;; windows root dir for a driver or Unix root
          (string-match "\\`\\([a-zA-Z]:\\)?/$" try-dir)
          ;; tramp root-dir
          (and (featurep 'tramp)
               (string-match (concat tramp-file-name-regexp ".*:/$") try-dir))))

       (defun buffer-action-find-make-dir (try-dir)
         "Return a directory contain makefile. try-dir is absolute
path."
         (if (buffer-action-is-root-dir try-dir)
             nil ;; return nil if failed to find such directory.
           (let ((candidate-make-file-name `("GNUmakefile" "makefile" "Makefile")))
             (or (catch 'break
                   (mapc (lambda (f)
                           (if (file-readable-p (concat (file-name-as-directory try-dir) f))
                               (throw 'break (buffer-action-throw-final-path try-dir))))
                         candidate-make-file-name)
                   nil)
                 (buffer-action-find-make-dir
                  (expand-file-name (concat (file-name-as-directory try-dir) "..")))))))

       (defun buffer-action-compile-setup (row)
         "Setup correct compiler action. ROW is matched one in
`buffer-action-table'."
         (let* ((dir (buffer-action-find-make-dir (expand-file-name ".")))
                (cmd (concat "make -C " dir)))
           (if (and dir (y-or-n-p (format "Run like this? `%s' " cmd)))
               (setq buffer-action-compile-action (concat cmd " "))
             (setq buffer-action-compile-action (nth 1 row)))))))
  )

(deh-section "compilation"
  (setq compilation-auto-jump-to-first-error t
        compilation-scroll-output t)
  ;;# Close complication buffer if succeed to compile
  (setq compilation-finish-functions
        (lambda (buf str)
          (when (and (string= (buffer-name buf) "*compilation*")
                     (not (string-match "exited abnormally" str))
                     (not (string-match "warning" str)))
            (run-at-time 0.5 nil 'delete-windows-on buf))))
  )

(deh-section-after "hideif"
  (setq hide-ifdef-initially t
        hide-ifdef-shadow t))

