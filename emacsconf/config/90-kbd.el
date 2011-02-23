;;; key setting
(define-prefix-command 'ctl-cc-map nil "Command prefix: C-c c")
(define-prefix-command 'ctl-ck-map nil "one-key prefix: C-c k")
(define-prefix-command 'ctl-z-map nil "Command prefix: C-z")

;; global key binding
(deh-define-key global-map
  ((kbd "C-z")   . 'ctl-z-map)
  ((kbd "C-d")   . 'delete-char-or-region)
  ((kbd "<C-delete>")   . 'delete-char-or-region)
  ((kbd "C-1")   . 'smart-mark-whole-sexp)
  ((kbd "C-2")   . 'set-mark-command)
  ((kbd "C-m")   . 'newline-and-indent)
  ((kbd "C-j")   . 'newline)
  ((kbd "C-a")   . 'my-beginning-of-line)
  ((kbd "C-e")   . 'my-end-of-line)
  ((kbd "C-S-k") . 'my-delete-line-backward)
  ((kbd "C-k")   . 'my-delete-line)
  ((kbd "M-d")   . 'my-delete-word)
  ((kbd "<M-backspace>") . 'my-backward-delete-word)
  ((kbd "C-o")   . 'vi-open-next-line)
  ((kbd "C-'")   . 'redo)
  ((kbd "C-\\")  . 'my-comment-or-uncomment-region)
  ((kbd "M-5")   . 'my-display-buffer-path)
  ((kbd "M-0")   . 'other-window)
  ((kbd "C-M-0")   . 'sr-speedbar-select-window)
  ((kbd "M-1")   . 'sdcv-search)
  ((kbd "M-'")   . 'just-one-space)
  ((kbd "M-f")   . 'ywb-camelcase-forward-word)
  ((kbd "M-b")   . 'ywb-camelcase-backward-word)
  ((kbd "M-m")   . 'smart-mark)
  ((kbd "<C-M-down>") . 'my-move-line-down)
  ((kbd "<C-M-up>")   . 'my-move-line-up)
  ((kbd "<M-S-down>") . 'my-dup-line-down)
  ((kbd "<S-down>") . 'my-dup-line-down-continued)
  ((kbd "<f5> <f5>") . 'my-revert-buffer)
  ((kbd "<f6>")  . 'my-toggle-sr-speedbar)
  ((kbd "<f11>") . 'w3m)
  ((kbd "<f12>") . 'my-switch-recent-buffer)
  ((kbd "<f8>")  . 'org-agenda)
  ((kbd "<f7>")  . 'calendar)
  ((kbd "C-h j") . (lambda () (interactive) (info "elisp")))
  ((kbd "C-h C-w") . 'woman)
  ((kbd "<C-mouse-4>") . 'text-scale-increase)
  ((kbd "<C-mouse-5>") . 'text-scale-decrease)
  ((kbd "<C-down-mouse-1>") . 'undefined)
  )

(deh-define-key (lookup-key global-map "\C-c")
  ("c" . 'ctl-cc-map)
  ("k" . 'ctl-ck-map)
  ("$" . 'toggle-truncate-lines)
  ;; ("f" . 'comint-dynamic-complete)
  ;; ("g" . 'fold-dwim-hide-all)
  ("i" . 'imenu)
  ("j" . 'ffap)
  ;; ("k" . 'auto-fill-mode)
  ;; ("q" . 'refill-mode)
  ;; ("u" . 'revert-buffer)
  ;; ("v" . 'imenu-tree)
  ;; ("w" . 'ywb-favorite-window-config)
  ("\C-b" . 'browse-url-at-point)
  ;; ("\C-b" . 'browse-url-of-buffer)
  ("\C-o" . 'my-switch-recent-buffer)
  ("\C-r" . 'my-revert-buffer)
  ("\C-t" . 'tv-view-history)
  )

(deh-define-key (lookup-key global-map "\C-x")
  ("\C-t" . 'transpose-sexps)
  ("\C-r" . 'find-file-root)
  ("\C-k" . 'kill-this-buffer)
  ("\C-_" . 'fit-frame)
  ;; ("t"    . 'template-expand-template)
  ("t"    . 'template-simple-expand-template)
  ("m"    . 'message-mail)
  ("c"    . 'ywb-clone-buffer)
  )

(deh-define-key ctl-cc-map
  ("a" . 'org-agenda)
  ("b" . 'org-iswitchb)
  ("c" . 'my-switch-scratch)
  ("d" . 'deh-customize-inplace)
  ("f" . 'find-library)
  ("h" . 'highlight-symbol-at-point)
  ("i" . 'ispell-word)
  ("l" . 'org-store-link)
  ("o" . 'recentf-open-files-compl)
  ("r" . 'compile-dwim-run)
  ("s" . 'compile-dwim-compile)
  ("v" . 'view-mode)
  ("x" . 'incr-dwim)
  ("z" . 'decr-dwim)
  ("\t" . 'ispell-complete-word)
  )

(deh-define-key ctl-z-map
  ("\C-z" . (if (eq window-system 'x) 'suspend-frame 'suspend-emacs)))

(windmove-default-keybindings)

(deh-define-key minibuffer-local-map
  ("\t" . 'comint-dynamic-complete))
(deh-define-key read-expression-map
  ("\t" . 'PC-lisp-complete-symbol))

(deh-require 'one-key
  (deh-define-key ctl-ck-map
    ("k" . 'one-key-menu-root)
    ("g" . 'one-key-menu-gtags)
    ("c" . 'one-key-menu-cscope)
    ("h" . 'one-key-menu-highlight)
    ("s" . 'one-key-menu-hideshow)
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ROOT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq one-key-menu-root-alist
        '(
          (("g" . "Gtags") . one-key-menu-gtags)
          (("c" . "Cscope") . one-key-menu-cscope)
          (("h" . "Highlight") . one-key-menu-highlight)
          (("s" . "Show Hide") . one-key-menu-hideshow)))

  (defun one-key-menu-root ()
    "The `one-key' menu for root."
    (interactive)
    (one-key-menu "ROOT" one-key-menu-root-alist))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Gtags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defvar one-key-menu-gtags-alist nil
    "The `one-key' menu alist for GTAGS.")

  (setq one-key-menu-gtags-alist
        '(
          (("," . "Find Tag Define") . xgtags-find-tag-from-here)
          (("." . "Find Tag Reference (No Prompt)") . xgtags-find-rtag-no-prompt)
          ((">" . "Find Tag Reference") . xgtags-find-rtag)
          (("t" . "Search Tag Define") . xgtags-find-tag)
          (("s" . "Find Symbol") . xgtags-find-symbol)
          (("p" . "Find Pattern") . xgtags-find-pattern)
          (("/" . "Pop Stack") . xgtags-pop-stack)
          (("b" . "Switch Current Window") . xgtags-switch-to-buffer)
          (("o" . "Switch Other Window") . xgtags-switch-to-buffer-other-window)
          (("x" . "Parse File") . xgtags-parse-file)
          (("f" . "Find File") . xgtags-find-file)
          (("g" . "Find With Grep") . xgtags-find-with-grep)
          (("i" . "Find With Idutils") . xgtags-find-with-idutils)
          (("m" . "Make Complete List") . xgtags-make-complete-alist)
          (("q" . "Query Replace Regexp") . xgtags-query-replace-regexp)
          (("v" . "Visit Root Directory") . xgtags-visit-rootdir)
          (("r" . "Return Window") . xgtags-select-tag-return-window)))

  (defun one-key-menu-gtags ()
    "The `one-key' menu for GTAGS."
    (interactive)
    (one-key-menu "GTAGS" one-key-menu-gtags-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cscope ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defvar one-key-menu-cscope-alist nil
    "The `one-key' menu alist for CSCOPE.")

  (setq one-key-menu-cscope-alist
        '(
          (("s" . "This Symbol") . cscope-find-this-symbol)
          (("d" . "Definition Prompt") . cscope-find-global-definition)
          (("g" . "Definition No Prompt") . cscope-find-global-definition-no-prompting)
          (("f" . "This File") . cscope-find-this-file)
          (("i" . "Including This File") . cscope-find-files-including-file)
          (("c" . "Calling This Function") . cscope-find-functions-calling-this-function)
          (("e" . "This Function Called") . cscope-find-called-functions)
          (("p" . "Pattern") . cscope-find-egrep-pattern)
          (("t" . "This String") . cscope-find-this-text-string)))

  (defun one-key-menu-cscope ()
    "The `one-key' menu for CSCOPE."
    (interactive)
    (one-key-menu "CSCOPE" one-key-menu-cscope-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Highlight ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defvar one-key-menu-highlight-alist nil
    "The `one-key' menu alist for Highlight.")

  (setq one-key-menu-highlight-alist
        '(
          (("h" . "Highlight At Point") . highlight-symbol-at-point)
          (("u" . "Remote All Highlights") . highlight-symbol-remove-all)
          (("n" . "Next Highlight") . highlight-symbol-next)
          (("p" . "Previous Highlight") . highlight-symbol-prev)
          (("N" . "Next Highlight In Defun") . highlight-symbol-next-in-defun)
          (("P" . "Previous Highlight In Defun") . highlight-symbol-prev-in-defun)
          (("q" . "Replace symbol At Point") . highlight-symbol-query-replace)))

  (defun one-key-menu-highlight ()
    "The `one-key' menu for Highlight."
    (interactive)
    (one-key-menu "HIGHLIGHT" one-key-menu-highlight-alist t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hideshow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defvar one-key-menu-hideshow-alist nil
    "The `one-key' menu alist for HIDESHOW.")

  (setq one-key-menu-hideshow-alist
        '(
          (("s" . "Show Block") . hs-show-block)
          (("h" . "Hide Block") . hs-hide-block)
          (("c" . "Toggle Hiding") . hs-toggle-hiding)
          (("j" . "Show All") . hs-show-all)
          (("k" . "Hide All") . hs-hide-all)))

  (defun one-key-menu-hideshow ()
    "The `one-key' menu for HIDESHOW."
    (interactive)
    (one-key-menu "HIDESHOW" one-key-menu-hideshow-alist t))

  (deh-section "one-key-default"
    ;; digested from one-key-default.el (rubikitch)
    (defun one-key-default-create-menu (key &rest depends)
      (ignore-errors
        (dolist (key depends)
          (let ((sym (intern (format "one-key-menu-%s"
                                     (replace-regexp-in-string " " "-" key)))))
            (one-key-default-set-key key sym)))
        (with-temp-buffer
          (one-key-insert-template key key)
          (eval-buffer))))

    (defun one-key-default-set-key (keystroke command)
      (let ((kb (read-kbd-macro keystroke)))
        (when (keymapp (key-binding kb))
          (global-set-key kb command))))

    ;; default settings
    (one-key-default-create-menu "ESC ESC")
    (one-key-default-create-menu "ESC" "ESC ESC")
    (one-key-default-create-menu "C-x RET")
    (one-key-default-create-menu "C-x ESC")
    (one-key-default-create-menu "C-x 4")
    (one-key-default-create-menu "C-x 5")
    (one-key-default-create-menu "C-x n")
    (one-key-default-create-menu "C-x v")
    (one-key-default-create-menu "C-x a i")
    (one-key-default-create-menu "C-x a" "C-x a i")
    (one-key-default-create-menu "C-x @")
    (one-key-default-create-menu "C-x"
                                 "C-x RET" "C-x ESC" "C-x 4" "C-x 5"
                                 "C-x a" "C-x n" "C-x v")
    (one-key-default-create-menu "M-g ESC")
    (one-key-default-create-menu "M-g" "M-g ESC")
    (one-key-default-create-menu "M-o ESC")
    (one-key-default-create-menu "M-o" "M-o ESC")
    (one-key-default-create-menu "<f1> 4")
    (one-key-default-create-menu "<f1>" "<f1> 4")
    )
  )