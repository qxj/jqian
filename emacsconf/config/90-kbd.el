;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

;;; prefix key
(define-prefix-command 'ctl-cc-map nil "Command prefix: C-c c")
(define-prefix-command 'ctl-z-map nil "Command prefix: C-z")
(define-prefix-command 'one-key-prefix nil "one-key prefix: C-c k")
(define-prefix-command 'bm-prefix nil "bm prefix: C-c b")
(define-prefix-command 'multi-term-prefix nil "mul-term prefix: C-c t")

;;; global key binding
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
  ((kbd "C-M-0") . 'sr-speedbar-select-window)
  ((kbd "M-1")   . 'sdcv-search)
  ((kbd "M-'")   . 'just-one-space)
  ((kbd "M--")   . 'delete-blank-lines)
  ((kbd "M-J")   . 'vi-join-lines)
  ((kbd "M-M")   . 'vi-merge-lines)
  ((kbd "M-f")   . 'camelcase-forward-word)
  ((kbd "M-b")   . 'camelcase-backward-word)
  ((kbd "M-m")   . 'smart-mark)
  ((kbd "M-[")   . 'recent-jump-jump-backward)
  ((kbd "M-]")   . 'recent-jump-jump-forward)
  ((kbd "<C-M-down>") . 'my-move-line-down)
  ((kbd "<C-M-up>")   . 'my-move-line-up)
  ((kbd "<M-S-down>") . 'my-dup-line-down)
  ((kbd "<S-down>") . 'my-dup-line-down-continued)
;;;; highlight symbol
  ((kbd "<C-f3>") . 'highlight-symbol-at-point)
  ((kbd "<f3>") . 'highlight-symbol-next)
  ((kbd "<S-f3>") . 'highlight-symbol-prev)
;;;; one key
  ((kbd "<f5>") . 'one-key-menu-anything)
  ((kbd "<f6>") . 'one-key-menu-root)
  ((kbd "<f12>")  . 'one-key-menu-toggle)

  ((kbd "<f8>")  . 'org-agenda)
  ((kbd "<f7>")  . 'calendar)
  ((kbd "C-h j") . (lambda () (interactive) (info "elisp")))
  ((kbd "C-h C-w") . 'woman)
  ((kbd "<C-mouse-4>") . 'text-scale-increase)
  ((kbd "<C-mouse-5>") . 'text-scale-decrease)
  ((kbd "<C-down-mouse-1>") . 'undefined)
  )

(deh-define-key ctl-x-map
  ("\C-b" . 'ibuffer)
  ("\C-j" . 'dired-jump)
  ("\C-t" . 'transpose-sexps)
  ("\C-r" . 'find-file-root)
  ("\C-k" . 'kill-this-buffer)
  ("\C-o" . 'my-switch-recent-buffer)
  ("\C-_" . 'fit-frame)
  ;; ("t"    . 'template-expand-template)
  ;; ("m"    . 'message-mail)
  )

(deh-define-key (lookup-key global-map "\C-c")
  ("b" . 'bm-prefix)
  ("k" . 'one-key-prefix)
  ("t" . 'multi-term-prefix)
  ("c" . 'ctl-cc-map)
  ("$" . 'toggle-truncate-lines)
  ;; ("f" . 'comint-dynamic-complete)
  ;; ("g" . 'fold-dwim-hide-all)
  ("i" . 'imenu)
  ("j" . 'ffap)
  ("r" . 'org-capture)
  ;; ("k" . 'auto-fill-mode)
  ;; ("q" . 'refill-mode)
  ;; ("u" . 'revert-buffer)
  ;; ("v" . 'imenu-tree)
  ;; ("w" . 'ywb-favorite-window-config)
  ("\C-b" . 'browse-url-at-point)
  ;; ("\C-b" . 'browse-url-of-buffer)
  ("\C-t" . 'tv-view-history)
  )

(deh-define-key ctl-cc-map
  ("b" . 'my-revert-buffer)
  ("c" . 'my-switch-scratch)
  ("d" . 'deh-customize-inplace)
  ("f" . 'find-library)
  ("h" . 'highlight-symbol-at-point)
  ("i" . 'ispell-word)
  ("n" . 'ywb-clone-buffer)
  ("o" . 'recentf-open-files-compl)
  ("r" . 'buffer-action-run)
  ("s" . 'buffer-action-compile)
  ("t" . 'template-simple-expand-template)
  ("v" . 'view-mode)
  ("\t" . 'ispell-complete-word)
  )

(deh-define-key bm-prefix
  ("b" . 'bm-toggle)
  ("n" . 'bm-next)
  ("p" . 'bm-previous)
  ("s" . 'bm-show)
  ("a" . 'bm-show-all))

(deh-define-key multi-term-prefix
  ("c" . 'multi-term)
  ("t" . 'multi-term-dedicated-open-select)
  ("q" . 'multi-term-dedicated-close)
  ("s" . 'multi-term-dedicated-select)
  ("g" . 'multi-term-dedicated-toggle))

(deh-define-key ctl-z-map
  ("\C-z" . (if (eq window-system 'x) 'suspend-frame 'suspend-emacs)))

(windmove-default-keybindings)

(deh-define-key minibuffer-local-map
  ("\t" . 'comint-dynamic-complete))
(deh-define-key read-expression-map
  ("\t" . 'PC-lisp-complete-symbol))

;;; one-key settings
(deh-require 'one-key
  (deh-define-key one-key-prefix
    ("k" . 'one-key-menu-root)
    ("a" . 'one-key-menu-anything)
    ("t" . 'one-key-menu-toggle)
    ("g" . 'one-key-menu-gtags)
    ("c" . 'one-key-menu-cscope)
    ("h" . 'one-key-menu-highlight)
    ("s" . 'one-key-menu-hideshow)
    ("v" . 'one-key-menu-vc)
    ("w" . 'one-key-menu-window)
    )

;;;; Root
  (defun one-key-menu-root ()
    "The `one-key' menu for root."
    (interactive)
    (one-key-menu
     "ROOT"
     '(
       (("a" . "Anything") . one-key-menu-anything)
       (("t" . "Toggle") . one-key-menu-toggle)
       (("g" . "Gtags") . one-key-menu-gtags)
       (("c" . "Cscope") . one-key-menu-cscope)
       (("h" . "Highlight") . one-key-menu-highlight)
       (("s" . "Show Hide") . one-key-menu-hideshow)
       (("v" . "Version Control") . one-key-menu-vc)
       (("w" . "Window") . one-key-menu-window))))

;;;; Anything
  (defun one-key-menu-anything ()
    "The `one-key' menu for ANYTHING."
    (interactive)
    (require 'anything-config nil t)    ; latter load
    (one-key-menu
     "ANYTHING"
     '(
       (("a" . "Anything") . anything)
       (("b" . "Buffers") . anything-buffers+)
       (("B" . "Bookmarks") . anything-c-pp-bookmarks)
       (("c" . "Commands") . anything-M-x)
       (("f" . "Files") . anything-for-files)
       (("i" . "Imenu") . anything-imenu)
       (("I" . "Info") . anything-info-pages)
       (("k" . "Kill Ring") . anything-show-kill-ring)
       (("o" . "Occur") . anything-occur)
       (("r" . "Register") . anything-register)
       (("m" . "Man Pages") . anything-man-woman)
       (("SPC" . "Execute anything commands") . anything-execute-anything-command)
       ) t))

;;;; Toggle
  (defun one-key-menu-toggle ()
    "The `one-key' menu for TOGGLE."
    (interactive)
    (one-key-menu
     "TOGGLE"
     '(
       (("e" . "Erc") . my-toggle-erc)
       (("g" . "Gdb") . my-toggle-gdb)
       (("i" . "Info") . my-toggle-info)
       (("m" . "Mutt") . my-toggle-mutt)
       (("s" . "SpeedBar") . my-toggle-sr-speedbar)
       (("S" . "Slime") . my-toggle-slime)
       (("t" . "Multi-Term") . my-toggle-multi-term)
       (("T" . "Twitter") . my-toggle-twittering)
       (("u" . "Gnus") . my-toggle-gnus)
       (("w" . "W3m") . my-toggle-w3m)
       ) t))

;;;; Gtags
  (defun one-key-menu-gtags ()
    "The `one-key' menu for GTAGS."
    (interactive)
    (one-key-menu
     "GTAGS"
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
       (("r" . "Return Window") . xgtags-select-tag-return-window)) t))

;;;; Cscope
  (defun one-key-menu-cscope ()
    "The `one-key' menu for CSCOPE."
    (interactive)
    (one-key-menu
     "CSCOPE"
     '(
       (("s" . "This Symbol") . cscope-find-this-symbol)
       (("d" . "Definition Prompt") . cscope-find-global-definition)
       (("g" . "Definition No Prompt") . cscope-find-global-definition-no-prompting)
       (("f" . "This File") . cscope-find-this-file)
       (("i" . "Including This File") . cscope-find-files-including-file)
       (("c" . "Calling This Function") . cscope-find-functions-calling-this-function)
       (("e" . "This Function Called") . cscope-find-called-functions)
       (("p" . "Pattern") . cscope-find-egrep-pattern)
       (("t" . "This String") . cscope-find-this-text-string)) t))

;;;; Highlight
  (defun one-key-menu-highlight ()
    "The `one-key' menu for Highlight."
    (interactive)
    (one-key-menu
     "HIGHLIGHT"
     '(
       (("h" . "Highlight At Point") . highlight-symbol-at-point)
       (("u" . "Remote All Highlights") . highlight-symbol-remove-all)
       (("n" . "Next Highlight") . highlight-symbol-next)
       (("p" . "Previous Highlight") . highlight-symbol-prev)
       (("N" . "Next Highlight In Defun") . highlight-symbol-next-in-defun)
       (("P" . "Previous Highlight In Defun") . highlight-symbol-prev-in-defun)
       (("q" . "Replace symbol At Point") . highlight-symbol-query-replace)) t))

;;;; Hideshow
  (defun one-key-menu-hideshow ()
    "The `one-key' menu for HIDESHOW."
    (interactive)
    (one-key-menu
     "HIDESHOW"
     '(
       (("s" . "Show Block") . hs-show-block)
       (("h" . "Hide Block") . hs-hide-block)
       (("c" . "Toggle Hiding") . hs-toggle-hiding)
       (("j" . "Show All") . hs-show-all)
       (("k" . "Hide All") . hs-hide-all)) t))

;;;; Window
  (defun one-key-menu-window ()
    "The `one-key' menu for WINDOW."
    (interactive)
    (one-key-menu
     "WINDOW"
     '(
       (("b" . "Balance") . balance-windows)
       (("l" . "Shrink If Larger") . shrink-window-if-larger-than-buffer)
       (("e" . "Enlarge") . enlarge-window)
       (("s" . "Shrink") . shrink-window)
       (("h" . "Enlarge H.") . enlarge-window-horizontally)
       (("y" . "Shrink H.") . shrink-window-horizontally)) t))

;;;; Verson control
  (defun one-key-menu-vc ()
    "The `one-key' menu for VERSION CONTROL."
    (interactive)
    (one-key-menu
     "VERSON CONTROL"
     '(
       (("+" . "Update") . vc-update)
       (("=" . "Diff Base") . vc-diff)
       (("#" . "Diff With Other Ver.") . vc-version-diff)
       (("~" . "View Other Ver.") . vc-revision-other-window)
       (("a" . "Update ChangeLog") . vc-update-change-log)
       ;; (("b" . "Switch Backend") . vc-switch-backend)
       ;; (("c" . "Rollback") . vc-rollback)
       (("d" . "Status") . vc-dir)
       (("g" . "Annotate!") . vc-annotate)
       (("h" . "Insert Headers") . vc-insert-headers)
       ;; (("i" . "Register") . vc-register)
       (("l" . "Print Log") . vc-print-log)
       (("m" . "Merge") . vc-merge)
       (("r" . "Retrieve Tag") . vc-retrieve-tag)
       (("s" . "Create Tag") . vc-create-tag)
       (("u" . "Revert") . vc-revert)
       (("v" . "Commit") . vc-next-action)
       ) t))
  )