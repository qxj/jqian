;; -*- mode: Emacs-Lisp -*-

;; Hi-lock: (("^;;; .*" (0 (quote hi-black-hb) t)))
;; Hi-lock: (("^;;;; .*" (0 (quote hi-black-b) t)))
;; Hi-lock: (("^\s+;;#.*" (0 (quote hi-blue) t)))
;; Hi-lock: (("make-variable-buffer-\\(local\\)" (0 font-lock-keyword-face)(1 'italic append)))
;; Hi-lock: end

;;; prefix key
(define-prefix-command 'ctl-c-map nil "Command prefix: C-c")
(define-prefix-command 'ctl-z-map nil "Command prefix: C-z")
(define-prefix-command 'ctl-cc-map nil "Command prefix: C-c c")
(define-prefix-command 'one-key-prefix nil "one-key prefix: C-c k")
(define-prefix-command 'multi-term-prefix nil "mul-term prefix: C-c t")

;;; global key binding
(deh-section kbd-global
  (bind-keys
   :map global-map
   ("C-c"  .  ctl-c-map)
   ("C-z"  .  ctl-z-map)
   ("C-d"  .  delete-char-or-region)
   ("<C-delete>"  .  delete-char-or-region)
   ("C-1"  .  extend-selection)
   ("M-2"  .  extend-selection)
   ("C-2"  .  set-mark-command)
   ("M-9"  .  anything)
   ("C-m"  .  newline-and-indent)
   ("C-j"  .  newline)
   ("C-a"  .  my-beginning-of-line)
   ("C-e"  .  my-end-of-line)
   ("C-S-k"  .  my-delete-line-backward)
   ("C-k"  .  my-delete-line)
   ("M-d"  .  my-delete-word)
   ("<M-backspace>" .  my-backward-delete-word)
   ("M-DEL" .  my-backward-delete-word) ; mac os x
   ("C-o"   .  vi-open-next-line)
   ("C-M-o" .  split-line)
   ("C-"   .  redo)
   ("C-\\"  .  my-comment-or-uncomment-region)
   ("M-5"   .  my-display-buffer-path)
   ("M-0"   .  other-window)
   ("C-M-0" .  sr-speedbar-select-window)
   ("M-1"   .  sdcv-search)
   ("M-'"   .  just-one-space)
   ("M--"   .  delete-blank-lines)
   ("M-J"   .  vi-join-lines)
   ("C-M-j" .  vi-merge-lines)
   ;; ("M-m" .  smart-mark)
   ("M-q"  .   compact-uncompact-block)
   ("M-["  .   er/expand-region)
   ("M-]"  .   er/contract-region)
   ;; similar to pager-row-up/down
   ;; ("<up>" . (lambda () (interactive) (scroll-up-command 1)))
   ;; ("M-n" . (lambda () (interactive) (scroll-up-command 1)))
   ;; ("<down>" . (lambda () (interactive) (scroll-down-command 1)))
   ;; ("M-p" .  (lambda () (interactive) (scroll-down-command 1)))
   ("<C-M-down>" .  my-move-line-down)
   ("<C-M-up>"   .  my-move-line-up)
   ("<M-S-down>" .  my-dup-line-down)
;;;; highlight symbol
   ("<C-f3>" .  highlight-symbol-at-point)
   ("<f3>"   .  highlight-symbol-next)
   ("<S-f3>" .  highlight-symbol-prev)
;;;; one key
   ("<f12>"  . one-key-menu-toggle)
   ("<f8>"   . org-agenda)
   ("<f7>"   . calendar)
   ("C-h j"  .  (lambda () (interactive) (info "elisp")))
   ("C-h C-w" .  woman)
   ("<C-mouse-4>" .  text-scale-increase)
   ("<C-mouse-5>" .  text-scale-decrease)
   ("<C-down-mouse-1>" .  undefined)
   ))

(deh-section kbd-ctl-x
  (bind-keys
   :map ctl-x-map
   ("C-b" . ibuffer)
   ("C-j" . dired-jump)
   ("C-t" . transpose-sexps)
   ("C-r" . find-file-root)
   ("C-k" . kill-this-buffer)
   ("C-o" . my-switch-recent-buffer)
   ("C-_" . fit-frame)
   ;; ;; ("t"  .  template-expand-template)
   ;; ;; ("m"  .  message-mail)
   ("\\"  .  align-regexp)
   ("C-2" .  pop-global-mark)
   ))

(deh-section kbd-ctl-c
  (bind-keys
   :map ctl-c-map
   ("k" . one-key-prefix)
   ("t" . multi-term-prefix)
   ("c" . ctl-cc-map)
   ("C-k" . kmacro-keymap)
   ("$" . toggle-truncate-lines)
   ;; ("f" . comint-dynamic-complete)
   ("g" . magit-status)
   ("l" . magit-log)
   ("i" . imenu)
   ("j" . ffap)
   ("r" . org-capture)
   ("C-j" . ace-jump-mode)
   ("C-p" . ace-jump-mode-pop-mark)
   ;; ("k" . auto-fill-mode)
   ;; ("q" . refill-mode)
   ;; ("u" . revert-buffer)
   ;; ("v" . imenu-tree)
   ;; ("w" . my-favorite-window-config)
   ("C-b" .  browse-url-at-point)
   ;; ("C-b" . browse-url-of-buffer)
   ("C-t" . tv-view-history)
   ("\t"  . tempo-complete-tag)
   ))

(deh-section kbd-ctl-cc
  (bind-keys
   :map ctl-cc-map
   ("b" . my-revert-buffer)
   ("c" . my-switch-scratch)
   ("d" . deh-customize-inplace)
   ("f" . flycheck-mode)
   ("h" . highlight-symbol-at-point)
   ("i" . ispell-word)
   ("m" . desktop-menu)
   ("n" . my-clone-buffer)
   ("o" . recentf-open-files-compl)
   ("r" . buffer-action-run)
   ("s" . buffer-action-compile)
   ("t" . auto-insert)
   ("u" . undo-tree-visualize)
   ("v" . view-mode)
   ("\t" . ispell-complete-word)
   ))

(bind-keys
 :map ctl-z-map
 ("\C-z" . (if (eq window-system 'x) suspend-frame suspend-emacs)))

(bind-keys
 :map minibuffer-local-map
 ("\t" . comint-dynamic-complete))

;;; one-key settings
(deh-package one-key
  :config
  (progn
    (custom-set-faces
     '(one-key-keystroke ((t (:foreground "DarkRed" :weight bold))))
     '(one-key-prompt ((t (:foreground "navy"))))
     '(one-key-title ((t (:foreground "blue")))))

    ;;# Root
    (defun one-key-menu-root ()
      "The `one-key' menu for root."
      (interactive)
      (one-key-menu
       "ROOT"
       '(
         (("t" . "Toggle") . one-key-menu-toggle)
         (("a" . "Anything") . one-key-menu-anything)
         (("g" . "Gtags") . one-key-menu-gtags)
         (("c" . "Cscope") . one-key-menu-cscope)
         (("h" . "Highlight") . one-key-menu-highlight)
         (("s" . "Show Hide") . one-key-menu-hideshow)
         (("v" . "Version Control") . one-key-menu-vc)
         (("w" . "Window") . one-key-menu-window)
         (("p" . "Projectile") . one-key-menu-projectile))))

    ;;# Anything
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

    ;;# helm
    (defun one-key-menu-helm ()
      "The `one-key' menu for HELM."
      (interactive)
      (require 'helm-config nil t)    ; latter load
      (one-key-menu
       "HELM"
       '(
         (("h" . "Helm mini") . helm-mini)
         (("b" . "Buffers") . helm-buffers-list)
         (("B" . "Bookmarks") . helm-bookmarks)
         (("c" . "Commands") . helm-M-x)
         (("l" . "Locate Files") . helm-locate)
         (("i" . "Imenu") . helm-imenu)
         (("k" . "Kill Ring") . helm-show-kill-ring)
         (("o" . "Occur") . helm-occur)
         (("r" . "Register") . helm-register)
         (("m" . "Man Pages") . helm-man-woman)
         ) t))

    ;;# Toggle
    (defun one-key-menu-toggle ()
      "The `one-key' menu for TOGGLE."
      (interactive)
      (one-key-menu
       "TOGGLE"
       '(
         (("e" . "Erc") . my-toggle-erc)
         (("g" . "Gdb") . my-toggle-gdb)
         (("i" . "Info") . my-toggle-info)
         (("s" . "SpeedBar") . my-toggle-sr-speedbar)
         (("S" . "Slime") . my-toggle-slime)
         (("t" . "Multi-Term") . my-toggle-multi-term)
         (("T" . "Twitter") . my-toggle-twittering)
         (("u" . "Gnus") . my-toggle-gnus)
         (("w" . "W3m") . my-toggle-w3m)
         ) t))

    ;;# Gtags
    (defun one-key-menu-gtags ()
      "The `one-key' menu for GTAGS."
      (interactive)
      (one-key-menu
       "GTAGS"
       '(
         (("," . "Find Tag Define") . gtags-find-tag-from-here)
         (("." . "Find Tag Reference (No Prompt)") . gtags-find-rtag-no-prompt)
         ((">" . "Find Tag Reference") . gtags-find-rtag)
         (("t" . "Search Tag Define") . gtags-find-tag)
         (("s" . "Find Symbol") . gtags-find-symbol)
         (("p" . "Find Pattern") . gtags-find-pattern)
         (("/" . "Pop Stack") . gtags-pop-stack)
         (("b" . "Switch Current Window") . gtags-switch-to-buffer)
         (("o" . "Switch Other Window") . gtags-switch-to-buffer-other-window)
         (("x" . "Parse File") . gtags-parse-file)
         (("f" . "Find File") . gtags-find-file)
         (("g" . "Find With Grep") . gtags-find-with-grep)
         (("i" . "Find With Idutils") . gtags-find-with-idutils)
         (("m" . "Make Complete List") . gtags-make-complete-alist)
         (("q" . "Query Replace Regexp") . gtags-query-replace-regexp)
         (("v" . "Visit Root Directory") . gtags-visit-rootdir)
         (("r" . "Return Window") . gtags-select-tag-return-window)) t))

    ;;# Cscope
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

    ;;# Highlight
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

    ;;# Hideshow
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

    ;;# Window
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

    ;;# Verson control
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
    ;;# projectile
    (defun one-key-menu-projectile ()
      "The `one-key' menu for Projectile."
      (interactive)
      (one-key-menu
       "Projectile"
       '(
         (("f" . "Find file") . projectile-find-file)
         (("z" . "Add to cache") . projectile-cache-current-file)
         (("s" . "Swith project") . projectile-switch-project)
         (("g" . "Grep") . projectile-grep)
         (("b" . "Swith buffer") . projectile-switch-to-buffer)
         (("o" . "Multi occur") . projectile-multi-occur)
         (("r" . "Replace") . projectile-replace)
         (("e" . "Recent opened") . projectile-recentf)
         (("k" . "Kill all buffers") . projectile-kill-buffers)
         (("D" . "Root dired") . projectile-dired)
         (("R" . "Regenerate Tags") . projectile-regenerate-tags)
         (("c" . "Compile!") . projectile-compile-project)
         ) t))
    )
  :bind
  (
   ("k"  .  one-key-menu-root)
   ("a"  .  one-key-menu-anything)
   ("t"  .  one-key-menu-toggle)
   ("g"  .  one-key-menu-gtags)
   ("c"  .  one-key-menu-cscope)
   ("h"  .  one-key-menu-highlight)
   ("s"  .  one-key-menu-hideshow)
   ("v"  .  one-key-menu-vc)
   ("w"  .  one-key-menu-window)
   ("p"  .  one-key-menu-projectile)
   ))
