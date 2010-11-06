;;; key setting
(define-prefix-command 'ctrl-cc-map nil "Command prefix: C-c c")
(define-prefix-command 'ctrl-z-map nil "Command prefix: C-z")

;; global key binding
(deh-define-key global-map
  ((kbd "C-z")   . 'ctrl-z-map)
  ((kbd "C-d")   . 'delete-char-or-region)
  ((kbd "<C-delete>")   . 'delete-char-or-region)
  ((kbd "C-1")   . 'smart-mark-whole-sexp)
  ((kbd "C-2")   . 'set-mark-command)
  ((kbd "C-m")   . 'newline-and-indent)
  ((kbd "C-j")   . 'newline)
  ((kbd "C-a")   . 'my-beginning-of-line)
  ((kbd "C-e")   . 'my-end-of-line)
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
  ((kbd "<f5> <f5>") . 'my-revert-buffer)
  ((kbd "<f6>")  . 'my-toggle-sr-speedbar)
  ((kbd "<f11>") . 'w3m)
  ((kbd "<f12>") . 'my-switch-recent-buffer)
  ((kbd "<f8>")  . 'org-agenda)
  ((kbd "<f7>")  . 'calendar)
  ((kbd "C-h j") . (lambda () (interactive) (info "elisp")))
  ((kbd "<C-mouse-4>") . 'text-scale-increase)
  ((kbd "C-+") . 'text-scale-increase)
  ((kbd "<C-mouse-5>") . 'text-scale-decrease)
  ((kbd "C--") . 'text-scale-decrease)
  ((kbd "<C-down-mouse-1>") . 'undefined)
  )

(deh-define-key (lookup-key global-map "\C-c")
  ("c" . 'ctrl-cc-map)
  ("$" . 'toggle-truncate-lines)
  ;; ("f" . 'comint-dynamic-complete)
  ;; ("g" . 'fold-dwim-hide-all)
  ("i" . 'imenu)
  ("j" . 'ffap)
  ("k" . 'auto-fill-mode)
  ("q" . 'refill-mode)
  ;; ("u" . 'revert-buffer)
  ;; ("v" . 'imenu-tree)
  ;; ("w" . 'ywb-favorite-window-config)
  ("\C-o" . 'browse-url-at-point)
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

(deh-define-key ctrl-cc-map
  ("a" . 'org-agenda)
  ("b" . 'org-iswitchb)
  ("c" . 'ywb-create/switch-scratch)
  ("d" . 'deh-customize-inplace)
  ("f" . 'find-library)
  ("i" . 'ispell-word)
  ("l" . 'org-store-link)
  ("r" . 'compile-dwim-run)
  ("s" . 'compile-dwim-compile)
  ("v" . 'view-mode)
  ("x" . 'incr-dwim)
  ("z" . 'decr-dwim)
  ("\t" . 'ispell-complete-word)
  )


(deh-define-key ctrl-z-map
  ("\C-z" . (if (eq window-system 'x) 'suspend-frame 'suspend-emacs)))

(windmove-default-keybindings)

(deh-define-key minibuffer-local-map
  ("\t" . 'comint-dynamic-complete))
(deh-define-key read-expression-map
  ("\t" . 'PC-lisp-complete-symbol))

