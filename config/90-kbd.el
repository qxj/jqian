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

(bind-keys
 ("C-c" . ctl-c-map)
 ("C-z" . ctl-z-map)
 ("C-c c" . ctl-cc-map))

;;; global key binding
(deh-section kbd-global
  (bind-keys
   ("C-d"  .  delete-char-or-region)
   ("<C-delete>"  .  delete-char-or-region)
   ;; ("C-1"  .  extend-selection)         ; alternative er/expand-region
   ;; ("M-2"  .  extend-selection)
   ("C-2"  .  set-mark-command)
   ("M-9"  .  anything)
   ("C-m"  .  newline-and-indent)
   ("C-j"  .  newline)
   ("C-a"  .  my/beginning-of-line)
   ("C-e"  .  my/end-of-line)
   ("C-S-k"  .  my/delete-line-backward)
   ("C-k"  .  my/delete-line)
   ("M-d"  .  my/delete-word)
   ("<M-backspace>" .  my/backward-delete-word)
   ("M-DEL" .  my/backward-delete-word) ; mac os x
   ("C-o"   .  vi-open-next-line)
   ("C-M-o" .  split-line)
   ("C-'"   .  redo)
   ("C-\\"  .  my/comment-or-uncomment-region)
   ("M-5"   .  my/display-buffer-path)
   ("M-0"   .  other-window)
   ("C-M-0" .  sr-speedbar-select-window)
   ("M-'"   .  just-one-space)
   ("M--"   .  delete-blank-lines)
   ("M-J"   .  vi-join-lines)
   ("C-M-j" .  vi-merge-lines)
   ;; ("M-m" .  smart-mark)
   ("M-q"  .   compact-uncompact-block)
   ("<C-M-down>" .  my/move-line-down)
   ("<C-M-up>"   .  my/move-line-up)
   ("<M-S-down>" .  my/dup-line-down)
   ("<f8>"   . org-agenda)
   ("<f7>"   . calendar)
   ("C-h j"  .  (lambda () (interactive) (info "elisp")))
   ("C-h C-w" .  woman)
   ("<C-mouse-4>" .  text-scale-increase)
   ("<C-mouse-5>" .  text-scale-decrease)
   ("<C-down-mouse-1>" .  undefined)
   ("<S-insert>"  .  yank-unindented)
   ))

(deh-section kbd-ctl-x
  (bind-keys
   :map ctl-x-map
   ("C-t" . transpose-sexps)
   ("C-r" . sudo-edit)
   ("C-k" . kill-this-buffer)
   ("C-o" . my/switch-recent-buffer)
   ("C-_" . fit-frame)
   ;; ("t"  .  template-expand-template)
   ;; ("m"  .  message-mail)
   ("\\"  .  align-regexp)
   ("C-2" .  pop-global-mark)
   ))

(deh-section kbd-ctl-c
  (bind-keys
   :map ctl-c-map
   ("C-k" . kmacro-keymap)
   ("$" . toggle-truncate-lines)
   ;; ("f" . comint-dynamic-complete)
   ("r" . org-capture)
   ;; ("k" . auto-fill-mode)
   ;; ("q" . refill-mode)
   ;; ("u" . revert-buffer)
   ;; ("v" . imenu-tree)
   ;; ("w" . my/favorite-window-config)
   ("C-b" .  browse-url-at-point)
   ;; ("C-b" . browse-url-of-buffer)
   ("C-t" . tv-view-history)
   ("'"   . toggle-quotes)
   ("\t"  . tempo-complete-tag)
   ))

(deh-section kbd-ctl-cc
  (bind-keys
   :map ctl-cc-map
   ("b" . my/revert-buffer)
   ("c" . my/switch-scratch)
   ("d" . deh-locate)
   ("f" . flycheck-mode)
   ("i" . ispell-word)
   ("l" . global-linum-mode)
   ("m" . desktop-menu)
   ("n" . my/clone-buffer)
   ("t" . auto-insert)
   ("v" . view-mode)
   ("\t" . ispell-complete-word)
   ))

(bind-keys
 :map ctl-z-map
 ("\C-z" . (if (eq window-system 'x) suspend-frame suspend-emacs)))

(bind-keys
 :map minibuffer-local-map
 ("\t" . comint-dynamic-complete))

(deh-package guide-key
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence
        '("C-x c" "C-x r" "C-x v" "C-x 4"
          "C-c b" "C-c c" "C-c p"))
  (guide-key-mode 1))
