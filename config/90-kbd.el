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
   ([remap delete-char]  .  delete-char-or-region)           ;C-d
   ([remap move-beginning-of-line]  .  my/beginning-of-line) ;C-a
   ([remap move-end-of-line]  .  my/end-of-line)             ;C-e
   ([remap kill-line]  .  my/delete-line)                    ;C-k
   ([remap kill-word]  .  my/delete-word)                    ;M-d
   ([remap backward-kill-word] .  my/backward-delete-word) ;M-DEL, <C-backspace>

   ("M-d"  .  my/delete-word)           ;M-d
   ("C-S-k" .  my/delete-line-backward)
   ;; ("C-1"  .  extend-selection)         ; alternative er/expand-region
   ;; ("M-2"  .  extend-selection)
   ("C-2"   .  set-mark-command)
   ("C-m"  .  newline-and-indent)
   ("C-j"  .  newline)
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
   ("M-n"  . (lambda() (interactive) (scroll-up-command 1)))
   ("<down>" . (lambda() (interactive) (scroll-up-command 1)))
   ("M-p"  . (lambda() (interactive) (scroll-down-command 1)))
   ("<up>" . (lambda() (interactive) (scroll-down-command 1)))
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


(deh-package hydra
  :config
  (defhydra hydra-font-setup ()
    "
  Font Size^^
  ----------------------------------------------------------------
  _=_ ↑
  _\-_ ↓
  "
    ("=" text-scale-increase)
    ("-" text-scale-decrease))

  (defhydra hydra-highlight-symbol ()
    "
  ^Highlight^   ^Next^    ^Previous^
  ----------------------------------------------------------------
  _h_ighlight   _n_ext    _p_revious
  "
    ("h" highlight-symbol-at-point)
    ("n" highlight-symbol-next)
    ("p" highlight-symbol-prev))

  (defhydra hydra-window-buffer (:color red :hint nil)
    "
   ^Window^             ^Buffer^           ^Frame^
  ^^^^^^^^---------------------------------------------------
   ^hjkl^: move         _p_: previous      _u_: winner undo      ....../ \-.   .
   _s_: split below     _n_: next          _r_: winner redo   .-/     (    o\.//
   _v_: split right     _b_: switch        _w_: revert all     |  ...  \./\---'
   _c_: delete this     _;_: last          ^ ^                 |.||  |.||
   _o_: delete other    _K_: kill current  ^ ^
  ^^^^^^^^
  "
    ("w" revert-all-buffers :color blue)

    ("u" winner-undo)
    ("r" winner-redo)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer)
    (";" mode-line-other-buffer :color blue)

    ("s" split-window-below)
    ("v" split-window-right)

    ("K" kill-this-buffer)

    ("c" delete-window)
    ("o" delete-other-windows :color blue)

    ;; ("H" hydra-move-splitter-left)
    ;; ("J" hydra-move-splitter-down)
    ;; ("K" hydra-move-splitter-up)
    ;; ("L" hydra-move-splitter-right)

    ("q" nil))
  (global-set-key (kbd "C-x w") 'hydra-window-buffer/body)
  )

;; (deh-package guide-key
;;   :diminish guide-key-mode
;;   :config
;;   (setq guide-key/guide-key-sequence
;;         '("C-x c" "C-x r" "C-x v" "C-x 4"
;;           "C-c b" "C-c c" "C-c p"))
;;   (guide-key-mode 1))


(deh-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  ;; Hide/Modify some function prefix in which-key show menu
  (dolist (item '(("\\`calc-" . "") ; Hide "calc-" prefixes when listing M-x calc keys
                  ("/body\\'" . "") ; Remove display the "/body" portion of hydra fn names
                  ("modi/" . "m/") ; The car is intentionally not "\\`modi/" to cover
                                        ; cases like `hydra-toggle/modi/..'.
                  ("\\`hydra-" . "+h/")
                  ("\\`org-babel-" . "ob/")
                  ("\\`my/" . "")))
    (add-to-list 'which-key-description-replacement-alist item)))
