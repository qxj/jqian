

(defun color-theme-blackboard ()
  "Color theme by JD Huntington, based off the TextMate Blackboard theme, created 2008-11-27"
  (interactive)
  (color-theme-install
   '(color-theme-blackboard
     (
      ;; (background-color . "#0C1021")
      (background-color . "black")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#A7A7A7")
      (foreground-color . "#F8F8F8")
      (mouse-color . "sienna1"))
     ;; (default ((t (:background "#0C1021" :foreground "#F8F8F8"))))
     (default ((t (:background "black" :foreground "#F8F8F8"))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
     (font-lock-builtin-face ((t (:foreground "#F8F8F8"))))
     (font-lock-comment-face ((t (:italic t :foreground "#AEAEAE"))))
     (font-lock-constant-face ((t (:foreground "#D8FA3C"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#FF6400"))))
     (font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

     (font-lock-string-face ((t (:foreground "#61CE3C"))))
     (font-lock-type-face ((t (:foreground "#8DA6CE"))))
     ;;(font-lock-variable-name-face ((t (:foreground "#FF6400"))))
     (font-lock-variable-name-face ((t (:foreground "#40E0D0"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (region ((t (:background "#253B76"))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (highlight ((t (:background "#222222"))))
     (highlight ((t (:background "#001"))))
     (highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))))))

(defun color-theme-wombat ()
  "The wombat color theme for Emacs."
  (interactive)
  (let ((wombat-fg "#f6f3e8")
        (wombat-bg "#242424")
        (wombat-green "#95e454")
        (wombat-green+1 "#cae682")
        (wombat-green+2 "#4BC98A")
        (wombat-red-1 "#e5786d")
        (wombat-red "tomato")
        (wombat-blue-2 "#2e3436")
        (wombat-blue-1 "#64a8d8")
        (wombat-blue "#8ac6f2")
        (wombat-magenta "#cc99cc")
        (wombat-orange-1 "#f57900")
        (wombat-orange "#e65c00")
        (wombat-orange+1 "#e9b96e")
        (wombat-orange+2 "#ffc125")
        (wombat-purple-1 "#ad7fa8")
        (wombat-purple "#cc99cc")
        (wombat-pink-1 "#f283b6")
        (wombat-pink "#F6B3DF")
        (wombat-gray-1 "#444444")
        (wombat-gray "#424242")
        (wombat-gray+1 "#99968b"))
    (color-theme-install
     `(color-theme-wombat
       ((background-color . ,"#242424")
        (background-mode . nil)
        (border-color . ,wombat-bg)
        (foreground-color . ,wombat-fg)
        (cursor-color . "orange"))

       ;; Font Lock
       (font-lock-builtin-face ((t (:foreground ,wombat-blue))))
       (font-lock-comment-delimiter-face ((t (:italic t :slant italic :foreground ,wombat-gray+1))))
       (font-lock-comment-face ((t (:italic t :slant italic :foreground ,wombat-gray+1))))
       (font-lock-constant-face ((t (:foreground ,wombat-red-1))))
       (font-lock-doc-face ((t (:foreground ,wombat-gray+1))))
       (font-lock-function-name-face ((t (:foreground ,wombat-green+1))))
       (font-lock-keyword-face ((t (:foreground ,wombat-blue))))
       (font-lock-negation-char-face ((t (:foreground ,wombat-red))))
       (font-lock-preprocessor-face ((t (:foreground ,wombat-red-1))))
       (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
       (font-lock-regexp-grouping-construct ((t (:bold t ,wombat-green))))
       (font-lock-string-face ((t (:italic t :foreground ,wombat-green))))
       (font-lock-type-face ((t (:foreground ,wombat-green+1))))
       (font-lock-variable-name-face ((t (:foreground ,wombat-blue))))
       (font-lock-warning-face ((t (:bold t :foreground ,wombat-red))))

       ;; UI Items
                                        ;(border ((t (:background "#888a85"))))
                                        ;(fringe ((t (:background "grey10"))))
       (minibuffer-prompt ((t (:foreground ,wombat-red :bold t))))
       (mode-line ((t (:background ,wombat-gray-1 :foreground ,wombat-fg))))
       (mode-line-emphasis ((t (:bold t))))
       (mode-line-highlight ((t (:background ,wombat-orange :box nil))))
       (mode-line-inactive ((t (:background ,wombat-bg :box (:line-width 1 :color ,wombat-gray :style nil)))))
       (region ((t (:foreground ,"black" :background ,wombat-green+1))))


       ;; Highlighting
       (lazy-highlight ((t (:italic t :background "yellow" :foreground "black"))))
       (highlight ((t (:background ,wombat-gray-1))))
       (highlight-changes-delete-face ((t (:foreground "red" :underline t))))
       (highlight-changes-face ((t (:foreground "red"))))
       (secondary-selection ((t (:background ,wombat-blue-1 :foreground "black" :bold t))))
       (hl-line ((t (:background ,"#333333"))))


       ;; Org-mode
       (org-date ((t (:foreground "Cyan" :underline t))))
       (org-agenda-date ((t (:foreground ,wombat-blue))))
       (org-agenda-date-weekend ((t (:bold t :foreground ,wombat-orange :weight bold))))
       (org-hide ((t (:foreground ,wombat-bg))))
       (org-todo ((t (:foreground ,wombat-pink :bold t))))
       (org-hide ((t (:foreground ,wombat-bg))))
       (org-done ((t (:foreground ,wombat-green+2))))
       (org-level-1 ((t (:foreground ,wombat-blue :bold t))))
       (org-level-2 ((t (:foreground "#ee9a49")))) ;"#ee9a49"))))
       (org-level-3 ((t (:foreground "#ff83fa"))))
       (org-level-4 ((t (:foreground "#ffa500"))))
       (org-level-5 ((t (:foreground "#ff4040"))))

                                        ;(comint-highlight-input ((t (:italic t :bold t))))
                                        ;(comint-highlight-prompt ((t (:foreground "#8ae234"))))
       (isearch ((t (:background ,wombat-orange-1 :foreground ,wombat-blue-2))))
       (isearch-lazy-highlight-face ((t (:foreground ,wombat-blue-2 :background ,wombat-orange+1))))

       ;; Parenthesis Matching
       (paren-face-match ((t (:inherit show-paren-match-face))))
       (paren-face-match-light ((t (:inherit show-paren-match-face))))
       (paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
       (show-paren-match-face ((t (:background ,"blue" :foreground "white" :bold t))))
       (show-paren-mismatch-face ((t (:background ,wombat-purple-1 :foreground ,wombat-blue-2))))

       (persp-selected-face ((t (:foreground ,wombat-blue-2))))

       (info-xref ((t (:foreground ,wombat-blue))))
       (info-xref-visited ((t (:foreground ,wombat-purple-1))))
       ))))


(deh-require 'fit-frame
  ;; avoid conflicting to winsav.el
  ;; (add-hook 'after-make-frame-functions 'fit-frame)
  )

(deh-section "theme"
  ;; (set-cursor-color "red")
	     )

