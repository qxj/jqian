;; -*- coding: utf-8 -*-

(deh-require 'fit-frame
  ;; avoid conflicting to winsav.el
  ;; (add-hook 'after-make-frame-functions 'fit-frame)
  )

(deh-section "theme"
  ;; (set-cursor-color "red")
  (setq custom-theme-directory (expand-file-name "theme" my-startup-dir))
  (load-theme 'adwaita t))
