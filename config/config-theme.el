;; Who doesn't love zenburn?
;;(use-package zenburn-theme :config (load-theme 'zenburn t))
(use-package solarized-theme :config (load-theme 'solarized-light t))

;; Fancy mode line
;; (use-package powerline :demand t)
;; (use-package airline-themes
;;   :demand t
;;   :config (load-theme 'airline-base16-gui-dark t))
;;(use-package smart-mode-line-powerline-theme)
(use-package smart-mode-line
  :demand t
  :config
  (progn
    (sml/setup)
    (set-face-attribute 'mode-line nil
                        :underline nil)
    (set-face-attribute 'mode-line-inactive nil
                        :underline nil
                        :overline nil)
    (set-face-attribute 'header-line nil
                        :box nil
                        :underline nil)))

(custom-set-faces
 '(epe-dir-face ((t (:inherit eshell-prompt))))
 '(epe-symbol-face ((t (:inherit eshell-ls-symlink)))))

(provide 'config-theme)
