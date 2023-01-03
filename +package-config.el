;;; +package-config.el -*- lexical-binding: t; -*-



;; ---------------------------------------
;; Configure packages
;;
;; Packages that are not part of Doom Emacs
;; are included via packages.el
;; and configured in package-config.el


;; Keycast - show keys and commands in modeline
;; https://github.com/staticaland/doom-emacs-config/blob/master/config.el
(use-package! keycast
  :commands keycast-mode   ;; load package on issuing command
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast-mode-line-update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast-mode-line-update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
      :height 0.9)
    '(keycast-key :inherit custom-modified
      :height 1.1
      :weight bold)))

;; Toggle keycast-mode
(map! :leader
      (:prefix "t"
       :desc "keycast" "k" #'keycast-mode))


;; ---------------------------------------
