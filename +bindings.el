;;; +bindings.el -*- lexical-binding: t; -*-

;; Key binding guide
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/
;; NOTE: use `map!' macro for convienience
;;
;; Bind key onto Evil Normal state
;; (map! :after evil
;;       :map evil-normal-state-map
;;       "/" #'+default/search-buffer)

;; ------------------------------------------------
;; Key binding vars

;; fd as Esc key binding
;; https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode/59/7
(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

;; https://discourse.doomemacs.org/t/what-are-leader-and-localleader-keys/153
;; Doom Defaults: `SPC' leader key, `SPC m' local leader
;; Practicalli: Set local leader to `,'
(setq doom-localleader-key ",")
;; ------------------------------------------------


;; ------------------------------------------------
;; Over-ride or add to Doom Emacs default key bindings

(map! :leader
      "SPC" nil
      :desc "M-x" "SPC" #'execute-extended-command)

;; Layout keys - disable `SPC TAB' workspace prefix
(map! :leader
       (:prefix-map ("TAB" . nil))
       (:prefix ("l". "Layouts")
         :desc "Last Layout" "<tab>" #'+workspace/other
         :desc "Display Tabs" "d" #'+workspace/display
         :desc "Delete layout" "D" #'+workspace/delete
         :desc "Layout list" "l" #'+workspace/switch-to
         :desc "Load Layout" "L" #'+workspace/load
         :desc "New Layout" "n" #'+workspace/new
         :desc "Rename Layout" "r" #'+workspace/rename
         :desc "Restore session" "R" #'+workspace/restore-last-session
         :desc "Save Layout" "s" #'+workspace/save
         :desc "Kill Session" "x" #'+workspace/kill-session
         :desc "Switch to 0" "0" #'+workspace/switch-to-0
         :desc "Switch to 1" "1" #'+workspace/switch-to-1
         :desc "Switch to 2" "2" #'+workspace/switch-to-2
         :desc "Switch to 3" "3" #'+workspace/switch-to-3
         :desc "Switch to 4" "4" #'+workspace/switch-to-4
         :desc "Switch to 5" "5" #'+workspace/switch-to-5
         :desc "Switch to 6" "6" #'+workspace/switch-to-6
         :desc "Switch to 7" "7" #'+workspace/switch-to-7
         :desc "Switch to 8" "8" #'+workspace/switch-to-8
         :desc "Switch to 9" "9" #'+workspace/switch-to-9))

;; Buffer customisations
(map! :leader
         "TAB" nil
         :desc "Last Buffer" "TAB" #'evil-switch-to-windows-last-buffer)

;; Replace Doom `/' highlight with buffer-search
(map! :after evil
      :map evil-normal-state-map
      "/" #'+default/search-buffer)

(map! :leader
       (:prefix "b"
         :desc "Dashboard" "h" #'+doom-dashboard/open
         :desc "Toggle Last" "TAB" #'evil-switch-to-windows-last-buffer))


;; Treemacs
;; Toggle treemacs project browser from project menu
(map! :leader
      (:prefix "p"
         "t" nil  ; disable project todos key binding
         :desc "Project browser" "t" #'+treemacs/toggle))


;; Change SPC g s to call Magit Status, rather than stage hunk at point
(map! :leader
      (:prefix "g"
        :desc "" "s" nil  ; remove existing binding
        :desc "Magit Status" "s" #'magit-status))

;; Diff of files
(map! :leader
       (:prefix "f"
        :desc "" "d" nil  ; remove existing binding
        (:prefix ("d" . "diff")
         :desc "3 files" "3" #'ediff3
         :desc "ediff" "d" #'diff
         :desc "ediff" "e" #'ediff
         :desc "version" "r" #'vc-root-diff
         :desc "version" "v" #'vc-ediff)))

;; Format
(map! :leader
       (:prefix ("=" . "format")
         :desc "buffer" "=" #'+format/buffer
         :desc "buffer" "b" #'+format/buffer
         :desc "region" "r" #'+format/region
         :desc "whitespace" "w" #'delete-trailing-whitespace))


;; ------------------------------------------------
;; Experiments
;; Use `,,` to close a commit message and `,k' to cancel
;; Doom maps `ZZ` to commit, `ZQ' to quit
;; (map! :after magit
;;       :map text-mode-map
;;       :localleader
;;       "," #'with-editor-finish
;;       "k" #'with-editor-cancel)

;; global-emacs bindings
(if IS-MAC (setq
             mac-right-command-modifier 'nil
             mac-command-modifier 'super
             mac-option-modifier 'meta
             mac-right-option-modifier 'nil
          ;;   mac-pass-control-to-system nil ;; what does this do?
             ))

(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-e") 'eval-region)
(global-set-key (kbd "s-b") 'eval-buffer)
(global-set-key (kbd "s-c") 'ns-copy-including-secondary)
;;clipboard yank
(global-set-key (kbd "M-v") 'clipboard-yank)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; Magit creates some global keybindings by default
;; but it's a nice to complement them with this one
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
