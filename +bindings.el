;;; +bindings.el -*- lexical-binding: t; -*-

;; Key binding guide
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56

;; fd as Esc key binding
;; https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode/59/7
(after! evil-escape
  (setq evil-escape-key-sequence "fd"))

;; https://discourse.doomemacs.org/t/what-are-leader-and-localleader-keys/153
;; Defaults: `SPC' leader key, `SPC m' local leader
;; Set local leader to `,'
(setq doom-localleader-key ",")

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


;; Use `,,` to close a commit message and `,k' to cancel
;; Doom maps `ZZ` to commit, `ZQ' to quit
;; (map! :after magit
;;       :map text-mode-map
;;       :localleader
;;       "," #'with-editor-finish
;;       "k" #'with-editor-cancel)
