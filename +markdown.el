;;; $DOOMDIR/+markdown.el -*- lexical-binding: t; -*-

;; ---------------------------------------
;; Markdown
;;
;; Doom Markdown configuration
;; ~/.config/emacs-doom/modules/lang/markdown/config.el


;; ---------------------------------------
;; Markdown key bindings

;; Changes
;; - move toggle prefix from `t' to `T'
;; - add table prefix `t'

(map! :map markdown-mode-map
        :localleader
        (:prefix ("t" . nil))
        (:prefix ("t" . "Table")
         :desc "Header" "h" #'markdown-table-hline-at-point-p
         :desc "Sort" "s" #'markdown-table-sort-lines
         :desc "Region to table" "r" #'markdown-table-convert-region
         :desc "Table insert" "t" #'markdown-insert-table
         (:prefix ("d" . "Delete")
          :desc "column" "c" #'markdown-table-delete-column
          :desc "row" "r" #'markdown-table-delete-row)
         (:prefix ("i" . "Insert")
          :desc "Column" "c" #'markdown-table-insert-column
          :desc "Row" "r" #'markdown-table-insert-row))

        (:prefix ("T" . "toggle")
         :desc "Inline LaTeX"      "e" #'markdown-toggle-math
         :desc "Code highlights"   "f" #'markdown-toggle-fontify-code-blocks-natively
         :desc "Inline images"     "i" #'markdown-toggle-inline-images
         :desc "URL hiding"        "l" #'markdown-toggle-url-hiding
         :desc "Markup hiding"     "m" #'markdown-toggle-markup-hiding
         :desc "Wiki links"        "w" #'markdown-toggle-wiki-links
         :desc "GFM checkbox"      "x" #'markdown-toggle-gfm-checkbox))

(use-package! org-protocol
  :after org
  :config
  (add-to-list 'org-protocol-protocol-alist
               '("org-find-file" :protocol "find-file" :function org-protocol-find-file :kill-client nil))
  (defun org-protocol-find-file (fname)
    "Process org-protocol://find-file?path= style URL."
    (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
      (find-file f)
      (raise-frame)
      (select-frame-set-input-focus (selected-frame)))))
