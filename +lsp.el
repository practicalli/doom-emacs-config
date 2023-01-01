;;; +lsp.el -*- lexical-binding: t; -*-


;; ---------------------------------------
;; LSP Configuration

(use-package! lsp-mode
  :commands lsp
  :config

  ;; Core
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil
        lsp-signature-function 'lsp-signature-posframe
        lsp-semantic-tokens-enable t
        lsp-idle-delay 0.2 ;; Smoother LSP features response in cost of performance (Most servers I use have good performance)
        lsp-use-plists nil)
  (add-hook 'lsp-after-apply-edits-hook (lambda (&rest _) (save-buffer)))
  (add-hook 'lsp-mode-hook (lambda () (setq-local company-format-margin-function #'company-vscode-dark-icons-margin)))

  ;; Clojure LSP local development
  ;; (let ((clojure-lsp-dev (expand-file-name "~/dev/clojure-lsp/clojure-lsp")))
  ;;   (when (file-exists-p clojure-lsp-dev)
  ;;     ;; clojure-lsp local development
  ;;     (setq lsp-clojure-custom-server-command `("bash" "-c" ,clojure-lsp-dev)
  ;;           lsp-completion-no-cache t
  ;;           lsp-completion-use-last-result nil)))
  )


(use-package! lsp-treemacs
  :config
  (setq lsp-treemacs-error-list-current-project-only t))


(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable t))

;; End of LSP
;; ---------------------------------------
