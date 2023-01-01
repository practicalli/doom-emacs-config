;;; +clojure.el -*- lexical-binding: t; -*-

;; ---------------------------------------
;; Clojure

;; Connecting to a reomote nREPL server
;; (setq nrepl-use-ssh-fallback-for-remote-hosts t)

;; Configure CIDER variables & LSP hooks

(use-package! clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t
        clojure-toplevel-inside-comment-form t  ;; evaluate expressions in comment as top level
))


(use-package! cider
  :after clojure-mode
  :config
  (setq cider-show-error-buffer t               ;'only-in-repl
        cider-font-lock-dynamically nil         ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil
        cider-use-xref nil                      ; use lsp

        cider-repl-pop-to-buffer-on-connect nil ; REPL buffer shown at starup
        cider-repl-display-help-banner nil      ; disable help banner
        cider-print-fn 'puget                   ; pretty printing with sorted keys / set values
        cider-result-overlay-position 'at-point ; results shown right after expression
        cider-overlays-use-font-lock t
        cider-repl-buffer-size-limit 100        ; limit lines shown in REPL buffer
        cider-repl-history-size 42
        )
  (set-lookup-handlers! '(cider-mode cider-repl-mode clj-refactor-mode) nil) ; use lsp
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)
  ;; use lsp completion
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))))



;; Kaocha test runner from Emacs
;; - provides rich test reports
(use-package! kaocha-runner
  :after cider
  :config
  (setq clojure-enable-kaocha-runner t          ; enable Kaocha test runner
))

;; End of Clojure
;; ---------------------------------------


;; ---------------------------------------
;; Clojure key bindings
;; TODO: review evaluation key bindings from Spacemacs

(map! :after cider
      :map clojure-mode-map
      :localleader
     (:prefix "e"
         :desc "Undefine" "u" #'cider-undef
         :desc "Undefine" "U" #'cider-undef-all)
      (:prefix ("s" . "REPL Session")
         :desc "Browse Session" "b" #'sesman-browser
         :desc "Session Info" "i" #'sesman-info
         :desc "Runner Warnings" "q" #'sesman-quit
         :desc "Kaocha Runner" "r" #'sesman-restart
         :desc "Start Session" "s" #'sesman-start))


;; Kaocha test runner from CIDER - Requires running REPL
;; next prefix expressions for key sequence, i.e. `SPC t k'
(map! :after kaocha-runner
      :map clojure-mode-map
      :localleader
      (:prefix "t"
       (:prefix ("k". "Kaocha")
         :desc "Run current test" "t" #'kaocha-runner-run-test-at-point
         :desc "Run test" "r" #'kaocha-runner-run-tests
         :desc "Run all tests" "a" #'kaocha-runner-run-all-tests
         :desc "Runner Warnings" "w" #'kaocha-runner-show-warnings
         :desc "Kaocha Runner" "h" #'kaocha-runner-hide-windows)))


;; End of Clojure Key bindings
;; ---------------------------------------
