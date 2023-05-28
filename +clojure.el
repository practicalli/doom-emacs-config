;;; $DOOMDIR/+clojure.el -*- lexical-binding: t; -*-

;; ---------------------------------------
;; Clojure

;; Connecting to a reomote nREPL server
;; (setq nrepl-use-ssh-fallback-for-remote-hosts t)

;; Configure CIDER variables & LSP hooks

(use-package! clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|nbb\\)" . clojure-mode)
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t
        clojure-toplevel-inside-comment-form t  ;; evaluate expressions in comment as top level
        )
   (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil)
  (defvar org-babel-default-header-args:clojure '((:results . "silent")))'
  (defun org-babel-execute:clojure (body params)
    "Evaluate a block of Clojure code with Babel."
    (lisp-eval-string body))
  (defun clerk-show ()
    (interactive
     (when-let
         ((filename
           (buffer-file-name)))
       (save-buffer)
       (cider-interactive-eval
        (concat "(nextjournal.clerk/show! \"" filename "\")"))))))


(use-package! cider
  :after clojure-mode
  :config
  (setq cider-show-error-buffer t               ; show stacktrace buffer
        cider-print-fn 'puget                   ; pretty printing with sorted keys / set values
        cider-result-overlay-position 'at-point ; results shown right after expression
        cider-overlays-use-font-lock t

        ;; LSP features over Cider features
        cider-font-lock-dynamically nil         ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-use-xref nil                      ; cider xref to find definitions

        ;; minimise the repl buffer activity
        cider-repl-buffer-size-limit 100        ; limit lines shown in REPL buffer
        cider-repl-display-help-banner nil      ; disable help banner
        cider-repl-history-size 10              ; limit command history
        cider-repl-history-file nil             ; write repl buffer commands to file DOOMDIR/.local/cider-repl-history
        cider-repl-history-highlight-current-entry nil   ; cider default
        cider-repl-history-highlight-inserted-item nil   ; cider default
        cider-repl-history-quit-action 'quit-window ; restores previous emacs window config (cider default )
        cider-repl-pop-to-buffer-on-connect nil ; REPL buffer shown at starup (nil does not show buffer)
        cider-repl-use-clojure-font-lock nil
        cider-repl-use-pretty-printing nil
        )
  (set-lookup-handlers! '(cider-mode cider-repl-mode) nil) ; use lsp
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)
  ;; use lsp completion
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))))



;; Kaocha test runner from Emacs
;; - provides rich test reports
(use-package! kaocha-runner
  :after cider
  :config
  ;; enable Kaocha test runner
  (setq clojure-enable-kaocha-runner t))

;; End of Clojure
;; ---------------------------------------


;; ---------------------------------------
;; Clojure helper functions

;; Toggle reader comment #_ at beginnig of an expression
(defun clojure-toggle-reader-comment-sexp ()
  (interactive)
  (let* ((point-pos1 (point)))
    (evil-insert-line 0)
    (let* ((point-pos2 (point))
           (cmtstr "#_")
           (cmtstr-len (length cmtstr))
           (line-start (buffer-substring-no-properties point-pos2 (+ point-pos2 cmtstr-len)))
           (point-movement (if (string= cmtstr line-start) -2 2))
           (ending-point-pos (+ point-pos1 point-movement 1)))
      (if (string= cmtstr line-start)
          (delete-char cmtstr-len)
        (insert cmtstr))
      (goto-char ending-point-pos)))
  (evil-emacs-state))

;; Assign keybinding to the toggle-reader-comment-sexp function
(define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)

;; ---------------------------------------


;; ---------------------------------------
;; Portal Integration

;; def portal to the dev namespace to allow dereferencing via @dev/portal
(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
    "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

;; Key bindings added to Debug Clojure section
;; - , d p p - portal open
;; - , d p c - portal clear
;; - , d p D - portal clear

;; ---------------------------------------
(defvar project-dir "~/work/frap/bb")

(defun corgi/cider-jack-in-babashka (&optional project-dir)
  "Start a utility CIDER REPL backed by Babashka, not related to a
specific project."
  (interactive)
  (let ((project-dir (or project-dir user-emacs-directory)))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buf)
       (set-process-query-on-exit-flag
        (get-buffer-process server-buf) nil)
       (cider-nrepl-connect
        (list :repl-buffer server-buf
              :repl-type 'clj
              :host (plist-get nrepl-endpoint :host)
              :port (plist-get nrepl-endpoint :port)
              :project-dir project-dir
              :session-name "babashka"
              :repl-init-function (lambda ()
                                    (setq-local cljr-suppress-no-project-warning t
                                                cljr-suppress-middleware-warnings t
                                                process-query-on-exit-flag nil)
                                    (set-process-query-on-exit-flag
                                     (get-buffer-process (current-buffer)) nil)
                                    (rename-buffer "*babashka-repl*"))))))))


;; ---------------------------------------
;; Clojure key bindings
;; TODO: review evaluation key bindings from Spacemacs

(map! :after cider
      :map clojure-mode-map
      :localleader
      :desc "REPL session" "'" #'sesman-start

      ;; Debug Clojure
      (:prefix ("d" . "debug/inspect")
       :desc "debug" "d" #'cider-debug-defun-at-point
       (:prefix ("i" . "inspect")
        :desc "last expression" "e" #'cider-inspect-last-sexp
        :desc "expression" "f" #'cider-inspect-defun-at-point
        :desc "inspector" "i" #'cider-inspect
        :desc "last result" "l" #'cider-inspect-last-result
        (:prefix ("p" . "portal")
         :desc "Clear" "c" #'portal.api/open
         :desc "Clear" "D" #'portal.api/close
         :desc "Open" "p" #'portal.api/open)
        :desc "value" "v" #'cider-inspect-expr))

      ;; Evaluation
      (:prefix "e"
       :desc "Expression to comment" ";" #'cider-eval-defun-to-comment
       ;; :desc "" "e$" #'spacemacs/cider-eval-sexp-end-of-line
       :desc "at point" "(" #'cider-eval-list-at-point
       :desc "buffer" "b" #'cider-eval-buffer
       "D" nil  ; Doom: send to repl
       :desc "prev expression" "e" #'cider-eval-last-sexp
       :desc "expresion" "f" #'cider-eval-defun-at-point
       :desc "interupt" "i" #'cider-interrupt
       ;; :desc "" "el" #'spacemacs/cider-eval-sexp-end-of-line
       :desc "macroexpand" "m" #'cider-macroexpand-1
       :desc "macroexpand all" "M" #'cider-macroexpand-all
       :desc "region" "r" #'cider-eval-region
       :desc "undefine" "u" #'cider-undef
       :desc "undefine" "U" #'cider-undef-all
       :desc "expresion at point" "v" #'cider-eval-sexp-at-point
       :desc "expresion upto point" "V" #'cider-eval-sexp-up-to-point
       :desc "replace with result" "w" #'cider-eval-last-sexp-and-replace)

      ;; Format Clojure
      (:prefix ("=" . "format")
       :desc "buffer" "=" #'cider-format-buffer
       :desc "region" "r" #'cider-format-region
       :desc "expression" "f" #'cider-format-defun
       (:prefix ("e" . "edn")
        :desc "expression" "b" #'cider-format-edn-buffer
        :desc "prev expression" "e" #'cider-format-edn-last-sexp
        :desc "expression" "r" #'cider-format-edn-region))

      ;; Goto / jump
      (:prefix ("g" . "goto/jump")
       :desc "pop back" "b" #'cider-pop-back
       :desc "classpath" "c" #'cider-classpath
       ;; :desc "Find var" "c" #'spacemacs/clj-find-var
       :desc "find ns" "n" #'cider-find-ns
       :desc "error" "e" #'cider-jump-to-compilation-error
       :desc "resource" "r" #'cider-find-resource
       :desc "spec" "s" #'cider-browse-spec
       :desc "spec All" "S" #'cider-browse-spec-all)

      ;; Help & Documentation
      (:prefix ("h" . "help")
       :desc "apropos" "a" #'cider-apropos
       :desc "cheetsheet" "c" #'cider-cheatsheet
       :desc "clojure docs" "d" #'cider-clojuredocs
       :desc "javadoc" "j" #'cider-javadoc
       :desc "browse ns" "n" #'cider-browse-ns
       :desc "browse all ns" "N" #'cider-browse-ns-all
       :desc "browse spec" "s" #'cider-browse-spec
       :desc "browse all spe" "S" #'cider-browse-spec-all)

      ;; Evaluation - Namespaces
      (:prefix ("n" . "namespace")
       :desc "reload all" "a" #'cider-ns-reload-all
       :desc "" "n" #'cider-eval-ns-form
       :desc "" "r" #'cider-ns-refresh
       :desc "" "l" #'cider-ns-reload
       :desc "" "L" #'cider-ns-reload-all)

      ;; Evaluation - Pretty print
      (:prefix ("n" . "Pretty print")
       :desc "Expression comment" ";" #'cider-pprint-eval-defun-to-comment
       :desc "Preceeding expresion comment" ":" #'cider-pprint-eval-last-sexp-to-comment
       :desc "Expression" "f" #'cider-pprint-eval-defun-at-point
       :desc "Preceeding Expression" "e" #'cider-pprint-eval-last-sexp)

      ;; Refactor - Doom clj-refactor hydra menu
      (:prefix-map ("R" . nil))

      ;; REPL Sesison management
      (:prefix ("s" . "REPL Session")
       ;; :desc "toggle buffer" "a" (if (eq m 'cider-repl-mode) 'cider-switch-to-last-clojure-buffer 'cider-switch-to-repl-buffer)
       :desc "Browse Session" "b" #'sesman-browser
       :desc "Goto Session" "g" #'sesman-goto
       :desc "Session Info" "i" #'sesman-info
       :desc "quit" "q" #'sesman-quit
       :desc "quit session" "Q" #'sesman-quit-session
       :desc "restart" "r" #'sesman-restart
       :desc "start Session" "s" #'sesman-start

       (:prefix ("l" . "Link Sessions")
        :desc "buffer" "b" #'sesman-link-with-buffer
        :desc "directory" "d" #'sesman-link-with-directory
        :desc "project" "p" #'sesman-link-with-project
        :desc "project" "s" #'cider-connect-sibling-clj
        :desc "project" "S" #'cider-connect-sibling-cljs
        :desc "unlink" "u" #'sesman-unlink))

      ;; Testing
      (:prefix ("t" . "Testing")
       :desc "loaded" "l" #'cider-test-run-loaded-tests
       :desc "namespace" "n" #'cider-test-run-ns-tests
       :desc "project" "p" #'cider-test-run-project-tests
       :desc "filters" "s" #'cider-test-run-ns-tests-with-filters
       :desc "show report" "S" #'cider-test-show-report
       :desc "filters" "r" #'cider-test-rerun-failed-tests
       :desc "filters" "R" #'cider-test-rerun-test
       :desc "test" "t" #'cider-test-run-test)

      (:prefix ("T" . "Toggle")
       :desc "auto-test" "a" #'cider-auto-test-mode
       :desc "enlightenment" "e" #'cider-enlighten-mode
       :desc "namespace" "n" #'cider-test-run-ns-tests
       :desc "project" "p" #'cider-test-run-project-tests
       :desc "filters" "s" #'cider-test-run-ns-tests-with-filters
       :desc "show report" "S" #'cider-test-show-report
       :desc "filters" "r" #'cider-test-rerun-failed-tests
       :desc "filters" "R" #'cider-test-rerun-test
       :desc "test" "t" #'cider-test-run-test))


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


;; ---------------------------------------
;; Clojure-mode configurations
;;
;; Do not indent single ; comment characters
;; (add-hook 'clojure-mode-hook (lambda () (setq-local comment-column 0)))

;; Auto-indent code automatically
;; https://emacsredux.com/blog/2016/02/07/auto-indent-your-code-with-aggressive-indent-mode/
;; Conflicts with Clojure LSP automatic formatting
;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; Lookup functions in Clojure - The Essentail Reference book
;; https://github.com/p3r7/clojure-essential-ref
;; (spacemacs/set-leader-keys "oh" 'clojure-essential-ref)
;;
;; end of clojure configuration
;; ---------------------------------------
