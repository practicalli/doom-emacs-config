;;; init-clojure.el --- Clojurce -*- lexical-binding: t -*-
;; Copyright (C) 2001-2023 Gas
;; Timestamp: <>
;; Author: Gas <gas@tuatara.red>
;; Version: 1.0
;; Package-Version: 0.8
;; Created: Sometime during the Covid-19 lockdown
;; Keywords: configuration, emacs
;; URL: https://github.com/frap/emacs
;; Package-Requires: ((emacs "27.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; This file maybe automatically tangled from config.org.
;; Hand edits will be overwritten!
;; Je t'ai prévenu putain!

;;; Code:
(use-package common-lisp-modes
  :diminish
;;  :disabled t
  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . common-lisp-modes-indent-or-fill-sexp))
  :preface
  (define-minor-mode common-lisp-modes-mode
    "Mode for enabling all modes that are common for lisps.
For reference, this is not a common-lisp modes mode, but a common
lisp-modes mode.
\\<common-lisp-modes-mode-map>"
    :lighter " clmm"
    :keymap (make-sparse-keymap))
  (defun common-lisp-modes-indent-or-fill-sexp ()
    "Indent s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (save-excursion
          (mark-sexp)
          (indent-region (point) (mark))))))
  (provide 'common-lisp-modes))

(defvar project-dir "~/work")

(use-package! clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|nbb\\)" . clojure-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  ;;(setq nrepl-use-ssh-fallback-for-remote-hosts t
  ;;      clojure-indent-style 'align-arguments)
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
  :mode (rx "/.edn" eos)
  :bind ("<M-return>" . 'clerk-show)
  ;;  :bind ("C-c C-a" . cider-eval-n-defuns)
  :config
  (defalias 'cquit 'cider-quit)
  ;; (require 'flycheck-clj-kondo)
  ;; ensure kondo is the first one
  ;;(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  ;;  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))

  ;;(define-key clojure-mode-map (kbd "C-:") 'hippie-expand-lines)
  ;;(define-key clojure-mode-map (kbd "C-\"") 'clojure-toggle-keyword-string)
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil)
  (defvar org-babel-default-header-args:clojure '((:results . "silent")))'
  (defun org-babel-execute:clojure (body params)
    "Evaluate a block of Clojure code with Babel."
    (lisp-eval-string body))
  (defun clojure-set-compile-command ()
    (let ((project-dir (clojure-project-dir)))
      (cond ((and (file-exists-p (expand-file-name "project.clj" project-dir))
                  (executable-find "lein"))
             (setq-local compile-command "lein "))
            ((and (file-exists-p (expand-file-name "deps.edn" project-dir))
                  (executable-find "clojure"))
             (setq-local compile-command "clojure ")))))
  (defun clojure-mode-setup ()
    "Setup Clojure buffer."
    (common-lisp-modes-mode)
    (clojure-set-compile-command)
    (flycheck-mode))
  (defun clerk-show ()
    (interactive
     (when-let
         ((filename
           (buffer-file-name)))
       (save-buffer)
       (cider-interactive-eval
        (concat "(nextjournal.clerk/show! \"" filename "\")"))))))

;; (use-package! cider
;;   :init
;; ;;  (setq cider-print-options
;; ;;        '(("length" 80)
;; ;;          ("level" 20)
;; ;;          ("right-margin" 80))
;; ;;        cider-save-file-on-load t)
;;   (setq cider-repl-result-prefix ";; => "
;;         cider-ns-refresh-show-log-buffer t
;;         ;;cider-font-lock-dynamically '(macro core function var deprecated)
;;         ;;cider-prompt-for-symbol nil
;;         )
;;   :config
;;   (add-hook 'cider-mode-hook #'eldoc-mode)
;;   (add-hook 'cider-repl-mode-hook #'eldoc-mode)
;;   )

;; (defun cider-eval-n-defuns (n)
;;   "Evaluate N top-level forms, starting with the current one."
;;   (interactive "P")
;;   (cider-eval-region (car (bounds-of-thing-at-point 'defun))
;;                      (save-excursion
;;                        (dotimes (i (or n 2))
;;                          (end-of-defun))
;;                        (point))))
;; smartparens-strict-mode to enforce parenthesis to match.
;; I map M-( to enclose the next expression as in paredit using a custom function.
;; Prefix argument can be used to indicate how many expressions to enclose
;; instead of just 1. E.g. C-u 3 M-( will enclose the next 3 sexps.

(use-package! cider
  :diminish t
  ;; :hook (((cider-repl-mode cider-mode) . eldoc-mode)
  ;;        (cider-repl-mode . common-lisp-modes-mode))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer))
  :custom-face
  (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
  (cider-error-highlight-face ((t (:inherit flymake-error))))
  (cider-warning-highlight-face ((t (:inherit flymake-warning))))
  :custom
  (nrepl-log-messages nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-tab-command #'indent-for-tab-command)
  (nrepl-hide-special-buffers t)
  (cider-test-show-report-on-success t)
  (cider-allow-jack-in-without-project t)
  (cider-use-fringe-indicators t )
  (cider-font-lock-dynamically '(macro var deprecated))
  (cider-save-file-on-load nil)
  (cider-inspector-fill-frame nil)
  (cider-auto-select-error-buffer t)
  (cider-eval-spinner t)
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (cider-enrich-classpath t)
  :config
  (setq cider-preferred-build-tool 'clojure-cli
        ;; ~make sure we can always debug nrepl issues~
        ;; Turning this off again, seems it may really blow up memory usage
        ;; nrepl-log-messages nil
        )

  ;; TODO: clean this up, submit to upstream where possible
  ;; More CIDER/clojure-mode stuff
  ;; - logical-sexp doesn't treat #_ correctly

  ;; New function, should go upstream. Kill all associated REPLs
  (defun corgi/cider-quit-all ()
    "Quit all current CIDER REPLs."
    (interactive)
    (let ((repls (seq-remove (lambda (r)
                               (equal r (get-buffer "*babashka-repl*")))
                             (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
      (seq-do #'cider--close-connection repls))
    ;; if there are no more sessions we can kill all ancillary buffers
    (cider-close-ancillary-buffers)
    ;; need this to refresh sesman browser
    (run-hooks 'sesman-post-command-hook))

  ;; When asking for a "matching" REPL (clj/cljs), and no matching REPL is found,
  ;; return any REPL that is there. This is so that cider-quit can be called
  ;; repeatedly to close all REPLs in a process. It also means that , s s will go
  ;; to any REPL if there is one open.
  (defun corgi/around-cider-current-repl (command &optional type ensure)
    (let ((repl (or
                 (if (not type)
                     (or (funcall command nil)
                         (funcall command 'any))
                   (funcall command type))
                 (get-buffer "*babashka-repl*"))))
      (if (and ensure (null repl))
          (cider--no-repls-user-error type)
        repl)))

  (advice-add #'cider-current-repl :around #'corgi/around-cider-current-repl)

  ;; This essentially redefines cider-repls. The main thing it does is return all
  ;; REPLs by using sesman-current-sessions (plural) instead of
  ;; sesman-current-session. It also falls back to the babashka repl if no repls
  ;; are connected/linked, so we can always eval.
  (defun corgi/around-cider-repls (_command &optional type ensure)
    (let ((type (cond
                 ((listp type)
                  (mapcar #'cider-maybe-intern type))
                 ((cider-maybe-intern type))))
          (repls (delete-dups
                  (seq-mapcat #'cdr
                              (or
                               (sesman-current-sessions 'CIDER)
                               (when ensure
                                 (user-error "CIDER sessions non liées"))))))
          (bb-repl (get-buffer "*babashka-repl*")))
      (or (seq-filter (lambda (b)
                        (and (cider--match-repl-type type b)
                             (not (equal b bb-repl))))
                      repls)
          (when bb-repl
            (list bb-repl)))))

  (advice-add #'cider-repls :around #'corgi/around-cider-repls)

  (defun corgi/cider-eval-last-sexp-and-replace ()
    "Alternative to cider-eval-last-sexp-and-replace, but kills
clojure logical sexp instead of ELisp sexp, and pprints the
result."
    (interactive)
    (let ((last-sexp (cider-last-sexp)))
      ;; we have to be sure the evaluation won't result in an error
      (cider-nrepl-sync-request:eval last-sexp)
      ;; seems like the sexp is valid, so we can safely kill it
      (let ((opoint (point)))
        (clojure-backward-logical-sexp)
        (kill-region (point) opoint))
      (cider-interactive-eval last-sexp
                              (cider-eval-pprint-with-multiline-comment-handler
                               (current-buffer)
                               (set-marker (make-marker) (point))
                               ""
                               " "
                               "")
                              nil
                              (cider--nrepl-print-request-map fill-column))))

  (defun corgi/cider-pprint-eval-last-sexp-insert ()
    (interactive)
    (let ((cider-comment-prefix "")
          (cider-comment-continued-prefix " ")
          (cider-comment-postfix ""))
      (cider-pprint-eval-last-sexp-to-comment)))

  (defadvice cider-find-var (before add-evil-jump activate)
    (evil-set-jump))

  (defun corgi/around-cider--choose-reusable-repl-buffer (_command _params)
    "Redefine cider--choose-reusable-repl-buffer to something more
sensible. If any dead REPL buffers exist when creating a new one
then simply delete them first. Return nil so `cider-create-repl'
creates a new one. Don't unnecessarily bother the user."
    (seq-do #'kill-buffer
            (seq-filter (lambda (b)
                          (with-current-buffer b
                            (and (derived-mode-p 'cider-repl-mode)
                                 (not (process-live-p (get-buffer-process b))))))
                        (buffer-list)))
    nil)

  (advice-add #'cider--choose-reusable-repl-buffer :around #'corgi/around-cider--choose-reusable-repl-buffer))

;; silence byte compiler
(require 'clojure-mode)
(require 'cider)

;; Most annoying JVM "feature" of all time
;; https://docs.cider.mx/cider/troubleshooting.html#empty-java-stacktraces
(defun corgi/around-cider-jack-in-global-options (command project-type)
  (if (eq 'clojure-cli project-type)
      (concat cider-clojure-cli-global-options
              " -J-XX:-OmitStackTraceInFastThrow")
    (funcall command project-type)))

(advice-add #'cider-jack-in-global-options :around #'corgi/around-cider-jack-in-global-options)

(use-package! clj-refactor
  :disabled t
  :diminish clj-refactor-mode
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
    :init
    (setq cljr-favor-prefix-notation nil
          cljr-favor-private-functions nil
          cljr-warn-on-eval nil
          cljr-eagerly-build-asts-on-startup nil
          cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]"
          cljr-cljs-clojure-test-declaration cljr-clojure-test-declaration
          cljr-cljc-clojure-test-declaration cljr-clojure-test-declaration
          cljr-magic-require-namespaces
          '(("io" . "clojure.java.io")
            ("cs" . "clojure.set")
            ("string" . "clojure.string")
            ("walk" . "clojure.walk")
            ("zip" . "clojure.zip")
            ("time" . "clj-time.core")
            ("log" . "clojure.tools.logging")
            ("jdbc" . "next.jdbc")
            ("pp" . "clojure.pprint")
            ("json" . "cheshire.json")))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-warn-on-eval nil))
;;  (cljr-add-keybindings-with-modifier "C-s-")
;;  (cljr-add-keybindings-with-prefix "C-c C-m")
;;  (define-key clj-refactor-map (kbd "C-x C-r") 'cljr-rename-file)
;;  (add-to-list 'cljr-project-clean-functions 'cleanup-buffer))


(use-package! clj-ns-name
:config
  (clj-ns-name-install))

(use-package! walkclj)

(use-package! flycheck-clj-kondo
  :after cider
  :when (executable-find "clj-kondo"))

(defun corgi/cider-pprint-eval-register (register)
  "Evaluate a Clojure snippet stored in a register.
Will ask for the register when used interactively. Put `#_clj' or
`#_cljs' at the start of the snippet to force evaluation to go to
a specific REPL type, no matter the mode (clojure-mode or
clojurescript-mode) of the current buffer.
You can use {{...}} to insert emacs-lisp code that willg get
evaluated, like `(println \"{{buffer-file-name}}\")'.
"
  (interactive (list (register-read-with-preview "Eval register: ")))
  (let ((reg (replace-regexp-in-string
              "{{\\([^}]+\\)}}"
              (lambda (s)
                (eval
                 (read
                  (match-string 1 s))))
              (get-register register))))
    (cond
     ((string-match-p "^#_cljs" reg)
      (with-current-buffer (car (cider-repls 'cljs))
        (cider--pprint-eval-form reg)))
     ((string-match-p "^#_clj" reg)
      (with-current-buffer (car (cider-repls 'clj))
        (cider--pprint-eval-form reg)))
     (t
      (cider--pprint-eval-form reg)))))

;; Backwards compatibility
(defalias 'corgi/cider-pprint-register #'corgi/cider-pprint-eval-register)

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

(defun corgi/cider-modeline-info ()
  (when (derived-mode-p 'clojure-mode)
    (let ((source-project-name (projectile-project-name)))
      (if-let* ((repls (ignore-errors (cider-repls (cider-repl-type-for-buffer)))))
          (thread-last
            repls
            (seq-map
             (lambda (repl)
               (with-current-buffer repl
                 (if (equal (buffer-name repl) "*babashka-repl*")
                     (propertize "bb" 'face '( :background "green"
                                               :foreground "black"))
                   (let ((info
                          (concat
                           (when-let ((repl-project-name
                                       (cider--project-name nrepl-project-dir)))
                             (when (not (equal repl-project-name
                                               source-project-name))
                               (concat ":" repl-project-name)))
                           (pcase (plist-get nrepl-endpoint :host)
                             ("localhost" "")
                             ("127.0.0.1" "")
                             (x (concat ":" x)))
                           ;;(format ":%d" (plist-get nrepl-endpoint :port))
                           )))
                     (cl-case cider-repl-type
                       (clj
                        (propertize (concat "clj" info)
                                    'face '( :background "#5881D8"
                                             :foreground "white")))
                       (cljs
                        (propertize (concat "cljs" info)
                                    'face '( :background "#f7df1e"
                                             :foreground "black")))
                       (pending-cljs
                        (propertize (concat "pending-cljs" info)
                                    'face '( :background "#f7df1e"
                                             :foreground "black")))))))))
            (s-join " "))
        (propertize "<pas connecté>" 'face '( :background "red"
                                               :foreground "white"))))))


(defun corgi/enable-cider-connection-indicator-in-current-buffer ()
  (when (not (seq-find
              (lambda (e)
                (eq e '(:eval (corgi/cider-modeline-info)))) mode-line-format))
    (setq mode-line-format
          (seq-mapcat
           (lambda (e)
             (if (eq 'mode-line-modes e)
                 '(" " (:eval (corgi/cider-modeline-info)) " " mode-line-modes)
               (list e)))
           mode-line-format))))

(defun corgi/enable-cider-connection-indicator ()
  "In Clojure buffers show an indicator in the modeline for which
 CIDER REPLs the current buffer is linked to, with color coding
for clj/cljs/bb, and extra info if the link goes to a different
project or host."
  (interactive)
  (add-hook 'clojure-mode-hook #'corgi/enable-cider-connection-indicator-in-current-buffer)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq 'clojure-mode major-mode)
        (corgi/enable-cider-connection-indicator-in-current-buffer)))))


;; Emacs has "registers", places to keep small snippets of text. We make it easy
;; to run a snippet of Clojure code in such a register, just press comma twice
;; followed by the letter that designates the register (while in a Clojure
;; buffer with a connected REPL). The code will be evaluated, and the result
;; pretty-printed to a separate buffer.

;; By starting a snippet with `#_clj' or `#_cljs' you can control which type of
;; REPL it will go to, in case you have both a CLJ and a CLJS REPL connected.
(set-register ?k "#_clj (do (require 'kaocha.repl) (kaocha.repl/run))")
(set-register ?K "#_clj (do (require 'kaocha.repl) (kaocha.repl/run-all))")
(set-register ?r "#_clj (do (require 'user :reload) (user/reset))")
(set-register ?g "#_clj (user/go)")
(set-register ?b "#_clj (user/browse)")

;; Enable our "connection indicator" for CIDER. This will add a colored marker
;; to the modeline for every REPL the current buffer is connected to, color
;; coded by type.
(corgi/enable-cider-connection-indicator)

;; Create a *scratch-clj* buffer for evaluating ad-hoc Clojure expressions. If
;; you make sure there's always a babashka REPL connection then this is a cheap
;; way to always have a place to type in some quick Clojure expression evals.
;;(with-current-buffer (get-buffer-create "*scratch-clj*")
;; (clojure-mode))

;; Connect to Babashka if we can find it. This is a nice way to always have a
;; valid REPL to fall back to. You'll notice that with this all Clojure buffers
;; get a green "bb" indicator, unless there's a more specific clj/cljs REPL
;; available.
(when (executable-find "bb")
  (corgi/cider-jack-in-babashka "~/work/frap/bb"))

;; add nbb repl experience
(cider-register-cljs-repl-type 'nbb-or-scittle "(+ 40 2)") ;;"(do (require '[promesa.core :as p]))")

(defun mm/cider-connected-hook ()
  (when (eq 'nbb-or-scittle cider-cljs-repl-type)
    (setq-local cider-show-error-buffer nil)
    (cider-set-repl-type 'cljs)))

(add-hook 'cider-connected-hook #'mm/cider-connected-hook)

(defun mm/cider-jack-in-nbb ()
  "Start a nbb nrepl process and connect."
  (interactive)
  (let* ((cider-allow-jack-in-without-project t)
         (orig-buffer (current-buffer))
         (params '(:jack-in-cmd "nbb nrepl-server :port 0"
                                :cljs-repl-type nbb-or-scittle))
         (params (cider--update-project-dir
                  params)))
    (nrepl-start-server-process
     (plist-get params :project-dir)
     (plist-get params :jack-in-cmd)
     (lambda (server-buffer)
       (with-current-buffer
           orig-buffer
         (cider-connect-sibling-cljs
          params
          server-buffer))))))

;;; FIXME: https://github.com/clojure-emacs/cider/issues/3255
(defun cider-verify-clojurescript-is-present ()
  "Check whether ClojureScript is present."
  (unless (nrepl-dict-get (cider-sync-tooling-eval "cljs.core/inc") "value")
    (user-error "ClojureScript n'est pas disponible.  See https://docs.cider.mx/cider/basics/clojurescript for details")))

(defun jet ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(use-package! clojure-snippets)

;; Kaocha test runner from Emacs
;; - provides rich test reports
(use-package! kaocha-runner
  :after cider
  :config
  ;; enable Kaocha test runner
  (setq clojure-enable-kaocha-runner t))

(provide '+clojure-corgi)
;;; +clojure.el ends here
