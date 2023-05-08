;;; +git.el -*- lexical-binding: t; -*-

;; ---------------------------------------
;; Version Control configuration

;; Load magit-todos
(use-package! magit-todos)

;; Location of developer tokens - default ~/.authinfo
;; Use XDG_CONFIG_HOME location or HOME
;; Optional:  (setq auth-source-cache-expiry nil)   ; default is 7200 (2h)
(setq auth-sources (list
                    (concat (getenv "XDG_CACHE_HOME") "/authinfo.gpg")
                    "~/.authinfo.gpg"))


;; Use Emacs as $EDITOR (or $GIT_EDITOR) for git commits messages
;; when using git commit on the command line
;; (global-git-commit-mode t)


;; Commit message checks
;; ~/.config/emacs/modules/emacs/vc/config.el
;; - checks for overlong-summary-line non-empty-line
;; (setq git-commit-summary-max-length 50
;;       git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
(after! magit (setq git-commit-summary-max-length 60

                    ;; Highlight specific characters changed
                    magit-diff-refine-hunk 'all

                    ;; Show project TODO lines in Magit Status
                    magit-todos-mode t

                    ;; Show Libravatar of commit author
                    magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
                    ))


;; Location of Git repositories
;; define paths and level of sub-directories to search
(setq magit-repository-directories
      '(("~/work/" . 2)))


;; Number of topics displayed (issues, pull requests)
;; open & closed, negative number for closed topics
;; or `forge-toggle-closed-visibility'
;; set closed to 0 to never show closed issues
;; (setq  forge-topic-list-limit '(100 . 0))
(setq  forge-topic-list-limit '(100 . -10))


;; GitHub user and organization accounts owned
;; used by @ c f  to create a fork
(setq forge-owned-accounts
      '(("trovemoney" "tempo"
         "frap" "frap"
         )))


;; Blacklist specific accounts, over-riding forge-owned-accounts
;; (setq forge-owned-blacklist
;;       '(("bad-hacks" "really-bad-hacks")))
;;
;;
;;
;;
;; End of Version Control configuration
;; ---------------------------------------
