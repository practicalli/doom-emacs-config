;;; +smartparens.el -*- lexical-binding: t; -*-

;; Smartparens - Practicalli config and key bindings


;; A Spacemacs like Lisp state menu (without the transient state)

(map! :leader
      (:prefix ("k". "Smartparens")
       :desc "Slurp forward" "s" #'sp-forward-slurp-sexp
       :desc "Slurp backward" "S" #'sp-backward-slurp-sexp
       :desc "" "$"   #'sp-end-of-sexp
       (:prefix ("`" . "Hybrid"))
       :desc "Kill" "k" #'sp-kill-hybrid-sexp
       :desc "Push" "p" #'sp-push-hybrid-sexp
       :desc "Slurp" "s" #'sp-slurp-hybrid-sexp
       :desc "Transpose" "t" #'sp-transpose-hybrid-sexp
       :desc "Absorb" "a" #'sp-absorb-sexp
       :desc "Barf forward" "b" #'sp-forward-barf-sexp
       :desc "Barf backward" "B" #'sp-backward-barf-sexp
       :desc "Convoluted" "c" #'sp-convolute-sexp
       (:prefix ("d" . "Delete")
        :desc "Symbol" "s" #'sp-kill-symbol
        :desc "Symbol Backward" "S" #'sp-backward-kill-symbol
        :desc "Word" "w" #'sp-kill-word
        :desc "Word Backward" "W" #'sp-backward-kill-word
        :desc "Kill" "x" #'sp-kill-sexp
        :desc "Kill Backward" "X" #'sp-backward-kill-sexp)
       :desc "Splice" "e" #'sp-splice-sexp-killing-forward
       :desc "Splice Backward" "E" #'sp-splice-sexp-killing-backward
       :desc "Symbol Backward" "h" #'sp-backward-symbol
       :desc "Sexp Backward" "H" #'sp-backward-sexp
       :desc "Join" "j" #'sp-join-sexp
       :desc "Sexp Forward" "l" #'sp-forward-sexp
       :desc "Sexp Forward" "L" #'sp-forward-sexp
       :desc "Raise" "r" #'sp-raise-sexp
       :desc "Slurp" "s" #'sp-forward-slurp-sexp
       :desc "Slurp Backward" "S" #'sp-backward-slurp-sexp
       :desc "Transpose" "t" #'sp-transpose-sexp
       :desc "Up Backward" "U" #'sp-backward-up-sexp
       (:prefix ("w" . "Wrap")
        :desc "()" "(" #'sp-wrap-round
        :desc "{}" "{" #'sp-wrap-curly
        :desc "[]" "[" #'sp-wrap-square
        :desc "Round" "w" #'sp-wrap-round
        :desc "Curly" "c" #'sp-wrap-curly
        :desc "Square" "s" #'sp-wrap-square
        :desc "Unwrap" "u" #'sp-unwrap-sexp)
       :desc "Copy sexp" "y" #'sp-copy-sexp))
