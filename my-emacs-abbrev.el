(define-abbrev-table 'global-abbrev-table '(
    ("lamdba" "lambda")
    ("separte" "separate")
    ("funciton" "function")
    ("@ln" "$(\\lambda, \\nu)$")
    ("@xn" "$(\\xi, \\nu)$")
    ("@x0" "$(\\xi, \\xi_0)$")
    ("@r" "\\rho")
    ))

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
