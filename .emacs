;; Root path for the main lisp files.

(package-initialize)

(setq EMACS-ROOT-DIR "~/git/Emacs/")
;; Location of external code
(setq EMACS-EXTERNAL-DIR "~/git/Emacs/External/")

(setq ESS-ROOT-DIR "~/ESS/")
(setq JULIA-EXECUTABLE "/usr/local/bin/julia")
(setq ESS-LISP-PATH  (concat ESS-ROOT-DIR "lisp/"))
(setq GIT-EMACS-PATH "~/git/git-emacs/");

;; Now load the actual .emacs file
(load-file (concat EMACS-ROOT-DIR "dot-emacs.el"))
