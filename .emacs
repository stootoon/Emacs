;; Root path for the main lisp files.
(setq EMACS-ROOT-DIR "~/git/Emacs/")
;; Location of external code
(setq EMACS-EXTERNAL-DIR "~/git/Emacs/External/")

(setq ESS-ROOT-DIR "~/ESS/")
(setq JULIA-EXECUTABLE "/usr/local/bin/julia")
(setq ESS-LISP-PATH  (concat ESS-ROOT-DIR "lisp/"))
(setq GIT-EMACS-PATH "~/git/git-emacs/");

;; Now load the actual .emacs file
(load-file (concat EMACS-ROOT-DIR "dot-emacs.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(matlab-auto-fill nil)
 '(matlab-mode-hook (quote (matlab-cell-initialize (lambda nil (define-key matlab-mode-map (kbd "C-c C-e") (quote matlab-cell-run-current-cell))) (lambda nil (setq truncate-lines t)) turn-off-auto-fill)))
 '(mlint-programs (quote ("mlint" "/misc/apps/matlab/matlabR2010a/bin/glnxa64/mlint")))
 '(truncate-lines t)
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :background "blue" :foreground "white" :slant normal :height 2.0))))
 '(org-level-2 ((t (:inherit outline-2 :background "dodger blue" :foreground "white" :slant normal :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :background "cornflower blue" :foreground "white" :height 1.4)))))
