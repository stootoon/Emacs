;; .emacs
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
			("melpa-stable" . "https://stable.melpa.org/packages/")
                        ("melpa" . "http://melpa.org/packages/")))

;;package-install git, git-blame, matlab-mode
;;git clone matlab-emacs into ~/git/External
(setq inhibit-splash-screen t)
(setq load-path (cons EMACS-ROOT-DIR load-path))
(setq load-path (cons EMACS-EXTERNAL-DIR load-path))

(load (concat EMACS-ROOT-DIR "emacs-functions.el"))

;; (global-hl-line-mode 1)
(defun st-turn-on-hl-line-mode ()
  (progn
    (hl-line-mode t)
    (set-face-background 'hl-line "#446")))

(defun st-set-up-down-bindings ()  
  "Sets the up and down keys to cycle commands as expected."
  (interactive)
  (progn
    (local-set-key (kbd "<up>") 'comint-previous-input)
    (local-set-key (kbd "<down>") 'comint-next-input)))

(defun st-reformat-json-buffer ()
  "Reformat the whole json buffer when you load it."
  (interactive)
  (json-reformat-region (point-min) (point-max)))

(add-hook 'ess-mode-hook 'st-turn-on-hl-line-mode)
(add-hook 'inferior-ess-mode-hook 'st-set-up-down-bindings)
(add-hook 'json-mode-hook 'st-reformat-json-buffer)

(display-line-numbers-mode 1) ;Turn on marginal line numbres

(window-numbering-mode) ;; Turn on window numbers.

(add-hook 'doc-view-mode-hook 'auto-revert-mode) ;; Refresh pdfs automatically

;; Recent files mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; rainbow-colors mode
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
; (add-hook 'ess-mode-hook 'rainbow-delimiters-mode)
; (add-hook 'c-mode-hook 'rainbow-delimiters-mode)

(load (concat EMACS-EXTERNAL-DIR "external.el")) ;; Load external functions

;;ido stuff
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; syntax color
(global-font-lock-mode t)

;; Matlab Mode
(setq matlab-binary-file "/usr/local/bin/matlab")
(add-to-list 'load-path (concat EMACS-EXTERNAL-DIR "matlab-emacs"))
;;(require 'matlab-load)
(load-file (concat EMACS-ROOT-DIR "matlab-cell.el"))

(load-file (concat EMACS-ROOT-DIR "st-multi-mode.el"))
(require 'st-multi-mode)

;; PHP Mode
(autoload 'php-mode (concat EMACS-EXTERNAL-DIR "php-mode.el") "Enter PHP mode." t) 
(setq auto-mode-alist (cons '("\\.php\\'" . php-mode) auto-mode-alist)) 

(require 'whitespace)

;; Color Themes
(require 'color-theme)
(color-theme-initialize)
(setq custom-theme-directory (concat EMACS-EXTERNAL-DIR "themes"))
;;(color-theme-dark-blue2)
(load-theme 'subatomic t)

;; Desktop stuff
(desktop-save-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode 1)
(setq truncate-lines 0)

;; Org Stuff
(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "INCONCLUSIVE" "POSTPONED" "CANCELED" "|" "DONE")))
(setq org-todo-keyword-faces
      '(
        ("TODO"  . (:foreground "firebrick2" :weight bold))
	("IN PROGRESS" . (:foreground "orange" :weight bold))
        ("WAITING"  . (:foreground "olivedrab" :weight bold))
        ("INCONCLUSIVE"  . (:foreground "yellow" :weight bold))
        ("POSTPONED"  . (:foreground "sienna" :weight bold))
        ("PROJECT"  . (:foreground "steelblue" :weight bold))
        ("DONE"  . (:foreground "forestgreen" :weight bold))
        ("MAYBE"  . (:foreground "dimgrey" :weight bold))
        ("CANCELED"  . shadow)
        ))

(setq org-log-done 'time)

(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-t") 'org-delete-backward-char)))

;; Folding stuff
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

(add-hook 'matlab-mode-hook 'turn-off-auto-fill)
(add-hook 'matlab-mode-hook '(lambda () (setq truncate-lines t)))
(add-hook 'matlab-mode-hook '(lambda () (define-key matlab-mode-map (kbd "C-c C-e") 'matlab-cell-run-current-cell)))

(st-ttc)
(switch-to-buffer "*Tao Te Ching*")

(put 'narrow-to-region 'disabled nil)

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))

(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(defun rotate-windows-helper(x d)
  (if (equal (cdr x) nil) (set-window-buffer (car x) d)
    (set-window-buffer (car x) (window-buffer (cadr x))) (rotate-windows-helper (cdr x) d)))
 
(defun rotate-windows ()
  (interactive)
  (rotate-windows-helper (window-list) (window-buffer (car (window-list))))
  (select-window (car (last (window-list)))))

(defun latex-bm-ify (var)
  "Asks for a variable inserts it at point within a \bm{ }."
  (interactive "sVariable: ")
  (insert "\\bm{")
  (insert var)
  (insert "}"))

;; Latex mode stuff
(defun st-define-latex-mode-keys ()
  "Hooks for latex mode."
  ;; Insert a Frame (for beamer)
  (define-key latex-mode-map (kbd "C-; C-i C-f") 
    '(lambda () 
       (interactive) 
       (setq start (point))
       (insert "\\begin{frame}{}\n\\end{frame}") 
       (setq end (point))
       (goto-char (- (point) 13))
       (indent-region start end)
       ))
  
;; Insert an itemize 
  (define-key latex-mode-map (kbd "C-; C-i C-i") 
    '(lambda () 
       (interactive) 
       (setq start (point))
       (insert "\\begin{itemize}\n\\item \n\\end{itemize}") 
       (setq end (point))
       (goto-char (- (point) 14))
       (indent-region start end)
       ))
  
  ;; Insert an equation
  (define-key latex-mode-map (kbd "C-; C-i C-q") 
    '(lambda () 
       (interactive) 
       (setq start (point))
       (insert "\n\\[\n\n\\]") 
       (setq end (point))
       (goto-char (- (point) 3))
       (indent-region start end)
       ))

  ;; Insert an equation array (split equation)
  (define-key latex-mode-map (kbd "C-; C-i C-s C-q") 
    '(lambda () 
       (interactive) 
       (setq start (point))
       (insert "\n\\begin{equation*}\n\\begin{split}\n\n\\end{split}\n\\end{equation*}") 
       (setq end (point))
       (goto-char (- (point) 28))
       (indent-region start end)
       )))

(add-hook 'latex-mode-hook 'st-define-latex-mode-keys)
; Other stuff
(global-set-key (kbd "C-z") nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Helper for compilation. Close the compilation window if
  ;; there was no error at all.
  (defun compilation-exit-autoclose (status code msg)
    ;; If M-x compile exists with a 0
    (when (and (eq status 'exit) (zerop code))
      ;; then bury the *compilation* buffer, so that C-x b doesn't go there
      (bury-buffer)
      ;; and delete the *compilation* window
      (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    ;; Always return the anticipated result of compilation-exit-message-function
    (cons msg code))
  ;; Specify my function (maybe I should have done a lambda function)
  (setq compilation-exit-message-function 'compilation-exit-autoclose)

(add-hook 'html-mode-hook
 (lambda ()
   (local-set-key (kbd "C-RET") 'html-list-item)
 )
)

(load (concat EMACS-ROOT-DIR "my-emacs-abbrev.el"))

(add-hook 'matlab-mode-hook '(lambda () (progn (mlint-minor-mode t) (matlab-toggle-show-mlint-warnings))))

;; Add some additional faces for org mode, especial useful for the log
(make-face 'font-lock-special-macro-face)
(set-face-foreground 'font-lock-special-macro-face "white")
(set-face-background 'font-lock-special-macro-face "dodgerblue4")
(set-face-bold-p 'font-lock-special-macro-face t)

(make-face 'font-lock-special-macro-face2)
(set-face-foreground 'font-lock-special-macro-face2 "white")
(set-face-background 'font-lock-special-macro-face2 "dodgerblue3")
(set-face-bold-p 'font-lock-special-macro-face2 t)

(make-face 'font-lock-special-text-tag-face)
(set-face-foreground 'font-lock-special-text-tag-face "white")
(set-face-background 'font-lock-special-text-tag-face "#33A1C9")
(set-face-bold-p 'font-lock-special-text-tag-face t)

(make-face           'font-lock-special-inline-math-face)
(set-face-foreground 'font-lock-special-inline-math-face "lavender")
(set-face-background 'font-lock-special-inline-math-face "steelblue")
(set-face-bold-p     'font-lock-special-inline-math-face nil)

(make-face           'font-lock-special-display-math-face)
(set-face-foreground 'font-lock-special-display-math-face "#98F5FF")
(set-face-background 'font-lock-special-display-math-face "#00688B")
(set-face-bold-p     'font-lock-special-display-math-face nil)

(make-face           'font-lock-special-math-block-face)
(set-face-foreground 'font-lock-special-math-block-face "gray90")
(set-face-background 'font-lock-special-math-block-face "dodgerblue4")
(set-face-bold-p     'font-lock-special-math-block-face nil)

(make-face           'font-lock-special-fixme-face)
(set-face-foreground 'font-lock-special-fixme-face "white")
(set-face-background 'font-lock-special-fixme-face "red")
(set-face-bold-p     'font-lock-special-fixme-face t)

(make-face           'font-lock-special-at-tag-face)
(set-face-foreground 'font-lock-special-at-tag-face "lightyellow1")
(set-face-background 'font-lock-special-at-tag-face "lightyellow4")
(set-face-bold-p     'font-lock-special-at-tag-face t)

(make-face           'font-lock-special-math-tag-face)
(set-face-foreground 'font-lock-special-math-tag-face "#435b9a")
(set-face-bold-p     'font-lock-special-math-tag-face nil)

(make-face           'font-lock-special-verified-face)
(set-face-foreground 'font-lock-special-verified-face "white")
(set-face-background 'font-lock-special-verified-face "darkgreen")
(set-face-bold-p     'font-lock-special-verified-face t)

(make-face           'font-lock-special-verified-file-face)
(set-face-foreground 'font-lock-special-verified-file-face "lightgreen")
(set-face-background 'font-lock-special-verified-file-face "green4")
(set-face-bold-p     'font-lock-special-verified-file-face t)

(make-face           'font-lock-special-probation-face)
(set-face-foreground 'font-lock-special-probation-face "white")
(set-face-background 'font-lock-special-probation-face "orange")
(set-face-bold-p     'font-lock-special-probation-face t)

(make-face           'font-lock-special-unverified-face)
(set-face-foreground 'font-lock-special-unverified-face "white")
(set-face-background 'font-lock-special-unverified-face "red")
(set-face-bold-p     'font-lock-special-unverified-face t)

(make-face           'font-lock-special-previous-face)
(set-face-foreground 'font-lock-special-previous-face "magenta")
(set-face-background 'font-lock-special-previous-face "#4B0082")
(set-face-bold-p     'font-lock-special-previous-face t)

(make-face           'font-lock-special-plot-face)
(set-face-foreground 'font-lock-special-plot-face "magenta")
(set-face-background 'font-lock-special-plot-face "#4B0082")
(set-face-bold-p     'font-lock-special-plot-face t)

(make-face           'font-lock-special-plot-file-face)
(set-face-foreground 'font-lock-special-plot-file-face "magenta")
(set-face-background 'font-lock-special-plot-file-face "#6B20A2")
(set-face-bold-p     'font-lock-special-plot-file-face nil)

(make-face           'font-lock-special-plot-file-face)
(set-face-foreground 'font-lock-special-plot-file-face "magenta")
(set-face-background 'font-lock-special-plot-file-face "#6B20A2")
(set-face-bold-p     'font-lock-special-plot-file-face nil)

(make-face           'font-lock-label-tag-face)
(set-face-foreground 'font-lock-label-tag-face "#435b2a")
(set-face-bold-p     'font-lock-label-tag-face nil)

(make-face           'font-lock-label-tag-content-face)
(set-face-foreground 'font-lock-label-tag-content-face "black")
(set-face-background 'font-lock-label-tag-content-face "yellow")
(set-face-bold-p     'font-lock-label-tag-content-face t)

(make-face           'font-lock-ref-tag-face)
(set-face-foreground 'font-lock-ref-tag-face "#435b2a")
(set-face-bold-p     'font-lock-ref-tag-face nil)

(make-face           'font-lock-ref-tag-content-face)
(set-face-foreground 'font-lock-ref-tag-content-face "yellow")
(set-face-bold-p     'font-lock-ref-tag-content-face t)
(set-face-underline-p     'font-lock-ref-tag-content-face t)

(defun add-custom-keyw()
  "adds a few special keywords for c and c++ modes"
  ;
  (font-lock-add-keywords nil
   '(
     ("R\\(EGREP\\)"  0 'font-lock-special-macro-face2)
     ("\\(MACRO\\)"  0 'font-lock-special-macro-face )
     ("\\(PREVIOUS\\)"  0 'font-lock-special-previous-face )
     ("\\(NEXT\\)"  0 'font-lock-special-previous-face )     
     ("\\(@[A-Z]*PLOT\\)"  0 'font-lock-special-plot-face )
     ("@[A-Z]*PLOT\\([ ]+[^[:space:]]+\\)"  1 'font-lock-special-plot-file-face )
     ("\\(@SCAN\\)"  0 'font-lock-special-plot-face )
     ("@SCAN\\([ ]+[^[:space:]]+\\)"  1 'font-lock-special-plot-file-face )
     ; more of those would go here
     ("\\(@VERIFIED\\)"  0 'font-lock-special-verified-face )
     ("\\(@PROBATION\\)"  0 'font-lock-special-probation-face )
     ("@VERIFIED\\([ ]+[A-Za-z].*\\)"  1 'font-lock-special-verified-file-face )
     ("\\(@UNVERIFIED\\)"  0 'font-lock-special-unverified-face )
     ("\\<\\(@[A-Z][A-Z|_]+\\)" 0 'font-lock-special-text-tag-face)
     ; ("@{\\([^$]+\\)@}" 1 'font-lock-special-display-math-face)
     ("\\(\\\\label{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-label-tag-face)
     ("\\(\\\\label{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-label-tag-face)
     ("\\(\\\\label{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-label-tag-content-face)
     ("[^$]\\$\\([^$]+\\)\\$" 1 'font-lock-special-inline-math-face)
     ("\\$\\$\\([^$]+\\)\\$\\$" 1 'font-lock-special-display-math-face)
     ("\\(.*&.*\\)" 0 'font-lock-special-math-block-face)
     ("\\(%[^%]+%\\)" 0 'font-lock-special-fixme-face)
     ("\\(@#\\)\\([^#]+\\)\\(#@\\)" 1 'font-lock-ref-tag-face)
     ("\\(@#\\)\\([^#]+\\)\\(#@\\)" 2 'font-lock-ref-tag-content-face)
     ("\\(@#\\)\\([^#]+\\)\\(#@\\)" 3 'font-lock-ref-tag-face)
     ("\\(@{+#?\\)" 0 'font-lock-special-at-tag-face)
     ("\\(@}+#?\\)" 0 'font-lock-special-at-tag-face)
     ("\\($+\\)" 0 'font-lock-special-math-tag-face)
     )
   )
 )
(add-hook 'org-mode-hook 'add-custom-keyw)

(make-face             'font-lock-tex-dim-face)
(set-face-foreground   'font-lock-tex-dim-face "gray40")
(set-face-bold-p       'font-lock-tex-dim-face nil)
(set-face-underline-p  'font-lock-tex-dim-face nil)
(set-face-attribute    'font-lock-tex-dim-face nil)

(make-face             'font-lock-special-tex-section-face)
(set-face-foreground   'font-lock-special-tex-section-face "white")
(set-face-background   'font-lock-special-tex-section-face "blue")
(set-face-bold-p       'font-lock-special-tex-section-face t)
(set-face-attribute    'font-lock-special-tex-section-face nil :height 2.0)

(make-face             'font-lock-special-tex-subsection-face)
(set-face-foreground   'font-lock-special-tex-subsection-face "white")
(set-face-background   'font-lock-special-tex-subsection-face "dodgerblue")
(set-face-bold-p       'font-lock-special-tex-subsection-face t)
(set-face-attribute    'font-lock-special-tex-subsection-face nil :height 1.6)

(make-face             'font-lock-special-tex-subsubsection-face)
(set-face-foreground   'font-lock-special-tex-subsubsection-face "white")
(set-face-background   'font-lock-special-tex-subsubsection-face "cornflower blue")
(set-face-bold-p       'font-lock-special-tex-subsubsection-face t)
(set-face-attribute    'font-lock-special-tex-subsubsection-face nil :height 1.4)

(make-face             'font-lock-special-tex-subsubsubsection-face)
(set-face-foreground   'font-lock-special-tex-subsubsubsection-face "white")
(set-face-background   'font-lock-special-tex-subsubsubsection-face "salmon")
(set-face-bold-p       'font-lock-special-tex-subsubsubsection-face t)
(set-face-attribute    'font-lock-special-tex-subsubsubsection-face nil :height 1.2)

(make-face             'font-lock-tex-todo-face)
(set-face-foreground   'font-lock-tex-todo-face "white")
(set-face-background   'font-lock-tex-todo-face "red")
(set-face-bold-p       'font-lock-tex-todo-face t)
(set-face-attribute    'font-lock-tex-todo-face nil :height 1.2)

(make-face             'font-lock-tex-done-face)
(set-face-foreground   'font-lock-tex-done-face "white")
(set-face-background   'font-lock-tex-done-face "green")
(set-face-bold-p       'font-lock-tex-done-face t)
(set-face-attribute    'font-lock-tex-done-face nil :height 1.2)

(defun add-custom-tex-keyw()
  "adds a few special keywords for tex modes"
  ;
  (font-lock-add-keywords nil
   '(
     ("\\([\\]section[\*]?{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-tex-dim-face)
     ("\\([\\]section[\*]?{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-special-tex-section-face)
     ("\\([\\]section[\*]?{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-tex-dim-face)
     ("\\([\\]subsection[\*]?{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-tex-dim-face)
     ("\\([\\]subsection[\*]?{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-special-tex-subsection-face)
     ("\\([\\]subsection[\*]?{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-tex-dim-face)
     ("\\([\\]subsubsection[\*]?{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-tex-dim-face)
     ("\\([\\]subsubsection[\*]?{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-special-tex-subsubsection-face)
     ("\\([\\]subsubsection[\*]?{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-tex-dim-face)
     ("\\([\\]todo{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-tex-dim-face)
     ("\\([\\]todo{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-tex-todo-face)
     ("\\([\\]todo{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-tex-dim-face)
     ("\\([\\]done{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-tex-dim-face)
     ("\\([\\]done{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-tex-done-face)
     ("\\([\\]done{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-tex-dim-face)
     )
   )
 )
(add-hook 'tex-mode-hook 'add-custom-tex-keyw)

(defun st-log-highlight ()
  "Puts highlight tags around the current region."
  (interactive)  
  (let* ((reg-start (region-beginning)) (reg-end (region-end)) (str (buffer-substring reg-start reg-end)))
    (goto-char reg-start)
    (insert "@^")
    (goto-char (+ reg-end 2))
    (insert "^@")))


(defun st-org-delete-char ()
  "Deletes a character. If at the end of the buffer, moves back one before deleting the character."
  (interactive)
  (if (< (point) (- (point-max) 1))
      (org-delete-char 1)
    (org-delete-char -1)))
      
(defun st-list-buffers-select ()
  "Lists all buffers visiting files and sets focus to that window, to allow easy selection."
  (interactive)
  (ibuffer)
  (hl-line-mode t))

(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun set-exec-path-for-mac-manually ()
  (interactive)
  (setq path-string "/Users/stootoon/anaconda/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Libary/TeX/texbin")
  (setenv "PATH" path-string)
  (setq exec-path (split-string path-string path-separator)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; (set-exec-path-for-mac-manually)

(defun with-silent-modifications (&rest body)
  t)

;;;;;;; KEY BINDINGS ;
;; Personal stuff
(global-set-key (kbd "C-; C-k") 'compile-current-buffer)
(global-set-key (kbd "C-; ;") 'recentf-open-files)
(global-set-key (kbd "C-x C-B") 'st-list-buffers-select)
(global-set-key (kbd "C-; C-d") 'fill-paragraph)
(global-set-key (kbd "C-; C-c") 'st-char-count)
(global-set-key (kbd "C-; C-w") 'st-word-count)
(global-set-key (kbd "C-; C-q") 'st-ttc)
(global-set-key (kbd "C-; C-m") 'st-matlab)
(global-set-key (kbd "C-; C-o") 'st-octave)
(global-set-key (kbd "C-; C-a") 'ffap)
(global-set-key (kbd "C-; C-j") 'st-julia)
(global-set-key (kbd "C-; C-p") 'st-python)
(global-set-key (kbd "C-; C-r") 'st-R)
(global-set-key (kbd "C-; C-u") 'git-push)
(global-set-key (kbd "C-; C-v") 'st-matlab-shell-list-variables)
(global-set-key (kbd "C-; C-f") 'matlab-shell-describe-command)
(global-set-key (kbd "C-; C-b") 'st-switch-to-other-buffer)
(global-set-key (kbd "C-; C-s") '(lambda () (interactive) (st-switch-to-buffer-by-name "*shell*")))
(global-set-key (kbd "C-; C-;") 'st-next-matlab-buffer)
(global-set-key (kbd "C-; C-'") 'st-previous-matlab-buffer)
(global-set-key (kbd "C-; C-t") '(lambda () (interactive) (dired ".")))
(global-set-key (kbd "C-' C-'") 'recompile)
(global-set-key (kbd "C-# C-b") '(lambda () (interactive) (st-latex-bracket-region "textbf")))
(global-set-key (kbd "C-# C-i") '(lambda () (interactive) (st-latex-bracket-region "emph")))
(global-set-key (kbd "C-; b") 'latex-bm-ify)
;; (global-set-key (kbd "C-; C-r") 'rotate-windows) 
(global-set-key (kbd "C-; C-l") 'toggle-truncate-lines)
(global-set-key (kbd "C-; C-t") 'org-delete-backward-char)
(global-set-key (kbd "C-; C-i") '(lambda () (interactive) (insert "_")))
(global-set-key (kbd "C-; C-o") '(lambda () (interactive) (insert "-")))
(global-set-key (kbd "C-; C-S-n") '(lambda () (interactive) (insert (buffer-name))))
;; for GDB/debugging in general
(global-set-key (kbd "<f5>") 'gud-cont)
(global-set-key (kbd "<f1>") 'gud-step);; equiv matlab step in
(global-set-key (kbd "<f2>") 'gud-next) ;; equiv matlab step 1 
(global-set-key (kbd "<f9>") 'gud-finish) ;; equiv matlab step out
;; Greek
(global-set-key (kbd "C-; C-g a") '(lambda () (interactive) (insert "\\alpha"))) 
(global-set-key (kbd "C-; C-g b") '(lambda () (interactive) (insert "\\beta"))) 
(global-set-key (kbd "C-; C-g g") '(lambda () (interactive) (insert "\\gamma"))) 
(global-set-key (kbd "C-; C-g d") '(lambda () (interactive) (insert "\\delta"))) 
(global-set-key (kbd "C-; C-g e") '(lambda () (interactive) (insert "\\epsilon"))) 
(global-set-key (kbd "C-; C-g z") '(lambda () (interactive) (insert "\\zeta"))) 
(global-set-key (kbd "C-; C-g h") '(lambda () (interactive) (insert "\\eta"))) 
(global-set-key (kbd "C-; C-g q") '(lambda () (interactive) (insert "\\theta"))) 
(global-set-key (kbd "C-; C-g i") '(lambda () (interactive) (insert "\\iota"))) 
(global-set-key (kbd "C-; C-g k") '(lambda () (interactive) (insert "\\kappa"))) 
(global-set-key (kbd "C-; C-g l") '(lambda () (interactive) (insert "\\lambda"))) 
(global-set-key (kbd "C-; C-g m") '(lambda () (interactive) (insert "\\mu"))) 
(global-set-key (kbd "C-; C-g n") '(lambda () (interactive) (insert "\\nu"))) 
(global-set-key (kbd "C-; C-g x") '(lambda () (interactive) (insert "\\xi"))) 
(global-set-key (kbd "C-; C-g o") '(lambda () (interactive) (insert "\\omicron"))) 
(global-set-key (kbd "C-; C-g p") '(lambda () (interactive) (insert "\\pi"))) 
(global-set-key (kbd "C-; C-g r") '(lambda () (interactive) (insert "\\rho"))) 
(global-set-key (kbd "C-; C-g s") '(lambda () (interactive) (insert "\\sigma"))) 
(global-set-key (kbd "C-; C-g t") '(lambda () (interactive) (insert "\\tau"))) 
(global-set-key (kbd "C-; C-g y") '(lambda () (interactive) (insert "\\upsilon"))) 
(global-set-key (kbd "C-; C-g f") '(lambda () (interactive) (insert "\\phi"))) 
(global-set-key (kbd "C-; C-g c") '(lambda () (interactive) (insert "\\xi"))) 
(global-set-key (kbd "C-; C-g w") '(lambda () (interactive) (insert "\\psi"))) 
(global-set-key (kbd "C-; C-g o") '(lambda () (interactive) (insert "\\omega"))) 
;;;Umlaute
(global-unset-key (kbd "M-u"))
(global-set-key (kbd "M-u a") '(lambda() (interactive) (insert "ä")))
(global-set-key (kbd "M-u A") '(lambda() (interactive) (insert "Ä")))
(global-set-key (kbd "M-u o") '(lambda() (interactive) (insert "ö")))
(global-set-key (kbd "M-u O") '(lambda() (interactive) (insert "Ö")))
(global-set-key (kbd "M-u u") '(lambda() (interactive) (insert "ü")))
(global-set-key (kbd "M-u U") '(lambda() (interactive) (insert "Ü")))

;;; Window numbering
(global-set-key (kbd "s-0") 'select-window-0)
(global-set-key (kbd "s-1") 'select-window-1)
(global-set-key (kbd "s-2") 'select-window-2)
(global-set-key (kbd "s-3") 'select-window-3)
(global-set-key (kbd "s-4") 'select-window-4)
(global-set-key (kbd "s-5") 'select-window-5)
(global-set-key (kbd "s-6") 'select-window-6)
(global-set-key (kbd "s-7") 'select-window-7)
(global-set-key (kbd "s-8") 'select-window-8)
(global-set-key (kbd "s-9") 'select-window-9)

;; Colors for rainbow-delimiters mode.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :foreground "MediumPurple1" :slant normal :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#d3bdff" :slant normal :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "thistle1" :height 1.2))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "SkyBlue1"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "magenta1"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "green1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "Yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "SeaGreen1")))))

(add-to-list 'auto-mode-alist '("\\.jsx$" . javascript-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

(setq web-mode-content-types-alist
  '(("jsx"  . "\\.js[x]?\\'")))

;; Python indentation
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(matlab-auto-fill nil)
 '(matlab-mode-hook
   (quote
    (matlab-cell-initialize
     (lambda nil
       (define-key matlab-mode-map
	 (kbd "C-c C-e")
	 (quote matlab-cell-run-current-cell)))
     (lambda nil
       (setq truncate-lines t))
     turn-off-auto-fill)) t)
 '(matlab-shell-command "/usr/local/bin/matlab")
 '(mlint-programs
   (quote
    ("mlint" "/misc/apps/matlab/matlabR2010a/bin/glnxa64/mlint")))
 '(ns-pop-up-frames nil)
 '(truncate-lines t)
 '(visible-bell t))

;; So anaconda doesn't complain.
(when (executable-find "python")
  (setq python-shell-interpreter "pythonw"))

;; Jedi
;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)                 ; optional
