;; .emacs
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                        ;("marmalade" . "http://marmalade-repo.org/packages/")
			("melpa-stable" . "https://stable.melpa.org/packages/")
                        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;package-install git, git-blame, matlab-mode
;;git clone matlab-emacs into ~/git/External
(setq inhibit-splash-screen t)
(setq load-path (cons EMACS-ROOT-DIR load-path))
(setq load-path (cons EMACS-EXTERNAL-DIR load-path))

(load (concat EMACS-ROOT-DIR "emacs-functions.el"))
(message "Loaded emacs-functions.")

;; ;; (global-hl-line-mode 1)

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

(add-hook 'json-mode-hook 'st-reformat-json-buffer)

;; ;(display-line-numbers-mode 1) ;Turn on marginal line numbres

(require 'window-numbering)
(window-numbering-mode) ;; Turn on window numbers.

(add-hook 'doc-view-mode-hook 'auto-revert-mode) ;; Refresh pdfs automatically

;; Recent files mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; rainbow-colors mode
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(load (concat EMACS-EXTERNAL-DIR "external.el")) ;; Load external functions

;;ido stuff
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; syntax color
(global-font-lock-mode t)

(require 'whitespace)

;; Color Themes
(require 'color-theme)
(color-theme-initialize)
(setq custom-theme-directory (concat EMACS-EXTERNAL-DIR "themes"))
(load-theme 'subatomic t)
;; cell color in notebooks :#384054 

;; ;; git
;; (add-to-list 'load-path GIT-EMACS-PATH)
;; (require 'git)
;; (require 'git-blame)
;; (require 'git-emacs)

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

;; (defun split-compilation-window-hook () 
;;     "Make sure that the compile window is splitting vertically"
;;     (progn
;;       (if (not (get-buffer-window "*compilation*"))
;;          (progn
;; 	    (split-window-horizontally)
;; 	    )
;; 	  )
;;       )
;; 	)

;; (add-hook 'compilation-mode-hook 'split-compilation-window-hook)


(add-hook 'html-mode-hook
 (lambda ()
   (local-set-key (kbd "C-RET") 'html-list-item)
 )
)

(load (concat EMACS-ROOT-DIR "my-emacs-abbrev.el"))

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

(make-face           'font-lock-highlight-yellow-face)
(set-face-foreground 'font-lock-highlight-yellow-face "black")
(set-face-background 'font-lock-highlight-yellow-face "yellow")
(set-face-bold-p     'font-lock-highlight-yellow-face t)


(defun add-custom-keyw()
  "adds a few special keywords for c and c++ modes"
									  ;
  (font-lock-add-keywords nil
   '(
     ;; ("R\\(EGREP\\)"  0 'font-lock-special-macro-face2)
     ;; ("\\(MACRO\\)"  0 'font-lock-special-macro-face )
     ;; ("\\(PREVIOUS\\)"  0 'font-lock-special-previous-face )
     ;; ("\\(NEXT\\)"  0 'font-lock-special-previous-face )     
     ;; ("\\(@[A-Z]*PLOT\\)"  0 'font-lock-special-plot-face )
     ;; ("@[A-Z]*PLOT\\([ ]+[^[:space:]]+\\)"  1 'font-lock-special-plot-file-face )
     ;; ("\\(@SCAN\\)"  0 'font-lock-special-plot-face )
     ;; ("@SCAN\\([ ]+[^[:space:]]+\\)"  1 'font-lock-special-plot-file-face )
     ;; ; more of those would go here
     ;; ("\\(@VERIFIED\\)"  0 'font-lock-special-verified-face )
     ;; ("\\(@PROBATION\\)"  0 'font-lock-special-probation-face )
     ;; ("@VERIFIED\\([ ]+[A-Za-z].*\\)"  1 'font-lock-special-verified-file-face )
     ;; ("\\(@UNVERIFIED\\)"  0 'font-lock-special-unverified-face )
     ;; ("\\<\\(@[A-Z][A-Z|_]+\\)" 0 'font-lock-special-text-tag-face)
     ;; ; ("@{\\([^$]+\\)@}" 1 'font-lock-special-display-math-face)
     ;; ("\\(\\\\label{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-label-tag-face)
     ;; ("\\(\\\\label{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-label-tag-face)
     ;; ("\\(\\\\label{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-label-tag-content-face)
     ;; ("[^$]\\$\\([^$]+\\)\\$" 1 'font-lock-special-inline-math-face)
     ;; ("\\$\\$\\([^$]+\\)\\$\\$" 1 'font-lock-special-display-math-face)
     ;; ("\\(.*&.*\\)" 0 'font-lock-special-math-block-face)
     ;; ("\\(%[^%]+%\\)" 0 'font-lock-special-fixme-face)
     ;; ("\\(@#\\)\\([^#]+\\)\\(#@\\)" 1 'font-lock-ref-tag-face)
     ;; ("\\(@#\\)\\([^#]+\\)\\(#@\\)" 2 'font-lock-ref-tag-content-face)
     ;; ("\\(@#\\)\\([^#]+\\)\\(#@\\)" 3 'font-lock-ref-tag-face)
     ;; ("\\(@{+#?\\)" 0 'font-lock-special-at-tag-face)
     ;; ("\\(@}+#?\\)" 0 'font-lock-special-at-tag-face)
     ;; ("\\($+\\)" 0 'font-lock-special-math-tag-face)
     ;; ("\\(@\\^\\)" 0 'font-lock-special-math-tag-face)
     ;; ("\\(\\^@\\)" 0 'font-lock-special-math-tag-face)	 	 
     ("@\\^\\([^\\^]+\\)\\^@" 1 'font-lock-highlight-yellow-face)
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

(make-face             'font-lock-st-tex-comment-face)
(set-face-foreground   'font-lock-st-tex-comment-face "gray10")
(set-face-background   'font-lock-st-tex-comment-face "gray50")
(set-face-bold-p       'font-lock-st-tex-comment-face t)
(set-face-underline-p  'font-lock-st-tex-comment-face nil)
(set-face-italic-p     'font-lock-st-tex-comment-face t)

(make-face             'font-lock-st-tex-insert-face)
(set-face-foreground   'font-lock-st-tex-insert-face "black")
(set-face-background   'font-lock-st-tex-insert-face "yellow")
(set-face-bold-p       'font-lock-st-tex-insert-face nil)
(set-face-underline-p  'font-lock-st-tex-insert-face nil)

(make-face             'font-lock-st-tex-highlight-face)
(set-face-foreground   'font-lock-st-tex-highlight-face "black")
(set-face-background   'font-lock-st-tex-highlight-face "yellow")
(set-face-bold-p       'font-lock-st-tex-highlight-face nil)
(set-face-underline-p  'font-lock-st-tex-highlight-face nil)

(make-face             'font-lock-st-tex-delete-face)
(set-face-foreground   'font-lock-st-tex-delete-face "white")
(set-face-background   'font-lock-st-tex-delete-face "gray80")
(set-face-bold-p       'font-lock-st-tex-delete-face nil)
(set-face-attribute    'font-lock-st-tex-delete-face nil :strike-through "red")


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

     ("\\([\\]pel{\\)\\([^}]+\\)\\(}\\)"  2 'font-lock-st-tex-insert-face)
     ("\\([\\]peld{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-st-tex-delete-face)
     ("\\([\\]pelc{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-st-tex-comment-face)
     ("\\([\\]hl{\\)\\([^}]+\\)\\(}\\)" 2 'font-lock-st-tex-highlight-face)     

     ("\\([\\]pel{\\)\\([^}]+\\)\\(}\\)"  1 'font-lock-tex-dim-face)
     ("\\([\\]peld{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-tex-dim-face)
     ("\\([\\]pelc{\\)\\([^}]+\\)\\(}\\)" 1 'font-lock-tex-dim-face)
     ("\\([\\]hl{\\)\\([^}]+\\)\\(}\\)"  1 'font-lock-tex-dim-face)          

     ("\\([\\]pel{\\)\\([^}]+\\)\\(}\\)"  3 'font-lock-tex-dim-face)
     ("\\([\\]peld{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-tex-dim-face)
     ("\\([\\]pelc{\\)\\([^}]+\\)\\(}\\)" 3 'font-lock-tex-dim-face)
     ("\\([\\]hl{\\)\\([^}]+\\)\\(}\\)"  3 'font-lock-tex-dim-face)               
     
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

(defun with-silent-modifications (&rest body)
  t)

;;;;;;; KEY BINDINGS ;
;; Personal stuff
(global-set-key (kbd "C-; C-k") 'st-compile-current-buffer)
(global-set-key (kbd "C-; ;") 'recentf-open-files)
(global-set-key (kbd "C-x C-B") 'st-list-buffers-select)
(global-set-key (kbd "C-; C-d") 'fill-paragraph)
(global-set-key (kbd "C-; C-c") 'st-char-count)
(global-set-key (kbd "C-; C-w") 'st-word-count)
(global-set-key (kbd "C-; C-q") 'st-ttc)
(global-set-key (kbd "C-; C-a") 'ffap)
(global-set-key (kbd "C-; C-p") 'st-python)
(global-set-key (kbd "C-; C-u") 'git-push)
(global-set-key (kbd "C-; C-b") 'st-switch-to-other-buffer)
(global-set-key (kbd "C-; C-s") '(lambda () (interactive) (st-switch-to-buffer-by-name "*shell*")))
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
 '(ns-pop-up-frames nil)
 '(truncate-lines t)
 '(visible-bell t))

;; So anaconda doesn't complain.
(when (executable-find "python")
  (setq python-shell-interpreter "pythonw"))

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg2"))
(setf epa-pinentry-mode 'loopback)
(epa-file-enable)
(message "GPG2 enabled.")

(defun st-switch-to-buffer (f)
  "Switches to the buffer for file 'f' if not there. If there, switches to the previous buffer."
  (interactive)
  (if (string= (buffer-file-name) f)
      (progn
	(switch-to-buffer (other-buffer)))
    (progn
      (find-file f)
      )))
			

(defun st-set-todo-list-bindings ()  
  "Sets the bindings that open various todolists."
  (interactive)
  (progn
    (global-set-key (kbd "C-. C-1") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/oneliners.org")))
    (global-set-key (kbd "C-. C-o") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/olfaction.org")))	
    (global-set-key (kbd "C-. C-d") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/git/Emacs/dot-emacs.el")))		
    (global-set-key (kbd "C-. C-s") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/silent_mcs.org")))	
    (global-set-key (kbd "C-. C-a") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/amblr.log.org")))
    (global-set-key (kbd "C-. C-c") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/Crick/crick.org")))	
    (global-set-key (kbd "C-. C-f") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/Finances/2019.org")))
    (global-set-key (kbd "C-. C-g") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/greek.org")))    
    (global-set-key (kbd "C-. C-i") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/ideas.org")))
    (global-set-key (kbd "C-. C-m") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/math.org")))
    (global-set-key (kbd "C-. C-l") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/Locust/locust.org")))
    (global-set-key (kbd "C-; C-;") 'other-frame)    
    (global-set-key (kbd "C-. C-t") (lambda () (interactive) (st-switch-to-buffer "/Users/stootoon/Dropbox/today.org")))
    ))


(st-set-todo-list-bindings)
										; Gmsh files should open in c-mode
(add-to-list 'auto-mode-alist '("\\.geo\\'" . c-mode))


(defun st-indent (&optional n)
  "Indents the region using spaces."
  (interactive)
  (unless n
	(setq n 4))
  (if (mark)
	  (let (start end)
		(save-restriction
		  (save-excursion
			(narrow-to-region (region-beginning) (region-end))
			(goto-char 1)
			(while (re-search-forward "^" nil t)
			  (replace-match (make-string n ? ))) ;; the space after? is necessary.
			  )))))

(defun st-unindent (&optional n)
  "Unindents the region using spaces."
  (interactive)
  (unless n
	(setq n 4))
  (if (mark)
	  (let (start end)
		(save-restriction
		  (save-excursion
			(narrow-to-region (region-beginning) (region-end))
			(goto-char 1)
			(while (re-search-forward (concat "^" (make-string n ? )) nil t)
			  (replace-match "")
			  (goto-char (+ (point) n))
			  ) ;; the space after? is necessary.
			  )))))

(defun st-compile-start ()
  (interactive)
  (message "st-compile-start called.")
					;(kill-buffer "*compilation*") ;; In case it's still hanging around
  (if (get-buffer-window "*compilation*")
      (progn
	(message "Found *compilation* buffer in a window, switching to it and closing.")
	(kill-buffer "*compilation*")))
  (window-configuration-to-register 9)
  (setq st-saved-window-configuration 1)
  (message "st-compile-start finished.")
  )

(defun st-goto-endof-compilation-buffer ()
  (interactive)
  (message "st-goto-endof-compilation-buffer")
  (if (get-buffer-window "*compilation*")
      (save-selected-window
	(switch-to-buffer-other-window "*compilation*")
	(end-of-buffer))
    (message "No *compilation* buffer found.")))
;; ;(add-hook 'compile-start-hook 'st-compile-start)

(defun st-insert-tex-eq ()
  "Asks the user for the name of an equation and inserts a reference to it."
  (interactive)
  (setq ref (read-string "Which equation? "))
  (insert (concat "\\Eqn{" ref "} ")))

(defun st-compile-current-buffer ()
  (interactive)
  (save-excursion
    (st-compile-start))
  (compile-current-buffer))

(defun st-post-compile ()
  (interactive)
  (message "st-post-compile called.")
  (if st-saved-window-configuration
	(progn
	  (setq st-saved-window-configuration nil)
	  (jump-to-register 9)
	  (message "Reverted to saved window configuration."))
	(message "No saved configuration found.")))

;; Helper for compilation. Close the compilation window if
  ;; there was no error at all.
(defun st-set-compilation-status (status code msg)
  ;; If M-x compile exists with a 0
  (setq st-compilation-status status)
  (setq st-compilation-code  code)
  (setq st-compilation-msg   msg)
  (setq st-compilation-exit t)
  
  (cons msg code)
  )

;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'st-set-compilation-status)

(defun st-compilation-finish-function (buffer desc)
  (message "st-compilation-finish-function. Buffer %s: %s" buffer desc)
  (if st-compilation-exit
      (progn
	(setq st-compilation-exit nil)
	(when (and (eq st-compilation-status 'exit) (zerop st-compilation-code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
	  (bury-buffer "*compilation*")
	  ;; and delete the *compilation* window
	  (delete-window (get-buffer-window (get-buffer "*compilation*")))
	  (st-post-compile)
	  ))))

(add-hook 'compilation-finish-functions 'st-compilation-finish-function)

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(setq inferior-lisp-program "sbcl")

(defvar outline-minor-mode-prefix "\M-#")

(defun st-python-shell-send-current-line ()
 (interactive)
 (python-shell-send-region (line-beginning-position) (line-end-position))
 (forward-line))

(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%H:%M"))
  (insert " ")
  )

(global-set-key (kbd "C-. C-.") 'now)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

(require 'reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "C-; C-y") 'st-goto-endof-compilation-buffer)))

(defvar outline-minor-mode-prefix "\M-#")

(defun st-msg (msg)
  (message (concat (format-time-string "%Y/%m/%d %H:%M:%S.%3N" (current-time)) " " msg)))
(global-undo-tree-mode)    

;;(pdf-tools-install)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "ESC <up>") 'move-line-up)
(global-set-key (kbd "ESC <down>")  'move-line-down)

(defvar markdown--first-displayable-cache (make-hash-table :test #'equal))

(defun markdown--first-displayable (seq)
  "Return the first displayable character or string in SEQ.
SEQ may be an atom or a sequence."
  (let ((c (gethash seq markdown--first-displayable-cache t)))
    (if (not (eq c t))
        c
      (puthash seq
               (let ((seq (if (listp seq) seq (list seq))))
                 (cond ((stringp (car seq))
                        (cl-find-if
                         (lambda (str)
                           (and (mapcar #'char-displayable-p (string-to-list str))))
                         seq))
                       ((characterp (car seq))
                        (cl-find-if #'char-displayable-p seq))))
               markdown--first-displayable-cache))))

(defun get-word () ;; From ergoemacs
   (interactive)
  (let (
        p1
        p2
        (case-fold-search t))
    (save-excursion
      (skip-chars-backward "_a-z0-9" )
      (setq p1 (point))
      (skip-chars-forward "_a-z0-9" )
      (setq p2 (point))
      (message "%s" (buffer-substring-no-properties p1 p2)))))

(defun peld ()
  (interactive)
  (save-excursion
    (setq w1 (get-word))
    (if (equal w1 "peld")
	(save-excursion
	  (setq s1 (1- (search-forward "{")))
	  (goto-char s1)
	  (forward-sexp)
	  (setq s2 (+ (point) 1))
	  (setq s1 (- s1 5))
	  (message "Cutting PELD: %d %d %s" s1 s2 (buffer-substring-no-properties s1 s2))
	  (kill-region s1 (1- s2))
	  )
      (message "Not peld"))
    )
  )

(defun pel ()
  (interactive)
  (save-excursion
    (setq w1 (get-word))
    (if (equal w1 "pel")
	(save-excursion
	  (setq s1 (1- (search-forward "{")))
	  (goto-char s1)
	  (forward-sexp)
	  (setq s2 (+ (point) 1))
	  (setq s1 (- s1 4))
	  (message "Accepting PEL: %d %d %s" s1 s2 (buffer-substring-no-properties s1 s2))
	  (kill-region (- s2 2) (1- s2))	  
	  (kill-region s1 (+ s1 5))
	  )
      (message "Not pel"))
    )
  )

(defun st-move-line-up ()
"Moves the current line up one and keeps point with it."
(interactive)
(transpose-lines 1)
(forward-line -2))

(defun st-move-line-down ()
"Moves the current line up one and keeps point with it."
(interactive)
(forward-line 1)
(transpose-lines 1)
(forward-line -1))

(global-set-key (kbd "C-x <up>") 'st-move-line-up)
(global-set-key (kbd "C-x <down>") 'st-move-line-down)

(global-set-key (kbd "M-g a") "α")
(global-set-key (kbd "M-g b") "β")
(global-set-key (kbd "M-g g") "γ")
(global-set-key (kbd "M-g d") "δ")
(global-set-key (kbd "M-g e") "ε")
(global-set-key (kbd "M-g z") "ζ")
(global-set-key (kbd "M-g h") "η")
(global-set-key (kbd "M-g q") "θ")
(global-set-key (kbd "M-g i") "ι")
(global-set-key (kbd "M-g k") "κ")
(global-set-key (kbd "M-g l") "λ")
(global-set-key (kbd "M-g m") "μ")
(global-set-key (kbd "M-g n") "ν")
(global-set-key (kbd "M-g x") "ξ")
(global-set-key (kbd "M-g o") "ο")
(global-set-key (kbd "M-g p") "π")
(global-set-key (kbd "M-g r") "ρ")
(global-set-key (kbd "M-g s") "σ")
(global-set-key (kbd "M-g t") "τ")
(global-set-key (kbd "M-g u") "υ")
(global-set-key (kbd "M-g f") "ϕ")
(global-set-key (kbd "M-g j") "φ")
(global-set-key (kbd "M-g c") "χ")
(global-set-key (kbd "M-g y") "ψ")
(global-set-key (kbd "M-g w") "ω")
(global-set-key (kbd "M-g A") "Α")
(global-set-key (kbd "M-g B") "Β")
(global-set-key (kbd "M-g G") "Γ")
(global-set-key (kbd "M-g D") "Δ")
(global-set-key (kbd "M-g E") "Ε")
(global-set-key (kbd "M-g Z") "Ζ")
(global-set-key (kbd "M-g H") "Η")
(global-set-key (kbd "M-g Q") "Θ")
(global-set-key (kbd "M-g I") "Ι")
(global-set-key (kbd "M-g K") "Κ")
(global-set-key (kbd "M-g L") "Λ")
(global-set-key (kbd "M-g M") "Μ")
(global-set-key (kbd "M-g N") "Ν")
(global-set-key (kbd "M-g X") "Ξ")
(global-set-key (kbd "M-g O") "Ο")
(global-set-key (kbd "M-g P") "Π")
(global-set-key (kbd "M-g R") "Ρ")
(global-set-key (kbd "M-g S") "Σ")
(global-set-key (kbd "M-g T") "Τ")
(global-set-key (kbd "M-g U") "Υ")
(global-set-key (kbd "M-g F") "Φ")
(global-set-key (kbd "M-g J") "Φ")
(global-set-key (kbd "M-g C") "Χ")
(global-set-key (kbd "M-g Y") "Ψ")
(global-set-key (kbd "M-g W") "Ω")
