(defun st-word-count ()
  "Counts the number of words in the buffer and prints to the message line."
  (interactive)
  (let ((wc 0))
    (save-excursion
      (beginning-of-buffer)
      (while (< (point) (point-max))
	(forward-word)
	(setq wc (+ wc 1))
	))
    (message "%d words in buffer." wc)))

(defun st-char-count ()
  "Counts the number of characters in the buffer and prints to the message line."
  (interactive)
  (let ((cc 0))
    (save-excursion
      (beginning-of-buffer)
      (while (< (point) (point-max))
	(forward-char)
	(setq cc (+ cc 1))
	))
    (message "%d characters in buffer." cc)))

(defun st-char-count-in-region ()
  "Counts the number of characters in the region and prints to the message line."
  (interactive)
  (setq cc (- (region-end) (region-beginning)))
  (message "%d characters in region." cc))

(defun st-count-string (str)
  "Counts the number of times the specified string occurs in the buffer."
  (interactive "sSearch string: ")
  (save-excursion
    (goto-char (point-min))
    (setq n 0)
    (while (search-forward str nil t) 
      (setq n (+ n 1)))
    (message "%d instances of '%s' found." n str))
  (setq retval n))

(defun st-ttc ()
  "Writes a random quote from the Tao Te Ching to a buffer."
  (interactive)
  (setq outputBuffer (get-buffer-create "*Tao Te Ching*"))
  (setq inputBuffer (find-file-noselect (concat EMACS-ROOT-DIR "ttc.txt")))
  (set-buffer inputBuffer)
  (goto-char (point-min))
  (setq nq (st-count-string "\n\n")) ;; Count the number of quotes
  (message "%d quotes found." nq)
  (setq wq (+ (% (abs (random t)) nq) 1)) ;; Pick a random quote
  (message "Using quote %d." wq)
  ;; Now grab the quote
  (search-forward "\n\n" nil t (- wq 1))
  (setq start (point))
  (search-forward "\n\n" nil t)
  (setq end (point))
  (message "Copying range [%d-%d]" start end)
  (copy-to-buffer outputBuffer start end)
  (switch-to-buffer outputBuffer)
  )

(defun st-insert-inline-latex-wrapper ()
  "Inserts $$ at the point and positions point to insert latex code."
  (interactive)
  (insert "$$")
  (backward-char))

(defun st-insert-inverse ()
  "Inserts ^{-1} at the point."
  (interactive)
  (insert "^{-1}"))

(defun st-insert-subscript ()
  "Inserts _{} at the point."
  (interactive)
  (insert "_{}")
  (backward-char))

(defun st-insert-superscript ()
  "Inserts ^{} at the point."
  (interactive)
  (insert "^{}")
  (backward-char))

(defun cleanup-string-for-xml (str)
  "Replaces various symbols in STR for correct XML rendering."
  (setq str (replace-regexp-in-string ">" "&gt;" str))
  (setq str (replace-regexp-in-string "<" "&lt;" str)))

(defun export-clozes-in-region-to-mnemosyne-xml (&optional latex)
  "Exports the clozes found in the current region to an XML file ready for import to Mnemosyne. If latex is non-nil, modifies cloze to be latex friendly."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((body nil) (clozes nil) (cloze nil) (question nil) (answer nil) (item nil))
	(narrow-to-region (point) (mark))
	(goto-char (point-min))
	(while (re-search-forward "\\[[^]]+\\]" nil t)
	  (let ((cloze-end (point)) (cloze-start (re-search-backward "\\[" nil nil)))
	    (setq item (list (buffer-substring (point-min) cloze-start) (buffer-substring cloze-start cloze-end) (buffer-substring cloze-end (point-max))))
	    (setq clozes (push item clozes))
	    (goto-char (+ cloze-end 1))))
	(print clozes)
					; Now write out the clozes to file
	(with-temp-buffer
	  (insert "<?xml version='1.0' encoding='utf-8'?>\n")
	  (insert "<mnemosyne core_version='1'>\n")
	  (dolist (item clozes)
	    (let ((head nil) (middle nil) (tail nil))
	      (setq head   (replace-regexp-in-string "\\]" "" (replace-regexp-in-string "\\[" "" (nth 0 item))))
	      (setq middle (replace-regexp-in-string "\\]" "" (replace-regexp-in-string "\\[" "" (nth 1 item))))
	      (setq tail   (replace-regexp-in-string "\\]" "" (replace-regexp-in-string "\\[" "" (nth 2 item))))
	      (if latex
		  (progn
		    (setq question (format "%s \\;[...]\\; %s" head tail))
		    (setq answer   (format "<$$>%s</$$>" middle)))
		(prog
		    (setq question (format "%s &lt;b&gt;&lt;font color='red'&gt;[...]&lt;/font&gt;&lt;/b&gt; %s" head tail))
		    (setq answer   (format "&lt;b&gt;&lt;font color='red'&gt;%s&lt;/font&gt;&lt;/b&gt;" middle))))
	      (insert (format "<item>\n<Q>%s</Q>\n<A>%s</A>\n</item>\n" (cleanup-string-for-xml question) (cleanup-string-for-xml answer)))
	      ))
	  (insert "</mnemosyne>\n")
	  (write-file (format "mnemClozes%s.xml" (format-time-string "%Y%m%d%H%M%S")))
					;(write-file "mnemClozes.xml")
	  )
	))))

(defun export-latex-clozes-in-region-to-mnemosyne-xml ()
  "Exports the clozes found in the current region to an XML file ready for import to Mnemosyne as latex friendly."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((body nil) (clozes nil) (cloze nil) (question nil) (answer nil) (item nil))
	(narrow-to-region (point) (mark))
	(goto-char (point-min))
	(while (re-search-forward "\\[[^]]+\\]" nil t)
	  (let ((cloze-end (point)) (cloze-start (re-search-backward "\\[" nil nil)))
	    (setq item (list (buffer-substring (point-min) cloze-start) (buffer-substring cloze-start cloze-end) (buffer-substring cloze-end (point-max))))
	    (setq clozes (push item clozes))
	    (goto-char (+ cloze-end 1))))
	(print clozes)
					; Now write out the clozes to file
	(with-temp-buffer
	  (insert "<?xml version='1.0' encoding='utf-8'?>\n")
	  (insert "<mnemosyne core_version='1'>\n")
	  (dolist (item clozes)
	    (let ((head nil) (middle nil) (tail nil))
	      (setq head   (replace-regexp-in-string "\\]" "" (replace-regexp-in-string "\\[" "" (nth 0 item))))
	      (setq middle (replace-regexp-in-string "\\]" "" (replace-regexp-in-string "\\[" "" (nth 1 item))))
	      (setq tail   (replace-regexp-in-string "\\]" "" (replace-regexp-in-string "\\[" "" (nth 2 item))))
	      (setq question (format "%s \\;[...]\\; %s" head tail))
	      (setq answer   (format "<$$>%s</$$>" middle))
	      (insert (format "<item>\n<Q>%s</Q>\n<A>%s</A>\n</item>\n" (cleanup-string-for-xml question) (cleanup-string-for-xml answer)))
	      ))
	  (insert "</mnemosyne>\n")
	  (write-file (format "mnemClozes%s.xml" (format-time-string "%Y%m%d%H%M%S")))
					;(write-file "mnemClozes.xml")
	  )
	))))

(defun count-regexp-in-buffer (re)
  "Counts the number of times a specified regexp shows up in the buffer."
  (interactive "sSearch string: ")
  (save-excursion
    (let ((n 0))
      (goto-char (point-min))
      (while (search-forward-regexp re (point-max) t)
	(setq n (+ n 1)))
      (message (format "%s found %d times." re n)))))

(defun st-beamer-next-frame ()
  "Moves point to the next frame."
  (interactive)
  (search-forward-regexp "begin{frame}"))

(defun st-beamer-previous-frame ()
  "Moves point to the previous frame."
  (interactive)
  (search-backward-regexp "begin{frame}"))

(defun st-beamer-highlight-frame-titles-dispatcher (change-start change-end change-len)
  "Dispatcher for highligting frames in beamer."
  (st-beamer-highlight-frame-titles))

(defun st-beamer-highlight-frame-titles-intialize ()
  "Initializes the frame title highlighting."
  (interactive)
  (if (boundp 'after-change-functions)
      (setq after-change-functions (push 'st-beamer-highlight-frame-titles-dispatcher after-change-functions))
    (setq after-change-functions (list 'st-beamer-highlight-frame-titles-dispatcher)))
  (st-beamer-highlight-frame-titles))

(defun st-beamer-highlight-frame-titles ()
  "Highlights frame titles in beamer. Dispatcher function should be added to post-change-functions to highlight interactively."
  (interactive)
  (when (string= major-mode "latex-mode")
    (when (boundp 'st-beamer-frame-title-overlays)
      (dolist (item st-beamer-frame-title-overlays)
	(when (overlayp item)
	  (delete-overlay item))))
    (setq st-beamer-frame-title-overlays nil)
    (save-excursion
      ;(message "Looking for titles...")
      (goto-char (point-min))
      (let ((frame-titles nil) (new-overlay nil))
	(while (search-forward-regexp "begin{frame}{\\(.*\\)}" (point-max) t)
	  (let ((new-overlay nil))
;	    (message "title: %s (%d - %d)" (match-string-no-properties 1) (match-beginning 1) (match-end 1))
	    (setq new-overlay  (make-overlay (match-beginning 1) (match-end 1)))
	    (overlay-put new-overlay'face  '(:background "yellow" :foreground "black" :underline nil)) 
	    (setq st-beamer-frame-title-overlays (push new-overlay st-beamer-frame-title-overlays))
	    )))
;      (message "Done looking.")
      )))

(defun st-switch-buffer (buffer-name)
  "Switches to the specified buffer if it exists. If already there switches back."
  (interactive)
  (if (string= (buffer-name) buffer-name)
      (switch-to-buffer (other-buffer))
    (if (get-buffer buffer-name)
	(switch-to-buffer (get-buffer buffer-name))
      (message (concat (concat "No " buffer-name) " buffer found.")))))

(defun st-R ()
  "Switches to the *R* buffer if it exists, or back if already there."
  (interactive)
  (st-switch-buffer "*R*"))

(defun st-python ()
  "Switches to the *Python* buffer if it exists, or back if already there."
  (interactive)
  (st-switch-buffer "*Python*"))

(defun st-julia ()
  "Switches to the *Julia* buffer if it exists, or back if already there."
  (interactive)
  (st-switch-buffer "*julia*"))

(defun st-matlab ()
  "Switches to the MATLAB buffer if it exists. If already in the matlab buffer switches to the previous buffer."
  (interactive)
  (if (string= (buffer-name) "*MATLAB*")
      (switch-to-buffer (other-buffer))
    (if (get-buffer "*MATLAB*")
	(switch-to-buffer (get-buffer "*MATLAB*"))
      (message "No MATLAB buffer found."))))

(defun st-octave ()
  "Switches to the OCTAVE buffer if it exists. If already in the matlab buffer switches to the previous buffer."
  (interactive)
  (if (string= (buffer-name) "*Inferior Octave*")
      (switch-to-buffer (other-buffer))
    (if (get-buffer "*Inferior Octave*")
	(switch-to-buffer (get-buffer "*Inferior Octave*"))
      (message "No Inferior Octave buffer found."))))

;; A couple of functions that git mode doesn't have.
(defun git-push ()
  "Makes a system call to git push."
  (interactive)
  (shell-command "git push"))

(defun git-pull ()
  "Makes a system call to git pull."
  (interactive)
  (shell-command "git pull"))

(boundp 'after-change-functions)
