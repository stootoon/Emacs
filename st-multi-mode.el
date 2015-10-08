; Functions for fontifying using multiple modes in a single buffer.
; The idea is simple:
; 1. A list specifies the modes to be used in addition to the basic one, and their corresponding regexps
; 2. The function cycles through the modes and saves their font-lock-defaults etc. variables.
; 3. It then chunks up the file into the differen modes
; 4. For each non-base mode, fontifys the corresponding regions by using a modified version of fontify-region which takes the font-lock defaults as an argument.
; 5. Repeats 3-4 at each insertion.
;
; We additionally disable font-lock-mode in the buffer and font-lock manually after every _change_ rather than every command

(defcustom st-multi-mode-other-modes-alist nil
"An alist of mode names and open and close regexp
patternsdelimiting the chunks in the other modes.  For
example (list (cons 'latex-mode' (list (cons '@{' '@}')))) would
indicate that latex-mode should be used between the regular
expressions @{ and @}.")
(make-variable-buffer-local 'st-multi-mode-other-modes-alist)

(defvar st-multi-mode-other-modes-font-lock-data-alist nil 
  "An alist containing the font-lock-defaults and
font-lock-keywords for each of the other modes defined in
st-multi-mode-other-modes-alist.")
(make-variable-buffer-local 'st-multi-mode-get-font-lock-data-alist)

(defvar st-multi-mode-chunks-alist nil 
  "An alist containing the chunks of the current buffer in each
  of the other modes.")
(make-variable-buffer-local 'st-multi-mode-chunks-alist)

(defun st-multi-mode-get-font-lock-data-alist ()
  "Goes through each of the other modes and loads
st-multi-mode-other-modes-font-lock-data with the
font-lock-defaults and font-lock-keywords for each of the other
modes."
  (interactive)
  (let ((main-mode major-mode))
    (setq st-multi-mode-other-modes-font-lock-data-alist (list))
    ;; Go through each of the other modes and grab their defaults
    (dolist (item st-multi-mode-other-modes-alist)
      (let ((this-mode-name (first item)) new-item)
	;; Have to do change the mode in a separate buffer because
	;; otherwise buffer local variables are removed
	(with-current-buffer (get-buffer-create "*st-multi-mode-temp*") 
	  (funcall (intern this-mode-name)) ; set the mode - this erase
	  (setq new-item (cons this-mode-name (cons font-lock-defaults font-lock-keywords))))
	(kill-buffer "*st-multi-mode-temp*")	
	(if st-multi-mode-other-modes-font-lock-data-alist
	    (setq st-multi-mode-other-modes-font-lock-data-alist
		  (append (list new-item) st-multi-mode-other-modes-font-lock-data-alist))
	  (setq st-multi-mode-other-modes-font-lock-data-alist (list new-item)))
	); let
      ); dolist
    ); let
)
 
;; chunkify file according to the modes
(defun st-multi-mode-chunkify-buffer ()
  "Goes through the buffer and finds the chunks for each of the other modes."
  (interactive)
  (setq st-multi-mode-chunks-alist (list))
  (save-restriction
    (widen)
    (save-excursion
      (dolist (item st-multi-mode-other-modes-alist)
	(let ((mode-name (first item)) (tags (rest item)) (chunks (list)) start-point end-point resume-point)
	  (dolist (tag-pair tags)
	    (let ((open-tag (car tag-pair)) (close-tag (cdr tag-pair)) (loop-count 0))
	      (goto-char 0)
	      (while (and (< (point) (point-max)) (< loop-count 100))
		(setq loop-count (+ loop-count 1))
		(if (not start-point) ; start-point is not set
		    ;; Look for the start point. 
		    ;; If it finds it, it will set start point at the point right after the pattern
		    ;; If it doesn't find it, it will set point to point-max, at which point the while should exit.
		    (setq start-point (re-search-forward open-tag nil 2))
		  (progn
		    ;; Look for the end-point
		    ;; If it finds it, it will set end-point at the point right after the pattern
		    ;; If it doesn't find it, it will set point to point-max, at which point the while should exit.
		    (setq end-point (re-search-forward close-tag nil 2)) 
		    (when end-point 
		      (setq resume-point end-point)
		      (setq end-point (- (re-search-backward close-tag) 1))
		      (let ((new-chunk (cons start-point end-point)))
			(if chunks
			    (setq chunks (append (list new-chunk) chunks))
			  (setq chunks (list new-chunk))))
		      ;; Reset the start and end-points
		      (setq end-point nil)
		      (setq start-point nil)
		      (goto-char resume-point)
		      (setq loop-count 0)
		      ); when end-point
		    );progn
		  ) ;if
		) ; while
	      (when (>= loop-count 100)
		(message "Infinite loop detected in st-multi-mode-chunkify-buffer."))
	      ); let open-tag
	    );dolist tag-pair
	  (when chunks
	    (if st-multi-mode-chunks-alist
		(setq st-multi-mode-chunks-alist (append (list (cons mode-name chunks)) st-multi-mode-chunks-alist))
	      (setq st-multi-mode-chunks-alist (list (cons mode-name chunks))))
	    );when
	); let mode-name
      ); do list item
    ); save restriction
  ); save excurison
)

;; Now loop through the chunks for each mode and fontify the region
(defun st-multi-mode-fontify-buffer ()
  "Fontifies the current buffer."
  (interactive)
  (font-lock-fontify-buffer) ; font-lock is off so we have to do this manually
  (save-restriction
    (widen)
    (save-excursion
      (let ((current-font-lock-defaults font-lock-defaults) (current-font-lock-keywords font-lock-keywords))
	(dolist (item st-multi-mode-other-modes-font-lock-data-alist)
	  (let* ( (key (car item)) (fl-defaults (car (cdr item))) (fl-keywords (cdr (cdr item))) (chunks (cdr (assoc key st-multi-mode-chunks-alist))))
	    (setq font-lock-defaults fl-defaults)
	    (setq font-lock-keywords fl-keywords)
	    (font-lock-set-defaults)
	    (dolist (chunk chunks)
	      (let ((region-start (car chunk)) (region-end (cdr chunk)))
		(font-lock-default-fontify-region region-start region-end nil)))))
	;; Now reset the values to their originals
	(setq font-lock-defaults current-font-lock-defaults)
	(setq font-lock-keywords current-font-lock-keywords)
	(font-lock-set-defaults)))))

(defun st-multi-mode-initialize ()
  "Intializes the st-multi-mode in the buffer."
  (interactive)
  (st-multi-mode-get-font-lock-data-alist)
  (font-lock-mode -1) ; disable font-lock-mode
  (remove-hook 'after-save-hook 'st-multi-mode-post-command-function)
  (add-hook 'after-save-hook    'st-multi-mode-post-command-function)
  )

(defun st-multi-mode-post-command-function ()
  "Chunks and fontifies the current buffer. For addition to the post-command hook."
  (interactive)
  (when st-multi-mode-other-modes-alist
    (st-multi-mode-chunkify-buffer)
    (st-multi-mode-fontify-buffer)))

(provide 'st-multi-mode)