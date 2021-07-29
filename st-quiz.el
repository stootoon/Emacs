(defun st-count-matches-on-line (pat)
  (interactive)
  (save-restriction
    (end-of-line)
    (setq end (point))
    (beginning-of-line)
    (setq beg (point))
    (narrow-to-region beg end)
    (when
	(re-search-forward pat nil t)
      (progn
	(message "Found at least one match: '%s'" (match-string-no-properties 0))
	;; Count the matches
	(setq n-matches 1)
	(while (re-search-forward pat nil t)
	  (progn
	    (message "Found another match : '%s'" (match-string-no-properties 0))
	    (setq n-matches (+ n-matches 1))))
	(message "Found %d matches." n-matches))))
  n-matches)

(defun narrow-to-line ()
  (end-of-line)
  (setq end (point))
  (beginning-of-line)
  (setq beg (point))
  (narrow-to-region beg end))

(dolist
    (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*" "*Quail Completions*"))
  (when (get-buffer buf)
    (with-current-buffer buf
      (setq-local face-remapping-alist '((default (:height 1)))))))

(defun quiz (pat)
  (interactive)
  (setq num-lines (count-lines (point-min) (point-max)))
  (setq max-attempts 10)
  (save-excursion
    (let ((quit nil))
      (while (not quit)
	(progn
	  (let ((found-match nil)
		(attempts 0))
	    (while (and (not found-match)
			(< attempts max-attempts))
	      (setq attempts (+ attempts 1))
	      (let ((random-line (random num-lines))) ; Pick a random line
		(message "Going to line %d/%d" random-line num-lines)
		(goto-line random-line)
		(let ((n-matches (st-count-matches-on-line pat))) ; count the number of matches on the line
		  (when n-matches
		    (setq which-match (+ 1 (random n-matches))) ; pick a random match
		    (message "Choosing match %d/%d" which-match n-matches)
		    (save-restriction
		      (narrow-to-line)
		      (re-search-forward pat nil t which-match))
		    (setq full-matched (match-string-no-properties 0))
		    (save-match-data
		      (setq matched (string-trim (match-string-no-properties 1)))) ; trim the whitespace
		    (setq found-match (and
				       (not (string= (upcase matched) matched))
				       t))
		    ); when n-matches
		  ) ; let n-matches
		); let random-line
	      ) ;while not found match
	    
	    (when (>= attempts max-attempts)
	      (progn
		(message "Didn't find any regexp matches after %d attempts." attempts)
		(return 1)))
	    ) ; let found match
	
	  (message "Found match at %d - %d: '%s'" (match-beginning 0) (match-end 0) full-matched)	  	  
	  (save-match-data
	    (setq starred-string (replace-regexp-in-string
				  "[^|\s-]" "_" full-matched)))
	  (replace-match starred-string)
	  (message "Match after replace-match: %d - %d: '%s'" (match-beginning 0) (match-end 0) full-matched)	  	        
	  (unwind-protect
	      (save-match-data
		(setq answer (read-string "Fill in the blank: ")))
	    (progn
	      (message "Running unwind-protect.")
	      (message "Match data = %d - %d: '%s'" (match-beginning 0) (match-end 0) full-matched)	  	  
	      (replace-match full-matched)
	      ))
	  (message "Answer = %s (%d)" answer (length answer))
	  (if (> (length answer) 0)
	      (if (string= answer matched)
		  (progn
		    (message "'%s' = '%s' ✓" matched answer)
		    (sleep-for 1)
		    )
		(progn
		  (message "%s ≠ %s ✗" matched answer)
		  (read-char)
		  )
		) ; if match answer
	    (progn
	      (message "Quitting")
	      (setq quit t))
	    )
	  ) ;progn
	); while not quit
      );  let quit nil
    ); save-excursion
  )

(setq alpha-pat "|\s-*\\([^|]+\\)\s-*")
(setq greek-pat "|\s-*\\(\\cg+\\)\s-*")
(defun greek-quiz ()
  (interactive)
  (quiz greek-pat))
