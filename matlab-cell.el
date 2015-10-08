(defvar matlab-cell-start-re "^%% .*" 
  "Regular expression that defines that start of a cell block.")

(defvar matlab-cell-background (cons 'background-color "#36648B")
  "Background color of an active cell.")

(defvar matlab-cell-overlay nil
  "The overlay variable containing the overlay.")

(defvar matlab-cell-overlay-buffer nil
  "The buffer associated with the overlay.")

(defun matlab-cell-create-overlay ()
  "Creates an overlay and sets the cell-overlay variable to it."
  (when matlab-cell-overlay
      (delete-overlay matlab-cell-overlay))
  (prog1 
      (setq matlab-cell-overlay (make-overlay 1 1))
    (setq matlab-cell-overlay-buffer (current-buffer))
    (overlay-put matlab-cell-overlay 'face matlab-cell-background)))
  
(defun matlab-cell-end (&optional buffer)
  "Returns the point at the start of the next cell."
  (interactive)
  (save-excursion
    (unless buffer
      (setq buffer (current-buffer)))
    (set-buffer buffer)
    (if (re-search-forward matlab-cell-start-re nil t)
	(match-beginning 0)
      (point-max))))

(defun matlab-cell-start (&optional buffer)
  "Returns the point starting the current cell."
  (interactive)
  (save-excursion
    (unless buffer
      (setq buffer (current-buffer)))
    (set-buffer buffer)
    (if (re-search-backward matlab-cell-start-re nil t)
	(match-beginning 0)
      (point-min))))

(defun matlab-cell-update ()
  "Updates the cell in the current buffer."
  (when (string= major-mode "matlab-mode")
    (unless matlab-cell-overlay
      (matlab-cell-create-overlay))
    (progn
      (unless (eq matlab-cell-overlay-buffer (current-buffer))
	(matlab-cell-create-overlay))
      (setq cs (matlab-cell-start matlab-cell-overlay-buffer))
      (setq ce (matlab-cell-end   matlab-cell-overlay-buffer))
      (move-overlay matlab-cell-overlay cs ce))))

(defun matlab-cell-add-post-command-hook ()
  "Adds matlab-cell-update-all to the post-command hooks."
  (add-hook 'post-command-hook 'matlab-cell-update))

(defun matlab-cell-initialize ()
  (matlab-cell-create-overlay)
  (matlab-cell-add-post-command-hook))

(defun matlab-cell-run-current-cell ()
  ""
  (interactive)
  (matlab-shell-run-region (matlab-cell-start) (matlab-cell-end)))
