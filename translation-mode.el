(define-derived-mode translation-mode text-mode "Translation"
  "Major mode for translating text."
  (make-local-variable 'text-mode-variant)
  (setq text-mode-variant t))
(setq translation-mode-hook '(translation-mode-initialize))

(defcustom translation-mode-hook nil
  "Hook run when entering translation mode."
  :type 'hook
  :options '(translation-mode-initialize)
  :group 'data)

(defvar translation-mode-source-file nil "Translation mode source file.")
(defvar translation-mode-translation-file nil "Translation mode translation fine.")
(defvar translation-mode-vocabulary-file nil "Translation mode vocabular file.")
(defvar translation-mode-data-file nil "Translation mode data file.")
(defvar translation-mode-hash-table nil "Translation mode hash table. Is filled with the translated clips.")
(defvar translation-mode-hash-indices nil "Translation mode hash indices. Contains the start points of the translate phrases.")

(defun load-file-into-buffer (file-name)
  "Load FILE-NAME into a buffer of the same name and returns the buffer."
  (find-file file-name)
  (get-buffer file-name))

(defun translation-mode-initialize ()
  "Initializes translation of the current buffer."
  (setq file-name (buffer-file-name (current-buffer)))
  (setq translation-mode-source-file file-name)
  (setq translation-mode-translation-file (concat file-name ".tra.el"))
  (setq translation-mode-vocabulary-file (concat file-name ".voc"))
  (setq translation-mode-data-file (concat file-name ".tra.dat"))
  (setq translation-mode-translation-hash-table nil)
  (setq translation-mode-hash-indices nil)
  (setq translation-mode-translation-data-buffer (load-file-into-buffer translation-mode-translation-file))
  (message "translation-mode-data-buffer: %s" translation-mode-translation-data-buffer)
  (message "translation-mode-data-buffer: %s" translation-mode-translation-data-buffer)
  (parse-translation-data translation-mode-translation-data-buffer)
  (find-file-read-only file-name)
  )
  
(defun parse-translation-data (buf)
  "Parses the translation data in BUF and loads it into a hash table indexed by the starting point of each translated phrase."
  (setq translation-mode-hash-data (make-hash-table))
  (setq translation-mode-hash-indices nil)
  (with-current-buffer buf
    (goto-char (point-min))
    (eval-buffer buf) ; loads the variable hash-data
    (while hash-data
      (setq value (car hash-data))
      (setq key (car value))
      (puthash key value translation-mode-hash-data)
      (setq translation-mode-hash-indices 
	    (append translation-mode-hash-indices (list key)))
      (setq hash-data (cdr hash-data))))
  translation-mode-hash-data)
    
;(setq buf (get-buffer "kapital.txt.tra.el"))
;(setq hash-table (parse-translation-data buf))
;(message "%s" (gethash 1 translation-mode-hash-data))
;(message "%s" translation-mode-hash-indices)
;(car 'parse-translation-data)
;translation-mode-hash-indices
;(append nil 'a)
;(kill-all-local-variables)