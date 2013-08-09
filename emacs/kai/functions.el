
;; ##################################
;; ##    MISCELLANOUS FUNCTIONS    ##
;; ##################################

(defun kill-other-buffers ()
  "Kill all other buffers except current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun major-mode ()
  "Get major mode of current buffer"
  (interactive)
  (message "%s" major-mode))

(defun toggle-comment-on-line ()
  "comment or uncomment after 'copy' current line"
  (interactive)
  ;; (copy-whole-line)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun select-next-window ()
  "Switch to the next window" 
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window" 
  (interactive)
  (select-window (previous-window)))

(defun add-to-multiple-hooks (hooks function)
  (mapc (lambda (hook) 
          (add-hook hook function))
        hooks))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "srename as: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1) (rename-buffer new-name) (set-visited-file-name new-name) (set-buffer-modified-p nil))))))

(defun move-file-and-buffer (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "Dmove file to dir: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname)	(set-buffer-modified-p nil) t))))

(defun goto-percent (pct)
  (interactive "nGoto percent: ")
  (goto-char (/ (* (point-max) pct) 100)))

(defun show-mode ()
  (interactive)
  (message "major mode: %s, mode name: %s, buffer: %s" major-mode mode-name (buffer-name)))

(defun reload-emacs-cfgfile ()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs") )

(defun quit-current-window ()
  (interactive)
  (other-window 1)
  (delete-other-windows))

(defun insert-header (title)
  "insert banner with optional 'title' parameter"
  (interactive "sTitle: ")
  (let ((len (length title))
        (totalLen (+ (length title) 2 4 2 4)))
    (if (> len 0)
        (progn (insert (format "%s\n" (make-string totalLen 35)))
               (insert (format "##    %s    ##\n" title))
               (insert (format "%s\n" (make-string totalLen 35))))
      (insert (make-string 25 35) "\n"))))

(defun p4-edit-current-buffer ()
  (interactive)
  (shell-command (concat "p4 edit " (remove-angle-bracket (buffer-name))))
  (revert-buffer nil t))

(defun p4-revert-current-buffer ()
  (interactive)
  (shell-command (concat "p4 revert " (remove-angle-bracket (buffer-name))))
  (revert-buffer nil t))

(defun p4-add-current-buffer ()
  (interactive)
  (shell-command (concat "p4 add " (remove-angle-bracket (buffer-name))))
  (revert-buffer nil t))

(defun backup-current-buffer ()
  "make a copy of current buffer with .bak subfix"
  (interactive)
  (let ((filename (if (string= major-mode "bookmark-bmenu-mode")
                      bmkp-current-bookmark-file
                    (remove-angle-bracket (buffer-name)))))
    (shell-command (concat "cp " filename " " (concat filename ".bak")))))

(defun backup-current-buffer-r ()
  "reversed backup-current-buffer: revert current buffer with .bak backup file"
  (interactive)
  (let ((filename (if (string= major-mode "bookmark-bmenu-mode")
                      bmkp-current-bookmark-file
                    (remove-angle-bracket (buffer-name)))))
    (shell-command (concat "cp " (concat filename ".bak") " " filename))
    (revert-buffer nil t)))



;; ############################
;; ##    WORD/STRING COPY    ##
;; ############################

;;
;; copy word
;;
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg (if (= arg 1) nil (funcall pasteMe))
      (funcall pasteMe))))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
       (copy-thing 'backward-word 'forward-word arg))

;; (defun copy-buffer-name (&optional arg)
;;   "Copy buffer-name at point into kill-ring"
;;   (interactive "P")
;;        (kill-new (buffer-name)))

;;
;; copy string
;;
(setq my-str-regex-matching "[\][ \t{}():;!@#\$%\^\\&\*\/\\,\.\?\"'+=<|>]") 

(defun beginning-of-string (&optional arg)
  "find the beginning-of-string and set 'point' to it"
  (re-search-backward my-str-regex-matching (line-beginning-position) 3 1)
  (if (looking-at my-str-regex-matching)  (goto-char (+ (point) 1)) ))

(defun end-of-string (&optional arg)
  "find the end-of-string and set 'point' to it"
  (re-search-forward my-str-regex-matching (line-end-position) 3 arg)
  (if (looking-back my-str-regex-matching) (goto-char (- (point) 1)) ))

(defun copy-string (&optional arg)
  " Try to copy a string and paste it to the mark
     When used in shell-mode, it will paste string on shell prompt by default "
  (interactive "P")
  (copy-thing 'beginning-of-string 'end-of-string arg))

(defun copy-whole-line (&optional arg)
  (interactive "P")
  (copy-region-as-kill (line-beginning-position) (line-end-position arg)))

;; ################
;; ##    TAGS    ##
;; ################

(defun build-index-all()
  "run my customized script to generate CSCOPE files, BROWSE, and TAGS,  under either current dir or $WROKDIR/datapower"
  (interactive)
  (build-index-cscope) ;; these functions are defined bellow...
  ;(build-index-etags)
  (build-index-ebrowse))

(defun build-index-cscope ()
  "run my customized script to generate cscope-files under either current dir or $WROKDIR/datapower"
  (interactive)
  (shell-command "build-index-cscope"))

(defun build-index-ebrowse ()
  "run my customized script to generate BROWSE file under either current dir or $WROKDIR/datapower"
  (interactive)
  (shell-command "build-index-ebrowse"))

(defun my-compile ()
  "issue 'compile' command and goto that window"
  (interactive)
  (call-interactively 'compile)
  (switch-to-buffer-other-frame "*compilation*")
  (delete-other-windows)
)

(defun my-kill-compilation ()
  (interactive)
  (kill-compilation)
  ;(quit-window)
)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; ================================================

(provide 'functions)

