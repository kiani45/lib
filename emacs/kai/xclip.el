;; check ../xclip.el
;;
;; NOTE:
;;     - always use 'clipboard' as src & dst
;;       (ignore 'primary', 'secondary')
;;
;;     - xsel has buffer limit problem, use xclip instead
;;
;;     - if you have some problem with xsel, "sudo rm ~/.xsel.log" may help
;;       sometimes you need to check whether "~/.xsel.log" is writtable or not
;;     - "xsel: Can't open display" problem is resolved by update-DISPLAY()

(defvar xclip-copy-paste-mode)

(defun update-DISPLAY ()
  "Set environment variable 'DISPLAY' from tmux env-var-list"
  (interactive)
  (when (getenv "TMUX") ;; we are under tmux session
    (setenv "DISPLAY" (or (substring (shell-command-to-string "tmux showenv | grep DISPLAY") 8 -1) ":0" ))))

(defun xclip-select (type data)
  "TYPE is a symbol: primary, secondary and clipboard. See `x-set-selection'."
  (let* ((process-connection-type nil)
         (proc (start-process "proc" nil "xclip" "-selection" (symbol-name type))))
    ;; non-nil result means error happended
    ;; this is bad predicate: (NEED TO BE FIXED)
    ;;     'proc' is async. we cannot gaurantee process will terminate in 5 milliseconds
    (when (accept-process-output proc 0 5)
      ; (warn "!!!")
      (update-DISPLAY) ;; update DISPLAY then retry
      (setq proc (start-process "proc" nil "xclip" "-selection" (symbol-name type))))
    (process-send-string proc data)
    (process-send-eof proc)))

(defun xclip-cut-function (text &optional push)
  (xclip-select 'clipboard text))

(defun xclip-paste-function()
  "paste with xsel, only calls update-DISPLAY on error"
  (let ((xsel-output (shell-command-to-string "xclip -o -selection clipboard"))) ;; copy from clipboard
    (when (string-match-p "^Error: Can't open display" xsel-output)
      (update-DISPLAY)
      (setq xsel-output (shell-command-to-string "xclip -o -selection clipboard")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output)))

(defun xclip-copy-paste-enable ()
  "Enable xclip-copy-paste-mode."
   (if window-system
       (message "Under window system, no need to enable")
     (message "Enable xclip-copy-paste-mode")
     (setq xclip-copy-paste-mode t)
     (setq x-select-enable-primary   nil)
     (setq x-select-enable-clipboard nil)
     (setq interprogram-cut-function   'xclip-cut-function)
     (setq interprogram-paste-function 'xclip-paste-function)))

(defun xclip-copy-paste-disable ()
  "Disable xclip-copy-paste-mode"
  (message "Disable xclip-copy-paste-mode")
  (setq xclip-copy-paste-mode nil)
  (setq x-select-enable-primary   nil)
  (setq x-select-enable-clipboard t)
  (setq interprogram-cut-function   'x-select-text)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

(defun xclip-copy-paste-toggle ()
  "Toggle xclip-copy-paste mode. You need to sudo apt-get install xclip.
Use ssh -Y when running ssh. 
DISPLAY problem can be fixed by calling update-DISPLAY()."
  (interactive)
  (unless window-system
    (if xclip-copy-paste-mode
        (xclip-copy-paste-disable)
      (xclip-copy-paste-enable))))

(provide 'xclip)
